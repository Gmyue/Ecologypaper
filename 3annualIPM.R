# =========================================================
# 0. 环境
# =========================================================
# install.packages(c("readxl","dplyr","tidyr","brms","purrr",
#                    "tibble","matrixStats","Matrix","ggplot2","this.path"))
library(readxl)
library(dplyr)
library(tidyr)
library(brms)
library(purrr)
library(tibble)
library(matrixStats)
library(Matrix)
library(ggplot2)
library(this.path)

setwd(this.path::this.dir())
set.seed(123)

# 快速检查临时目录是否可写（Stan 编译需要）
if (!dir.exists(tempdir())) dir.create(tempdir(), recursive = TRUE)
if (!file.access(tempdir(), 2) == 0) {
  dir.create("C:/Rtemp", showWarnings = FALSE)
  Sys.setenv(TMP = "C:/Rtemp", TMPDIR = "C:/Rtemp")
}

# =========================================================
# 1. 工具函数
# =========================================================
safe_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(numeric(0))
  if (is.numeric(x)) return(x)
  x <- sub("^=", "", as.character(x))
  x <- gsub("^[xX×]$|^NA$|^N$|^\\s*$", NA, x)
  x <- gsub("[^0-9eE.+-]", "", x)
  suppressWarnings(as.numeric(x))
}

safe_int <- function(x) {
  xn <- safe_num(x)
  if (length(xn) == 0) return(integer(0))
  as.integer(round(xn))
}

clean_treat <- function(df) {
  df %>%
    mutate(
      Drought       = as.integer(Drought       %in% c("drought","Drought","1",1)),
      Fertilization = as.integer(Fertilization %in% c("High","high","1",1)),
      HM            = as.integer(HM            %in% c("HM","hm","1",1))
    )
}

# 容错读取工作表
read_sheet <- function(file, sheet) {
  sheets <- excel_sheets(file)
  if (is.numeric(sheet)) {
    return(read_excel(file, sheet = sheet, .name_repair = "unique"))
  }
  if (!sheet %in% sheets) {
    idx <- grep(sheet, sheets, fixed = TRUE)
    if (length(idx) == 0) idx <- grep(sheet, sheets)
    if (length(idx) == 0) {
      stop("找不到工作表 '", sheet, "'。可用: ", paste(sheets, collapse = " | "))
    }
    sheet <- sheets[idx[1]]
  }
  read_excel(file, sheet = sheet, .name_repair = "unique")
}

# =========================================================
# 2. 读取原始数据
# =========================================================
file_23 <- "./数据/野外实验23年数据.xlsx"
file_24 <- "./数据/野外实验24年数据.xlsx"
file_25 <- "./数据/野外实验25年数据.xlsx"

init_23 <- read_sheet(file_23, 1)
end_23  <- read_sheet(file_23, 2)

init_24 <- read_sheet(file_24, 1)
end_24  <- read_sheet(file_24, 2)


init_25 <- read_sheet(file_25, 1)
end_25  <- read_sheet(file_25, 2)

# =========================================================
# 3. 补 2024 / 2025 年终的处理信息
# =========================================================
# 2024 年终本身有处理列，但若缺失则用 Sheet3 补
if (!all(c("Drought","Fertilization","HM") %in% names(end_24))) {
  treat_24 <- sheet3_24 %>%
    select(Block, Cage, Drought, Fertilization, HM) %>%
    distinct(Block, Cage, .keep_all = TRUE) %>%
    clean_treat()
  end_24 <- end_24 %>%
    select(-any_of(c("Drought","Fertilization","HM"))) %>%
    left_join(treat_24, by = c("Block","Cage"))
} else {
  end_24 <- clean_treat(end_24)
}

# 2025 年终没有处理列，从 init_25 补
treat_25 <- init_25 %>%
  select(Block, Cage, Drought, Fertilization, HM) %>%
  distinct(Block, Cage, .keep_all = TRUE) %>%
  clean_treat()
end_25 <- end_25 %>%
  select(-any_of(c("Drought","Fertilization","HM"))) %>%
  left_join(treat_25, by = c("Block","Cage"))

# 2023 年终有处理列
end_23 <- clean_treat(end_23)

# =========================================================
# 4. 清理初始数据
# =========================================================
clean_init <- function(df, year) {
  df <- as.data.frame(df)
  if (!"Subplot" %in% names(df)) df$Subplot <- 1
  
  for (i in 0:4) {
    cc <- paste0("Stage_", i)
    if (!cc %in% names(df)) df[[cc]] <- 0
    df[[cc]] <- safe_num(df[[cc]])
  }
  
  # 面积：2024 有 T_Total 可反推
  if ("T_Total" %in% names(df) && "Total" %in% names(df)) {
    df$area <- safe_num(df$Total) / safe_num(df$T_Total)
  } else {
    df$area <- 1
  }
  
  df$Year <- year
  df <- clean_treat(df)
  
  df %>%
    group_by(Year, Block, Cage, Drought, Fertilization, HM) %>%
    summarise(
      Stage_0 = sum(Stage_0, na.rm = TRUE),
      Stage_1 = sum(Stage_1, na.rm = TRUE),
      Stage_2 = sum(Stage_2, na.rm = TRUE),
      Stage_3 = sum(Stage_3, na.rm = TRUE),
      Stage_4 = sum(Stage_4, na.rm = TRUE),
      N0 = sum(Stage_0 + Stage_1 + Stage_2 + Stage_3 + Stage_4, na.rm = TRUE),
      area = sum(area, na.rm = TRUE),
      .groups = "drop"
    )
}

init_all <- bind_rows(
  clean_init(init_23, 2023),
  clean_init(init_24, 2024),
  clean_init(init_25, 2025)
)

# 2025 的 Stage 是小数（密度换算），先取整，模型能跑
init_all <- init_all %>%
  mutate(across(c(Stage_0:Stage_4, N0), safe_int))

# =============================
#这是一段git修改记录测试文字
# =============================


# =============================
#这是一段git修改记录测试文字
# =============================



# =============================
#这是一段git修改记录测试文字
# =============================


# =============================
#这是一段git修改记录测试文字
# =============================
# =========================================================
# 5. 清理年终数据
# =========================================================
clean_end_all <- function(df, year) {
  df <- as.data.frame(df)
  
  # 冠幅列：模糊匹配 space，并删除原始列，避免 bind 类型冲突
  space_cols <- grep("space", names(df), ignore.case = TRUE, value = TRUE)
  if (length(space_cols) > 0) {
    df$Space <- safe_num(df[[space_cols[1]]])
    df[[space_cols[1]]] <- NULL
  } else {
    df$Space <- NA_real_
  }
  
  df$Height <- safe_num(df$Height)
  if (!"Count" %in% names(df)) df$Count <- NA_real_
  df$Count <- safe_num(df$Count)
  
  if (!all(c("Drought","Fertilization","HM") %in% names(df))) {
    stop("Year ", year, " 年终缺少处理列")
  }
  
  df$Year <- year
  df <- clean_treat(df)
  df <- df[!is.na(df$Height), ]
  
  keep_cols <- c("Year","Block","Cage","Drought","Fertilization","HM",
                 "Height","Space","Count")
  keep_cols <- keep_cols[keep_cols %in% names(df)]
  df[, keep_cols, drop = FALSE]
}

h_dat <- bind_rows(
  clean_end_all(end_23, 2023),
  clean_end_all(end_24, 2024),
  clean_end_all(end_25, 2025)
) %>%
  mutate(logN0 = log(ifelse(is.na(Count), 1, Count) + 1))

# 按笼子聚合出年末汇总
end_all <- h_dat %>%
  group_by(Year, Block, Cage, Drought, Fertilization, HM) %>%
  summarise(
    N_end  = n(),
    mean_H = mean(Height, na.rm = TRUE),
    sd_H   = sd(Height, na.rm = TRUE),
    mean_W = mean(Space, na.rm = TRUE),
    sd_W   = sd(Space, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# 6. 构造年际数据
# =========================================================
# 年末 t -> 年初 t+1
rec_dat <- end_all %>%
  inner_join(
    init_all %>%
      mutate(Year = Year - 1) %>%
      select(Year, Block, Cage,
             N0_next = N0,
             Stage_0, Stage_1, Stage_2, Stage_3, Stage_4),
    by = c("Year","Block","Cage")
  )

# 年初 t -> 年末 t
end_dat <- init_all %>%
  inner_join(
    end_all %>% select(Year, Block, Cage, N_end, mean_H),
    by = c("Year","Block","Cage")
  ) %>%
  mutate(
    logN0   = log(N0 + 1),
    logArea = log(area + 1e-6)
  )

# 响应整数化
rec_dat <- rec_dat %>%
  mutate(
    N0_next_int = safe_int(N0_next),
    Stage_0 = safe_int(Stage_0),
    Stage_1 = safe_int(Stage_1),
    Stage_2 = safe_int(Stage_2),
    Stage_3 = safe_int(Stage_3),
    Stage_4 = safe_int(Stage_4),
    N_total_stage = Stage_0 + Stage_1 + Stage_2 + Stage_3 + Stage_4,
    log_N_end_p1  = log(N_end + 1)
  ) %>%
  filter(!is.na(N0_next_int), N_total_stage > 0)

# 检查是否还有非整数
rec_dat %>%
  summarise(across(c(Stage_0:Stage_4, N0_next_int, N_end),
                   ~ sum(.x %% 1 != 0, na.rm = TRUE))) %>%
  print()

# =========================================================
# 7. 拟合模型
# =========================================================
# 7.1 年末数量
m_end <- brm(
  bf(N_end ~ Drought + Fertilization + HM + logN0 + logArea +
       (1 | Block) + (1 | Cage)),
  family = negbinomial(),
  data   = end_dat,
  prior  = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# 7.2 年末株高
m_h <- brm(
  bf(Height ~ Drought + Fertilization + HM + logN0 +
       (1 | Block) + (1 | Cage)),
  family = gaussian(),
  data   = h_dat,
  prior  = c(
    prior(normal(0, 2), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sigma")
  ),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# 7.3 补充模型（响应整数）
m_rec <- brm(
  bf(N0_next_int ~ Drought + Fertilization + HM +
       log_N_end_p1 + mean_H +
       (1 | Block) + (1 | Cage)),
  family = negbinomial(),
  data   = rec_dat,
  prior  = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0, 1), class = "b"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# 7.4 叶片分布（multinomial + trials）
#     先验不写，使用 brms 默认；避免 dpar 命名问题
m_leaf <- brm(
  bf(cbind(Stage_0, Stage_1, Stage_2, Stage_3, Stage_4) |
       trials(N_total_stage) ~
       Drought + Fertilization + HM + log_N_end_p1 + mean_H +
       (1 | Block) + (1 | Cage)),
  family = multinomial(),
  data   = rec_dat,
  chains = 4, iter = 4000, warmup = 2000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# 如果 m_leaf 仍报错，换 dirichlet（不要求整数）
# m_leaf <- brm(
#   bf(cbind(S0,S1,S2,S3,S4) ~
#        Drought + Fertilization + HM + log_N_end_p1 + mean_H +
#        (1 | Block) + (1 | Cage)),
#   family = dirichlet(),
#   data = rec_dat %>%
#     mutate(S0 = Stage_0 + 0.5, S1 = Stage_1 + 0.5,
#            S2 = Stage_2 + 0.5, S3 = Stage_3 + 0.5, S4 = Stage_4 + 0.5),
#   chains = 4, iter = 4000, warmup = 2000, cores = 4
# )

# =========================================================
# 8. 构建野外 IPM（可识别简化版）
# =========================================================
height_breaks <- quantile(h_dat$Height, probs = seq(0, 1, length.out = 6),
                          na.rm = TRUE)
height_breaks[1] <- -Inf
height_breaks[6] <- Inf

get_beta <- function(model, term, strict = TRUE) {
  fe <- fixef(model)
  rn <- rownames(fe)
  if (term %in% rn) return(fe[term, 1])
  rn_clean   <- gsub("[^A-Za-z0-9_]", "", rn)
  term_clean <- gsub("[^A-Za-z0-9_]", "", term)
  idx <- grep(term_clean, rn_clean, fixed = TRUE)
  if (length(idx) > 0) return(fe[idx[1], 1])
  if (strict) stop("找不到系数: ", term, "\n可用: ", paste(rn, collapse = ", "))
  0
}

build_IPM_one <- function(post_h, post_rec,
                          treat = c(Drought = 0, Fertilization = 0, HM = 0),
                          N_end  = 50,
                          mean_H = 150,
                          logN0  = median(h_dat$logN0, na.rm = TRUE),
                          Block  = rec_dat$Block[1],
                          Cage   = rec_dat$Cage[1]) {
  
  height_breaks <- quantile(h_dat$Height,
                            probs = seq(0, 1, length.out = 6),
                            na.rm = TRUE)
  height_breaks[1] <- -Inf
  height_breaks[6] <- Inf
  
  # ---- 给 m_h 用的 newdata（必须有 logN0）----
  newdata_h <- data.frame(
    Drought       = treat["Drought"],
    Fertilization = treat["Fertilization"],
    HM            = treat["HM"],
    logN0         = logN0,
    Block         = Block,
    Cage          = Cage
  )
  
  # ---- 给 m_rec 用的 newdata ----
  newdata_rec <- data.frame(
    Drought       = treat["Drought"],
    Fertilization = treat["Fertilization"],
    HM            = treat["HM"],
    log_N_end_p1  = log(N_end + 1),
    mean_H        = mean_H,
    Block         = Block,
    Cage          = Cage
  )
  
  mu_h    <- mean(posterior_epred(post_h,   newdata = newdata_h))
  sigma_h <- posterior_summary(post_h, pars = "sigma")[1, "Estimate"]
  N0_next <- mean(posterior_epred(post_rec, newdata = newdata_rec))
  
  cat("mu_h =", round(mu_h, 1),
      " N0_next =", round(N0_next, 1),
      " N0/N_end =", round(N0_next / N_end, 2), "\n")
  
  p_k <- diff(pnorm(height_breaks, mean = mu_h, sd = sigma_h))
  p_k <- p_k / sum(p_k)
  G <- matrix(rep(p_k, each = 5), nrow = 5, byrow = TRUE)
  
  p_leaf <- rep(1/5, 5)
  F_mat <- matrix(rep(N0_next / max(N_end, 1) * p_leaf, times = 5),
                  nrow = 5, ncol = 5, byrow = FALSE)
  
  F_mat %*% G
}

A_demo <- build_IPM_one(
  post_h = m_h, post_rec = m_rec,
  treat  = c(Drought = 1, Fertilization = 1, HM = 0),
  N_end  = 50, mean_H = 150
)
colSums(A_demo)
eigen(A_demo)$values[1]

# 8 个处理组合
treat_combos <- expand.grid(
  Drought       = c(0, 1),
  Fertilization = c(0, 1),
  HM            = c(0, 1)
)

# 用每个处理组合的均值
treat_means <- rec_dat %>%
  group_by(Drought, Fertilization, HM) %>%
  summarise(
    N_end_mean  = mean(N_end, na.rm = TRUE),
    mean_H_mean = mean(mean_H, na.rm = TRUE),
    .groups = "drop"
  )

lambda_res <- treat_combos %>%
  left_join(treat_means, by = c("Drought","Fertilization","HM")) %>%
  rowwise() %>%
  mutate(
    A = list(build_IPM_one(
      post_h = m_h, post_rec = m_rec,
      treat  = c(Drought = Drought,
                 Fertilization = Fertilization,
                 HM = HM),
      N_end  = N_end_mean,
      mean_H = mean_H_mean
    )),
    lambda = eigen(A[[1]])$values[1]
  ) %>%
  ungroup()

print(lambda_res)

# =========================================================
# 9. 后验预测检查
# =========================================================
pp_end      <- posterior_predict(m_end, newdata = end_dat)
pp_end_mean <- colMeans(pp_end)

plot(end_dat$N_end, pp_end_mean,
     xlab = "Observed N_end", ylab = "Predicted N_end",
     main = "Posterior predictive check: N_end")
abline(0, 1, col = "red")

# 保存模型，避免重复拟合
saveRDS(list(m_end = m_end, m_h = m_h, m_rec = m_rec, m_leaf = m_leaf),
        file = "brms_models.rds")

