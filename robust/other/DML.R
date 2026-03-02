install.packages(c("fixest","DoubleML","mlr3","mlr3learners","data.table"))
install.packages(c("xgboost","ranger"))
install.packages("ranger")
install.packages("xgboost")

library(fixest)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(data.table)
library(haven)
library(haven)
library(fixest)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(xgboost)

# ----------------------------
# 1️⃣ 读取数据
# ----------------------------
df <- read_dta("/Users/libingchen/Desktop/NTUP_BL/robust/other/data.dta")

# 只用 year >= 2007
df <- subset(df, year >= 2007)

# ----------------------------
# 2️⃣ 双固定效应残差化
# ----------------------------

# outcome
df$Y_resid  <- resid(feols(EMP ~ 1 | city + year, data = df))

# treatment（只保留 did）
df$D_resid <- resid(feols(did ~ 1 | city + year, data = df))

# ----------------------------
# 3️⃣ 控制变量（请确认名字是否存在）
# ----------------------------
X_vars <- c("econ1","gov","infra","open","urban","edu3")

# 如果不确定变量名，先运行：
# names(df)

# ----------------------------
# 4️⃣ 构造 DML 数据
# ----------------------------
library(data.table)

# 转成 data.table
df <- as.data.table(df)

# 构造 DML 数据
dml_data <- DoubleMLData$new(
  data = df[, c("Y_resid","D_resid", X_vars), with = FALSE],
  y_col = "Y_resid",
  d_cols = "D_resid"
)


# ----------------------------
# learners
# ----------------------------

rf_learner <- lrn("regr.ranger", num.trees = 500)

xgb_learner <- lrn("regr.xgboost",
                   nrounds = 200,
                   eta = 0.05,
                   max_depth = 6,
                   objective = "reg:squarederror")

# ----------------------------
# 6️⃣ DML（k = 4）
# ----------------------------

# Random Forest (k=4)
dml_rf_k4 <- DoubleMLPLR$new(
  dml_data,
  ml_g = rf_learner,
  ml_m = rf_learner,
  n_folds = 4
)

dml_rf_k4$fit()

# XGBoost (k=4)
dml_xgb_k4 <- DoubleMLPLR$new(
  dml_data,
  ml_g = xgb_learner,
  ml_m = xgb_learner,
  n_folds = 4
)

dml_xgb_k4$fit()


# ----------------------------
# 7️⃣ DML（k = 5）
# ----------------------------

# Random Forest (k=5)
dml_rf_k5 <- DoubleMLPLR$new(
  dml_data,
  ml_g = rf_learner,
  ml_m = rf_learner,
  n_folds = 5
)

dml_rf_k5$fit()

# XGBoost (k=5)
dml_xgb_k5 <- DoubleMLPLR$new(
  dml_data,
  ml_g = xgb_learner,
  ml_m = xgb_learner,
  n_folds = 5
)

dml_xgb_k5$fit()


# ----------------------------
# 8️⃣ 提取并格式化函数（更稳版本）
# ----------------------------

extract_dml <- function(model){
  
  coef  <- as.numeric(model$coef)[1]
  se    <- as.numeric(model$se)[1]
  pval  <- as.numeric(model$pval)[1]
  
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.1, "*", "")))
  
  coef_fmt <- sprintf("%.4f%s", coef, stars)
  se_fmt   <- sprintf("(%.4f)", se)
  
  return(list(coef = coef_fmt, se = se_fmt))
}


# ----------------------------
# 9️⃣ 提取四个模型
# ----------------------------

m1 <- extract_dml(dml_rf_k4)
m2 <- extract_dml(dml_xgb_k4)
m3 <- extract_dml(dml_rf_k5)
m4 <- extract_dml(dml_xgb_k5)


# ----------------------------
# 🔟 组装论文格式表格
# ----------------------------

result_table <- data.frame(
  term = c("treat × post",""),
  `(1)` = c(m1$coef, m1$se),
  `(2)` = c(m2$coef, m2$se),
  `(3)` = c(m3$coef, m3$se),
  `(4)` = c(m4$coef, m4$se),
  check.names = FALSE
)

print(result_table)
