library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

# =========================
# 1️⃣ 讀資料
# =========================
df <- read_dta("/Users/libingchen/Desktop/NTUP_BL/Descriptive/descriptive.dta")

df$treat <- factor(df$treat,
                   levels = c(0,1),
                   labels = c("Control","Treatment"))

# =========================
# 2️⃣ 變量
# =========================
vars <- c("EMP","econ1","gov","edu3",
          "lpr","infra","open","urban")

# =========================
# 3️⃣ 顏色設定（對調後）
# EMP → 柔紅
# 控制變量 → 青綠
# =========================

# 柔紅系（EMP）
color_EMP  <- c("#D94A4A", "#F7A8A8")

# 青綠系（控制變量）
color_ctrl <- c("#007C7C", "#66D1D1")

# =========================
# 4️⃣ 作圖函數（CI 版本）
# =========================
make_plot <- function(varname){
  
  summary_df <- df %>%
    group_by(treat) %>%
    summarise(
      mean = mean(.data[[varname]], na.rm = TRUE),
      sd   = sd(.data[[varname]], na.rm = TRUE),
      n    = n(),
      se   = sd / sqrt(n),
      ci   = qt(0.975, df = n - 1) * se,
      .groups = "drop"
    )
  
  # t-test
  tt <- t.test(df[[varname]] ~ df$treat)
  t_value <- round(tt$statistic, 2)
  p_value <- tt$p.value
  
  stars <- ifelse(p_value < 0.01, "***",
                  ifelse(p_value < 0.05, "**",
                         ifelse(p_value < 0.1, "*", "")))
  
  label_text <- paste0("t = ", t_value, " ", stars)
  
  y_max     <- max(summary_df$mean + summary_df$ci, na.rm = TRUE)
  y_bracket <- y_max * 1.04
  y_text    <- y_max * 1.08
  
  # 每組抽樣 50
  set.seed(123)
  df_sample <- df %>%
    group_by(treat) %>%
    slice_sample(n = 50)
  
  # 顯示名稱
  display_name <- ifelse(varname == "econ1", "econ",
                         ifelse(varname == "edu3", "edu", varname))
  
  # 顏色選擇
  if(varname == "EMP"){
    fill_colors <- color_EMP
  } else {
    fill_colors <- color_ctrl
  }
  
  p <- ggplot(summary_df, aes(x = treat, y = mean, fill = treat)) +
    
    geom_col(width = 0.5, alpha = 0.9) +
    
    # 95% CI
    geom_errorbar(aes(ymin = mean - ci,
                      ymax = mean + ci),
                  width = 0.08,
                  size = 0.5) +
    
    # 散點改回透明灰
    geom_jitter(data = df_sample,
                aes(x = treat,
                    y = .data[[varname]]),
                width = 0.05,
                size = 1.1,
                alpha = 0.25,
                color = "grey50",
                inherit.aes = FALSE) +
    
    scale_fill_manual(values = fill_colors) +
    
    # bracket
    geom_segment(aes(x = 1, xend = 2,
                     y = y_bracket,
                     yend = y_bracket),
                 inherit.aes = FALSE,
                 size = 0.45) +
    
    geom_segment(aes(x = 1, xend = 1,
                     y = y_bracket,
                     yend = y_bracket*0.99),
                 inherit.aes = FALSE,
                 size = 0.45) +
    
    geom_segment(aes(x = 2, xend = 2,
                     y = y_bracket,
                     yend = y_bracket*0.99),
                 inherit.aes = FALSE,
                 size = 0.45) +
    
    annotate("text",
             x = 1.5,
             y = y_text,
             label = label_text,
             size = 3.4,
             color = "grey20") +
    
    labs(title = display_name,
         x = "",
         y = "") +
    
    theme_classic(base_size = 10) +
    theme(
      legend.position = "none",
      panel.border = element_rect(color = "black",
                                  fill = NA,
                                  size = 0.5),
      plot.title = element_text(hjust = 0.5,
                                size = 9,
                                face = "plain"),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8)
    ) +
    
    coord_cartesian(ylim = c(0, y_max*1.12))
  
  return(p)
}

# =========================
# 5️⃣ 生成圖
# =========================
plots <- lapply(vars, make_plot)

# =========================
# 6️⃣ 拼接 2 × 4
# =========================
final_plot <- wrap_plots(plots, ncol = 4) +
  plot_layout(guides = "collect") &
  theme(
    plot.margin = margin(2, 2, 2, 2),
    panel.spacing = unit(0.4, "lines")
  )

print(final_plot)


ggsave(
  filename = "/Users/libingchen/Desktop/NTUP_BL/Descriptive/All_variables_CI_panel_final.png",
  plot = final_plot,
  width = 12,
  height = 7,
  dpi = 300
)

