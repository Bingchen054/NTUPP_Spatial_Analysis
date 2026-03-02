
install.packages("sjPlot")
install.packages("interactions")
install.packages("ggeffects")
install.packages("patchwork")

library(patchwork)

library(fixest)
library(sjPlot)
library(interactions)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(interactions)
library(tidyr)

controls <- c("econ1", "gov", "lpr", "infra", "open", "urban", "edu3")
rhs <- paste(c("did", controls), collapse = " + ")


plot_did_interaction <- function(
    model,
    moderator,        # name of moderator variable (string)
    data,
    year_min,
    panel_title,      # "(A) ...", "(B) ..."
    file_name
) {
  
  # -----------------------------
  # 1. Restrict to estimation sample
  # -----------------------------
  est_sample <- data %>%
    filter(year >= year_min) %>%
    filter(!is.na(EMP), !is.na(did), !is.na(.data[[moderator]]))
  
  low_m  <- min(est_sample[[moderator]], na.rm = TRUE)
  high_m <- max(est_sample[[moderator]], na.rm = TRUE)
  
  # -----------------------------
  # 2. Marginal predictions
  # -----------------------------
  pred <- ggpredict(
    model,
    terms = c(
      "did [0,1]",
      paste0(moderator, " [", low_m, ",", high_m, "]")
    )
  )
  
  # ±1 SE bands
  pred$conf.low  <- pred$predicted - pred$std.error
  pred$conf.high <- pred$predicted + pred$std.error
  
  pred_df <- as.data.frame(pred)
  
  # -----------------------------
  # 3. Compute within-group DID slopes
  # -----------------------------
  slope_df <- pred_df %>%
    select(group, x, predicted) %>%
    pivot_wider(names_from = x, values_from = predicted) %>%
    mutate(slope = `1` - `0`)
  
  # -----------------------------
  # 4. Plot
  # -----------------------------
  p <- ggplot(
    pred,
    aes(x = x, y = predicted, color = group)
  ) +
    geom_line(aes(linetype = group), linewidth = 1.1) +
    geom_point(aes(shape = group), size = 3) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high, fill = group),
      alpha = 0.18,
      color = NA,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = c(0, 1),
      labels = c("DID = 0", "DID = 1")
    ) +
    scale_color_manual(
      values = c("#1F4E79", "#B22222"),
      labels = c("Low built-up area", "High built-up area")
    ) +
    scale_fill_manual(values = c("#C7DDEA", "#F2C6C2")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_shape_manual(values = c(16, 18)) +
    labs(
      x = "",
      y = "EMP",
      title = panel_title,
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text    = element_text(color = "black"),
      axis.title.y = element_text(size = 13),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text  = element_text(size = 11),
      legend.key.width  = unit(2.2, "cm"),
      legend.key.height = unit(0.9, "cm"),
      legend.background = element_rect(color = "black", linewidth = 0.5, fill = NA),
      legend.box.background = element_rect(color = "black", linewidth = 0.5, fill = NA)
    ) +
    guides(
      linetype = "none",
      shape = "none",
      fill = "none",
      color = guide_legend(override.aes = list(linewidth = 0.9))
    ) +
    annotate(
      "rect",
      xmin = -Inf, xmax = Inf,
      ymin = -Inf, ymax = Inf,
      fill = NA,
      color = "black",
      linewidth = 0.8
    ) +
    geom_text(
      data = slope_df,
      aes(
        x = 1,
        y = `1`,
        label = paste0("\u03B2 = ", round(slope, 3)),
        color = group
      ),
      hjust = 0.8,
      vjust = -1.2,
      size = 4.5,
      show.legend = FALSE
    )
  
  # -----------------------------
  # 5. Save figure
  # -----------------------------
  ggsave(
    filename = file_name,
    plot = p,
    width = 6,
    height = 5,
    units = "in",
    dpi = 600
  )
  
  return(p)
}


# -----------------------------
# 6. models
# -----------------------------
m_A <- feols(
  EMP ~ did * builtarea + econ1 + gov + lpr + infra + open + urban + edu3 |
    city + year,
  data = df %>% filter(year >= 2009),
  vcov = "hetero"
)

p_A <- plot_did_interaction(
  model       = m_A,
  moderator   = "builtarea",
  data        = df,
  year_min    = 2009,
  panel_title = "(A) Moderating effect of built-up area",
  file_name   = "/Users/libingchen/Desktop/interaction/Figure_A.png"
)

print(p_A)

ggsave(
  filename = "/Users/libingchen/Desktop/interaction/Figure_A.png",
  plot = p_int1_final,
  width = 6,
  height = 9,
  units = "in",
  dpi = 600
)


# -----------------------------
m_B <- feols(
  EMP ~ did * spat_compact + econ1 + gov + lpr + infra + open + urban + edu3 |
    city + year,
  data = df %>% filter(year >= 2007),
  vcov = "hetero"
)
summary(m_B)
p_B <- plot_did_interaction(
  model       = m_B,
  moderator   = "spat_compact",
  data        = df,
  year_min    = 2007,
  panel_title = "(B) Moderating effect of spatial compactness",
  file_name   = "/Users/libingchen/Desktop/interaction/Figure_B.png"
)

print(p_B)

ggsave(
  filename = "/Users/libingchen/Desktop/interaction/Figure_B.png",
  plot = p_B,
  width = 6,
  height = 9,
  units = "in",
  dpi = 600
)

# -----------------------------
# Marginal predictions for C (2010)
pred_C <- ggpredict(
  m_C,
  terms = c(
    "did [0,1]",
    paste0("ln_bh_2010_t [",
           min(df$ln_bh_2010_t, na.rm = TRUE), ",",
           max(df$ln_bh_2010_t, na.rm = TRUE), "]")
  )
)

pred_C_df <- as.data.frame(pred_C)

library(tidyr)
library(dplyr)

delta_C <- pred_C_df %>%
  select(group, x, predicted) %>%
  pivot_wider(
    names_from = x,
    values_from = predicted
  ) %>%
  mutate(
    delta_EMP = `1` - `0`
  )

m_C <- feols(
  EMP ~ did * ln_bh_2010_t + econ1 + gov + lpr + infra + open + urban + edu3 |
    city + year,
  data = df %>% filter(year >= 2009),
  vcov = "hetero"
)

p_C <- plot_did_interaction(
  model       = m_C,
  moderator   = "ln_bh_2010_t",
  data        = df,
  year_min    = 2009,
  panel_title = "(C) Moderating effect of built-up area (2010)",
  file_name   = "/Users/libingchen/Desktop/interaction/Figure_C.png"
)

print(p_C)
ggsave(
  filename = "/Users/libingchen/Desktop/interaction/Figure_C.png",
  plot = p_C,
  width = 6,
  height = 9,
  units = "in",
  dpi = 600
)
# -----------------------------
m_D <- feols(
  EMP ~ did * ln_bh_2020_t + econ1 + gov + lpr + infra + open + urban + edu3 |
    city + year,
  data = df %>% filter(year >= 2009),
  vcov = "hetero"
)

p_D <- plot_did_interaction(
  model       = m_D,
  moderator   = "ln_bh_2020_t",
  data        = df,
  year_min    = 2009,
  panel_title = "(D) Moderating effect of built-up area (2020)",
  file_name   = "/Users/libingchen/Desktop/interaction/Figure_D.png"
)
print(p_D)
ggsave(
  filename = "/Users/libingchen/Desktop/interaction/Figure_D.png",
  plot = p_D,
  width = 6,
  height = 9,
  units = "in",
  dpi = 600
)


