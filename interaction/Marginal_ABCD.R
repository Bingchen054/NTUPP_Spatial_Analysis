
install.packages("sjPlot")
install.packages("interactions")
install.packages("ggeffects")

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


m_int1 <- feols(
  EMP ~ did * builtarea + econ1 + gov + lpr + infra + open + urban + edu3 |
    city + year,
  data = df %>% filter(year >= 2009),
  vcov = "hetero"
)

summary(m_int1)

# Marginal predictions at DID = 0/1 and low/high built-up area
pred_ba <- ggpredict(
  m_int1,
  terms = c(
    "did [0,1]",
    paste0("builtarea [", low_ba, ",", high_ba, "]")
  )
)

# Replace CI with ±1 standard error bands
pred_ba$conf.low  <- pred_ba$predicted - pred_ba$std.error
pred_ba$conf.high <- pred_ba$predicted + pred_ba$std.error



pred_df <- as.data.frame(pred_ba)

# Slope = predicted(DID = 1) − predicted(DID = 0)
slope_df <- pred_df %>%
  select(group, x, predicted) %>%
  pivot_wider(
    names_from = x,
    values_from = predicted
  ) %>%
  mutate(
    slope = `1` - `0`
  )

p_int1_final <- ggplot(
  pred_ba,
  aes(
    x = x,
    y = predicted,
    color = group
  )
) +
  # Main lines (solid vs dashed)
  geom_line(
    aes(linetype = group),
    linewidth = 1.1
  ) +
  # End-point markers
  geom_point(
    aes(shape = group),
    size = 3
  ) +
  # ±1 SE uncertainty bands
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high, fill = group),
    alpha = 0.18,
    color = NA,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("DID = 0", "DID = 1"),
  )+
  scale_color_manual(
    values = c("#1F4E79", "#B22222"),
    labels = c("Low built-up area", "High built-up area")
  ) +
  scale_fill_manual(
    values = c("#C7DDEA", "#F2C6C2")
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed")
  ) +
  scale_shape_manual(
    values = c(16, 18)
  ) +
  labs(
    x = "",
    y = "EMP",
    title = "(A) Moderating effect of built-up area",
    caption = ""
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
    legend.box.background = element_rect(color = "black", linewidth = 0.5, fill = NA),
    plot.caption = element_text(hjust = 0, size = 10)
  ) +
  guides(
    linetype = "none",
    shape = "none",
    fill = "none",
    color = guide_legend(
      override.aes = list(linewidth = 0.9)
    )
  ) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill = NA,
    color = "black",
    linewidth = 0.8
  )
p_int1_final <- p_int1_final +
  geom_text(
    data = slope_df,
    aes(
      x = 1,
      y = `1`,
      label = paste0("\u03B2 = ", round(slope, 3)),
      color = group
    ),
    hjust = 0.8,   # ⭐ 水平居中
    vjust = -1.2,  # ⭐ 点的正上方（关键）
    size = 4.5,
    show.legend = FALSE
  )



print(p_int1_final)


ggsave(
  filename = "/Users/libingchen/Desktop/interaction/Figure_A_interaction.png",
  plot = p_int1_final,
  width = 6,
  height = 9,
  units = "in",
  dpi = 600
)

