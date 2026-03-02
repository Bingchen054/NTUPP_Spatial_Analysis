############################################################
# Packages
############################################################

pkgs <- c(
  "tidyverse", "fixest", "broom", "stringr",
  "ggplot2", "patchwork", "haven", "scales"
)
installed <- rownames(installed.packages())
to_install <- setdiff(pkgs, installed)
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

############################################################
# Load data
############################################################

df <- read_dta("/Users/libingchen/Desktop/spatial_spill/data_1108.dta") %>%
  filter(year >= 2007)

############################################################
# Model A: cluster(city)
############################################################

m_spill_city <- feols(
  EMP ~ did +
    dist_0_50 + dist_50_100 + dist_100_150 + dist_150_200 +
    dist_200_250 + dist_250_300 + dist_300_350 + dist_350_400 +
    econ1 + gov + lpr + infra + open + urban + edu3 |
    year + prov,
  data = df,
  cluster = ~city
)

############################################################
# Model B: cluster(province)
############################################################

m_spill_prov <- feols(
  EMP ~ did +
    dist_0_50 + dist_50_100 + dist_100_150 + dist_150_200 +
    dist_200_250 + dist_250_300 + dist_300_350 + dist_350_400 +
    econ1 + gov + lpr + infra + open + urban + edu3 |
    year + prov,
  data = df,
  cluster = ~prov
)

############################################################
# Plotting function (shared by A/B)
############################################################

plot_gradient <- function(model, panel_title) {
  
  coef_df <- tidy(model) %>%
    filter(str_detect(term, "^dist_")) %>%
    mutate(
      dist = case_when(
        str_detect(term, "0_50")   ~ "0–50",
        str_detect(term, "50_100") ~ "50–100",
        str_detect(term, "100_150")~ "100–150",
        str_detect(term, "150_200")~ "150–200",
        str_detect(term, "200_250")~ "200–250",
        str_detect(term, "250_300")~ "250–300",
        str_detect(term, "300_350")~ "300–350",
        str_detect(term, "350_400")~ "350–400"
      ),
      ci_low  = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    drop_na(dist) %>%
    mutate(
      dist = factor(
        dist,
        levels = c(
          "0–50","50–100","100–150","150–200",
          "200–250","250–300","300–350","350–400"
        )
      ),
      sign = ifelse(estimate >= 0, "Positive", "Negative"),
      sig  = ifelse(ci_low > 0 | ci_high < 0, "**", "")
    )
  
  # statistics
  adj_r2 <- round(as.numeric(fitstat(model, "ar2")), 3)
  
  did_row <- tidy(model) %>% filter(term == "did")
  did_est <- round(as.numeric(did_row$estimate), 3)
  did_p   <- as.numeric(did_row$p.value)
  did_sig <- case_when(
    did_p < 0.01 ~ "***",
    did_p < 0.05 ~ "**",
    did_p < 0.10 ~ "*",
    TRUE ~ ""
  )
  
  ymin <- min(coef_df$ci_low, na.rm = TRUE)  - 0.3
  ymax <- max(coef_df$ci_high, na.rm = TRUE) + 0.3
  
  pos_col <- alpha("#5FA8A6", 0.85)
  neg_col <- alpha("#D88C8C", 0.85)
  
  ggplot(coef_df, aes(x = dist, y = estimate, fill = sign)) +
    geom_col(width = 0.65, color = "grey25", linewidth = 0.35) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high),
      width = 0.12, linewidth = 0.35, color = "grey35"
    ) +
    geom_point(shape = 16, size = 2.0, color = "grey20") +
    geom_hline(yintercept = 0, linewidth = 0.9, color = "black") +
    geom_text(
      data = subset(coef_df, sig != ""),
      aes(
        label = sig,
        y = ifelse(estimate >= 0, ci_high + 0.18, ci_low - 0.18)
      ),
      size = 5.5, fontface = "bold", color = "black"
    ) +
    scale_fill_manual(
      values = c("Positive" = pos_col, "Negative" = neg_col),
      guide = "none"
    ) +
    scale_y_continuous(breaks = c(-2, -1, 0, 1, 2)) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    annotate(
      "segment",
      x = 5.5, xend = 5.5,
      y = ymin, yend = ymax,
      linetype = "dashed", linewidth = 0.4, color = "grey40"
    ) +
    annotate(
      "text",
      x = 0.6, y = ymax - 0.05,
      hjust = 0, vjust = 1,
      label = paste0(
        "Adj. R\u00B2 = ", adj_r2, "\n",
        "\u03B2(DID) = ", did_est, did_sig
      ),
      size = 4.8
    ) +
    labs(
      title = panel_title,
      x = "Buffers (km)",
      y = "Estimator"
    ) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 11),
      axis.line = element_line(linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
      plot.margin = margin(6, 10, 6, 6)
    )
}

############################################################
# Generate A / B panels and combine
############################################################

p_A <- plot_gradient(m_spill_city, "(A) Clustered by city")
p_B <- plot_gradient(m_spill_prov, "(B) Clustered by province")

fig_AB <- p_A + p_B + plot_layout(ncol = 2)

print(fig_AB)

ggsave(
  filename = "/Users/libingchen/Desktop/spatial_spill/estamator.svg",
  plot = fig_AB,
  width = 16,
  height = 9,
  units = "in",
  dpi = 600
)

