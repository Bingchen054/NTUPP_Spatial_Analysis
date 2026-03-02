# ============================================================
# Resource-based Heterogeneity
# Bootstrap DID + Violin Plot
# ============================================================

# Packages
library(haven)
library(dplyr)
library(fixest)
library(ggplot2)

# Data
df <- read_dta("/Users/libingchen/Desktop/NTUP_BL/heterogenity/heter.dta")

# Controls
controls <- c("econ1", "gov", "lpr", "infra", "open", "urban", "edu3")

# Regression formula
rhs <- paste(c("did", controls), collapse = " + ")
fml <- as.formula(paste0("EMP ~ ", rhs, " | city + year"))



# ============================================================
# (A) Resource-based heterogeneity
# ============================================================
# One bootstrap draw of DID
get_did <- function(d){
  as.numeric(coef(feols(fml, data = d, vcov = "hetero"))["did"])
}

# Bootstrap by city
set.seed(123)
boot_did <- function(flag){
  replicate(500, {
    samp <- sample(unique(df$city), replace = TRUE)
    d <- df %>% filter(year >= 2007, resource == flag, city %in% samp)
    get_did(d)
  })
}

did_r1 <- boot_did(1)
did_r0 <- boot_did(0)

# Bootstrap data for plotting
boot_df <- data.frame(
  did = c(did_r1, did_r0),
  group = factor(rep(c("Resource cities", "Non-resource cities"),
                     each = length(did_r1)))
)

# Baseline regressions (point estimates)
m_r1 <- feols(fml, data = df %>% filter(year >= 2007, resource == 1), vcov = "hetero")
m_r0 <- feols(fml, data = df %>% filter(year >= 2007, resource == 0), vcov = "hetero")

# Regression table (DID only)
etable(m_r1, m_r0, keep = "did", tex = FALSE)

coef_df <- data.frame(
  group   = c("Resource cities", "Non-resource cities"),
  did_hat = c(coef(m_r1)["did"], coef(m_r0)["did"])
)

# Distribution-level p-value
p_val <- wilcox.test(did_r1, did_r0)$p.value
p_lab <- paste0("p-value (Diff.)=", formatC(p_val, format = "e", digits = 2))
# y positions for annotations
y_top <- max(boot_df$did, na.rm = TRUE)
y_mid <- y_top + 0.03   # position for β DID labels (above violins)
y_high <- y_top + 0.10  # position for bracket
# Text labels under each violin
coef_labels <- data.frame(
  x = c(1, 2),
  y = rep(y_mid, 2),
  label = c(
    paste0("beta_DID= ", round(coef(m_r0)["did"], 3),
           ifelse(pvalue(m_r0)["did"] < 0.01, "***",
                  ifelse(pvalue(m_r0)["did"] < 0.05, "**",
                         ifelse(pvalue(m_r0)["did"] < 0.1, "*", "")))),
    paste0("beta_DID = ", round(coef(m_r1)["did"], 3),
           ifelse(pvalue(m_r1)["did"] < 0.01, "***",
                  ifelse(pvalue(m_r1)["did"] < 0.05, "**",
                         ifelse(pvalue(m_r1)["did"] < 0.1, "*", ""))))
  )
)


p <- ggplot(boot_df, aes(x = group, y = did)) +
  geom_violin(aes(fill = group), alpha = 0.65, trim = FALSE, color = "grey30") +
  geom_boxplot(width = 0.12, fill = "white",
               outlier.shape = NA, color = "grey30") +
  geom_jitter(aes(color = group), width = 0.12, size = 1.1, alpha = 0.25) +
  geom_point(
    data = coef_df,
    aes(x = group, y = did_hat, fill = group),
    inherit.aes = FALSE,
    shape = 21, size = 4, color = "black", stroke = 0.6
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
  annotate("segment", x = 1, xend = 2,
           y = max(boot_df$did, na.rm = TRUE) + 0.05,
           yend = max(boot_df$did, na.rm = TRUE) + 0.05,
           color = "grey25") +
  annotate("segment", x = 1, xend = 1,
           y = max(boot_df$did, na.rm = TRUE) + 0.03,
           yend = max(boot_df$did, na.rm = TRUE) + 0.05,
           color = "grey25") +
  annotate("segment", x = 2, xend = 2,
           y = max(boot_df$did, na.rm = TRUE) + 0.03,
           yend = max(boot_df$did, na.rm = TRUE) + 0.05,
           color = "grey25") +
  annotate(
    "text",
    x = 1.5,
    y = y_high,
    label = paste0(
      "p-value (Diff.) = ",
      formatC(p_val_m, format = "e", digits = 2),
      if (p_val_m < 0.01) "***"
      else if (p_val_m < 0.05) "**"
      else if (p_val_m < 0.1) "*"
      else ""
    ),
    size = 5,
    color = "black"
  )+
  
  annotate(
    "text",
    x = coef_labels$x,
    y = coef_labels$y,
    label = coef_labels$label,
    size = 4.5,
    fontface = "italic"
  ) +
  scale_fill_manual(values = c(
    "Resource cities"     = "#FF4EA6",
    "Non-resource cities" = "#6EC6FF"
  )) +
  scale_color_manual(values = c(
    "Resource cities"     = "#FF4EA6",
    "Non-resource cities" = "#0E4FB0"
  )) +
  labs(
    x = "",
    y = "DID coefficient",
    title = "(A) Resource-based heterogeneity"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold", size = 13,  color = "black"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  )+ 
  annotate(
  "rect",
  xmin = -Inf, xmax = Inf,
  ymin = -Inf, ymax = Inf,
  fill = NA,
  color = "black",
  linewidth = 0.8
)

print(p)

ggsave(
  "/Users/libingchen/Desktop/hetro/Fig3A_Resource_based_heterogeneity.png",
  plot = p,
  width = 6,
  height = 9
)





# ============================================================
# (B) Regional heterogeneity (East vs Non-East)
# ============================================================
m_east_0 <- feols(
  fml,
  data = df %>% filter(year >= 2007, east == 0),
  vcov = ~ city
)

m_east_1 <- feols(
  fml,
  data = df %>% filter(year >= 2007, east == 1),
  vcov = ~ city
)

etable(
  m_east_0, m_east_1,
  keep = "%did",
  dict = c(did = "DID"),
  tex = FALSE
)

# Bootstrap
set.seed(123)
boot_did_east <- function(flag){
  replicate(500, {
    samp <- sample(unique(df$city), replace = TRUE)
    d <- df %>% filter(year >= 2007, east == flag, city %in% samp)
    get_did(d)
  })
}

did_e1 <- boot_did_east(1)   # East
did_e0 <- boot_did_east(0)   # Non-East

boot_df_e <- data.frame(
  did = c(did_e1, did_e0),
  group = factor(rep(c("East cities", "Non-East cities"),
                     each = length(did_e1)))
)

# Point estimates
coef_df_e <- data.frame(
  group   = c("East cities", "Non-East cities"),
  did_hat = c(coef(m_east_1)["did"], coef(m_east_0)["did"])
)

# Distribution-level p-value
p_val_e <- wilcox.test(did_e1, did_e0)$p.value
p_lab_e <- paste0("p-value(Diff.) = ", formatC(p_val_e, format = "e", digits = 2))

# Y positions (统一、唯一)
y_top  <- max(boot_df_e$did, na.rm = TRUE)
# Heights (final tuned version)
y_mid  <- y_top + 0.035   # β DID：紧贴提琴上方
y_p    <- y_top + 0.075   # Diff. P：在括号下方、居中
y_high <- y_top + 0.095   # 括号：最上层



# β DID labels (左右错开)
coef_labels_e <- data.frame(
  x = c(0.95, 2.05),
  y = rep(y_mid, 2),
  label = c(
    paste0("beta_DID = ", round(coef(m_east_1)["did"], 3),
           ifelse(pvalue(m_east_1)["did"] < 0.01, "***",
                  ifelse(pvalue(m_east_1)["did"] < 0.05, "**",
                         ifelse(pvalue(m_east_1)["did"] < 0.1, "*", "")))),
    paste0("beta_DID = ", round(coef(m_east_0)["did"], 3),
           ifelse(pvalue(m_east_0)["did"] < 0.01, "***",
                  ifelse(pvalue(m_east_0)["did"] < 0.05, "**",
                         ifelse(pvalue(m_east_0)["did"] < 0.1, "*", ""))))
  )
)

# Plot
p_B <- ggplot(boot_df_e, aes(x = group, y = did)) +
  geom_violin(aes(fill = group), alpha = 0.65, trim = FALSE, color = "grey30") +
  geom_boxplot(width = 0.12, fill = "white",
               outlier.shape = NA, color = "grey30") +
  geom_jitter(aes(color = group), width = 0.12,
              size = 1.1, alpha = 0.25) +
  geom_point(
    data = coef_df_e,
    aes(x = group, y = did_hat, fill = group),
    inherit.aes = FALSE,
    shape = 21, size = 4,
    color = "black", stroke = 0.6
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "grey55") +
  
  # Bracket
  annotate("segment", x = 1, xend = 2,
           y = y_high, yend = y_high, color = "grey25") +
  annotate("segment", x = 1, xend = 1,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  annotate("segment", x = 2, xend = 2,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  
  # Diff. P
  annotate(
    "text",
    x = 1.5,
    y = y_high+0.05,
    label = paste0(
      "p-value (Diff.) = ",
      formatC(p_val_m, format = "e", digits = 2),
      if (p_val_m < 0.01) "***"
      else if (p_val_m < 0.05) "**"
      else if (p_val_m < 0.1) "*"
      else ""
    ),
    size = 5,
    color = "black"
  )+

  
  # β DID labels
  annotate("text",
           x = coef_labels_e$x,
           y = y_mid,
           label = coef_labels_e$label,
           size = 4.5,
           fontface = "italic") +
  
  scale_fill_manual(values = c(
    "East cities"     = "#00A6A6",  # teal
    "Non-East cities" = "#F26B6B"   # coral
  )) +
  scale_color_manual(values = c(
    "East cities"     = "#008C8C",  # deeper teal
    "Non-East cities" = "#C94A4A"   # deeper coral
  )) +
  labs(
    x = "",
    y = expression(DID ~ coefficient),
    title = "(B) Regional heterogeneity (East vs Non-East)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  )+
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = NA, color = "black", linewidth = 0.8)

print(p_B)

ggsave("/Users/libingchen/Desktop/hetro/Fig3B_East_heterogeneity.png",
       p_B, width = 6, height = 9)

# ============================================================
#  (C) Metropolitan heterogeneity (Metro vs Non-Metro)
# ============================================================

m_metro_0 <- feols(
  fml,
  data = df %>% filter(year >= 2007, metro == 0),
  vcov = "hetero"
)

m_metro_1 <- feols(
  fml,
  data = df %>% filter(year >= 2007, metro == 1),
  vcov = "hetero"
)

etable(
  m_metro_0, m_metro_1,
  keep = "%did",
  dict = c(did = "DID"),
  tex = FALSE
)

set.seed(123)
boot_did_metro <- function(flag){
  replicate(500, {
    samp <- sample(unique(df$city), replace = TRUE)
    d <- df %>% filter(year >= 2007, metro == flag, city %in% samp)
    get_did(d)
  })
}

did_m1 <- boot_did_metro(1)   # Metro
did_m0 <- boot_did_metro(0)   # Non-Metro

boot_df_m <- data.frame(
  did = c(did_m1, did_m0),
  group = factor(rep(c("Metro cities", "Non-metro cities"),
                     each = length(did_m1)))
)

coef_df_m <- data.frame(
  group   = c("Metro cities", "Non-metro cities"),
  did_hat = c(coef(m_metro_1)["did"], coef(m_metro_0)["did"])
)

# Diff. P with significance stars (robust version)
if (p_val_e < 0.01) {
  p_star_e <- "***"
} else if (p_val_e < 0.05) {
  p_star_e <- "**"
} else if (p_val_e < 0.1) {
  p_star_e <- "*"
} else {
  p_star_e <- ""
}

p_lab_e <- paste0(
  "p-value (Diff.) = ",
  formatC(p_val_e, format = "e", digits = 2),
  " ",
  p_star_e
)



y_top  <- max(boot_df_m$did, na.rm = TRUE)
y_mid  <- y_top + 0.020
y_high <- y_top + 0.110

coef_labels_m <- data.frame(
  x = c(0.95, 2.05),
  y = rep(y_mid, 2),
  label = c(
    paste0("beta_DID = ", round(coef(m_metro_1)["did"], 3),
           ifelse(pvalue(m_metro_1)["did"] < 0.01, "***",
                  ifelse(pvalue(m_metro_1)["did"] < 0.05, "**",
                         ifelse(pvalue(m_metro_1)["did"] < 0.1, "*", "")))),
    paste0("beta_DID = ", round(coef(m_metro_0)["did"], 3),
           ifelse(pvalue(m_metro_0)["did"] < 0.01, "***",
                  ifelse(pvalue(m_metro_0)["did"] < 0.05, "**",
                         ifelse(pvalue(m_metro_0)["did"] < 0.1, "*", ""))))
  )
)

p_C <- ggplot(boot_df_m, aes(x = group, y = did)) +
  geom_violin(aes(fill = group), alpha = 0.65, trim = FALSE, color = "grey30") +
  geom_boxplot(width = 0.12, fill = "white",
               outlier.shape = NA, color = "grey30") +
  geom_jitter(aes(color = group), width = 0.12,
              size = 1.1, alpha = 0.25) +
  geom_point(
    data = coef_df_m,
    aes(x = group, y = did_hat, fill = group),
    inherit.aes = FALSE,
    shape = 21, size = 4,
    color = "black", stroke = 0.6
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "grey55") +
  
  # Bracket
  annotate("segment", x = 1, xend = 2,
           y = y_high, yend = y_high, color = "grey25") +
  annotate("segment", x = 1, xend = 1,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  annotate("segment", x = 2, xend = 2,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  
  # Diff. P
  annotate(
    "text",
    x = 1.5,
    y = y_high + 0.018,
    label = paste0(
      "p-value (Diff.) = ",
      formatC(p_val_m, format = "e", digits = 2),
      if (p_val_m < 0.01) "***"
      else if (p_val_m < 0.05) "**"
      else if (p_val_m < 0.1) "*"
      else ""
    ),
    size = 5,
    color = "black"
  )+
  
  # β DID
  annotate("text",
           x = coef_labels_m$x,
           y = coef_labels_m$y,
           label = coef_labels_m$label,
           size = 4.5,
           fontface = "italic") +
  
  scale_fill_manual(values = c(
    "Metro cities"     = "#5A5CFF",  # indigo
    "Non-metro cities" = "#FFB000"   # amber
  )) +
  scale_color_manual(values = c(
    "Metro cities"     = "#3E40CC",  # deep indigo
    "Non-metro cities" = "#CC8A00"   # deep amber
  )) +
  labs(
    x = "",
    y = expression(DID ~ coefficient),
    title = "(C) Metropolitan vs. non-metropolitan cities"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  ) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = NA, color = "black", linewidth = 0.8)
  


print(p_C)

ggsave("/Users/libingchen/Desktop/hetro/Fig3C_Metro_heterogeneity.png",
       p_C, width = 6, height = 9)


# ============================================================
# (D) Inflow vs. non-inflow cities
# ============================================================
m_inflow_0 <- feols(
  fml,
  data = df %>% filter(year >= 2007, national_region == 0),
  vcov = ~ city
)

m_inflow_1 <- feols(
  fml,
  data = df %>% filter(year >= 2007, national_region == 1),
  vcov = ~ city
)

etable(
  m_inflow_0, m_inflow_1,
  keep = "%did",
  dict = c(did = "DID"),
  tex = FALSE
)

set.seed(123)
boot_did_inflow <- function(flag){
  replicate(500, {
    samp <- sample(unique(df$city), replace = TRUE)
    d <- df %>% filter(year >= 2007, national_region == flag, city %in% samp)
    get_did(d)
  })
}

did_i1 <- boot_did_inflow(1)   # Inflow cities
did_i0 <- boot_did_inflow(0)   # Non-inflow cities

boot_df_i <- data.frame(
  did = c(did_i1, did_i0),
  group = factor(rep(c("Inflow cities", "Non-inflow cities"),
                     each = length(did_i1)))
)

coef_df_i <- data.frame(
  group   = c("Inflow cities", "Non-inflow cities"),
  did_hat = c(coef(m_inflow_1)["did"], coef(m_inflow_0)["did"])
)

p_val_i <- wilcox.test(did_i1, did_i0)$p.value

if (p_val_i < 0.01) {
  p_star_i <- "***"
} else if (p_val_i < 0.05) {
  p_star_i <- "**"
} else if (p_val_i < 0.1) {
  p_star_i <- "*"
} else {
  p_star_i <- ""
}

p_lab_i <- paste0(
  "p-value (Diff.) = ",
  formatC(p_val_i, format = "e", digits = 2),
  " ",
  p_star_i
)

y_top  <- max(boot_df_i$did, na.rm = TRUE)
y_mid  <- y_top + 0.035   # β DID
y_p    <- y_top + 0.075   # Diff. P
y_high <- y_top + 0.095   # bracket

coef_labels_i <- data.frame(
  x = c(0.95, 2.05),
  y = rep(y_mid, 2),
  label = c(
    paste0("beta_DID = ", round(coef(m_inflow_1)["did"], 3),
           ifelse(pvalue(m_inflow_1)["did"] < 0.01, "***",
                  ifelse(pvalue(m_inflow_1)["did"] < 0.05, "**",
                         ifelse(pvalue(m_inflow_1)["did"] < 0.1, "*", "")))),
    paste0("beta_DID = ", round(coef(m_inflow_0)["did"], 3),
           ifelse(pvalue(m_inflow_0)["did"] < 0.01, "***",
                  ifelse(pvalue(m_inflow_0)["did"] < 0.05, "**",
                         ifelse(pvalue(m_inflow_0)["did"] < 0.1, "*", ""))))
  )
)

p_D <- ggplot(boot_df_i, aes(x = group, y = did)) +
  geom_violin(aes(fill = group), alpha = 0.65, trim = FALSE, color = "grey30") +
  geom_boxplot(width = 0.12, fill = "white",
               outlier.shape = NA, color = "grey30") +
  geom_jitter(aes(color = group), width = 0.12,
              size = 1.1, alpha = 0.25) +
  geom_point(
    data = coef_df_i,
    aes(x = group, y = did_hat, fill = group),
    inherit.aes = FALSE,
    shape = 21, size = 4,
    color = "black", stroke = 0.6
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "grey55") +
  
  # Bracket
  annotate("segment", x = 1, xend = 2,
           y = y_high, yend = y_high, color = "grey25") +
  annotate("segment", x = 1, xend = 1,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  annotate("segment", x = 2, xend = 2,
           y = y_mid + 0.005, yend = y_high, color = "grey25") +
  
  # Diff. P
  annotate("text",
           x = 1.5, y = y_p+0.1,
           label = p_lab_i,
           size = 5, color = "black") +
  
  # β DID
  annotate("text",
           x = coef_labels_i$x,
           y = coef_labels_i$y,
           label = coef_labels_i$label,
           size = 4.5,
           fontface = "italic",
           color = "black") +
  
  scale_fill_manual(values = c(
    "Inflow cities"     = "#2E8B57",  # sea green
    "Non-inflow cities" = "#8B5A2B"   # brown
  )) +
  scale_color_manual(values = c(
    "Inflow cities"     = "#1F6F4A",
    "Non-inflow cities" = "#6B4423"
  )) +
  labs(
    x = "",
    y = "DID coefficient",
    title = "(D) Inflow vs. non-inflow cities"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 13, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  ) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = NA, color = "black", linewidth = 0.8)

print(p_D)

ggsave("/Users/libingchen/Desktop/hetro/Fig3D_Inflow_heterogeneity.png",
       p_D, width = 6, height = 9)


