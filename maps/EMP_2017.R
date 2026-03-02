############################################
## EMP 城市层级空间分布图（原逻辑 + 清洗融合版）
############################################

## 0. Install required packages (if not installed)
required_pkgs <- c(
  "sf",
  "dplyr",
  "stringr",
  "ggplot2",
  "scico",
  "ggspatial",
  "haven"
)

installed_pkgs <- rownames(installed.packages())

for (pkg in required_pkgs) {
  if (!pkg %in% installed_pkgs) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(scico)
library(ggspatial)
library(haven)

## 1. Load data (already prepared)
setwd("/Users/libingchen/Desktop/NTUP_BL/maps")

citymap_raw   <- read_sf("chinacity2013mini/chinacity2013mini.shp")
citylinemap_raw <- read_sf("chinacity2013mini/chinacity2013mini_line.shp")
emp_df <- read_dta("EMP_2017.dta")

## ======================================================
## >>> MOD 1: 清洗 polygon 图层（只保留“城市”）
##     目的：去掉指北针 / 比例尺 / 小地图框格等多边形装饰
## ======================================================
citymap <- citymap_raw %>%
  filter(class == "城市")

## ======================================================
## >>> MOD 2: 清洗 line 图层（只保留需要的线要素）
##     目的：去掉指北针线条 / 比例尺线条等制图装饰
## ======================================================
citylinemap <- citylinemap_raw %>%
  filter(class %in% c(
    "城市",
    "九段线",
    "小地图框格",
    "胡焕庸线"
  ))

## 3. City name standardization & filtering 
citymap_city <- citymap %>%
  mutate(
    city_std = 市 %>%
      str_extract("^.+?市") %>%
      str_trim()
  )

emp_df <- emp_df %>%
  mutate(
    city_std = city %>%
      str_extract("^.+?市") %>%
      str_trim()
  )

citymap2 <- citymap_city %>%
  left_join(emp_df, by = "city_std") %>%
  mutate(
    EMP = if_else(is.na(EMP), 0, EMP)
  )

nrow(citymap2)
nrow(citymap)

sum(is.na(citymap2$EMP))

na_table <- citymap2 %>%
  filter(is.na(EMP)) %>%
  distinct(市, city_std) %>%
  arrange(city_std)

na_table

n_distinct(citymap2$city_std)

## 4. Recompute EMP groups using quartiles with 0 as lower bound
q_breaks <- quantile(
  citymap2$EMP,
  probs = seq(0, 1, 0.25),
  na.rm = TRUE
)

q_breaks[1] <- 0
q_breaks[length(q_breaks)] <- max(citymap2$EMP, na.rm = TRUE)

citymap2$EMP_q <- cut(
  citymap2$EMP,
  breaks = unique(q_breaks),
  include.lowest = TRUE,
  right = TRUE
)

## 5. Plot: mini distribution (quintiles)
cols_oslo <- scico::scico(6, palette = "oslo")
cols_oslo <- cols_oslo[-1]     # drop whitest
cols_oslo <- rev(cols_oslo)    # low -> light, high -> dark

p_emp <- ggplot(citymap2) +
  geom_sf(
    aes(fill = EMP_q),
    color = "gray",
    linewidth = 0.01
  ) +
  
  geom_sf(
    data = citylinemap,
    color = "black",
    linewidth = 0.3,
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "#fee8c8",
      "#fdbb84",
      "#fc8d59",
      "#e34a33",
      "#b30000"
    ),
    name = "EMP (quartiles)"
  ) +
  
  labs(
    title = "Spatial Distribution of EMP across Chinese Prefecture-level Cities (2017)",
  ) +
  
  coord_sf(
    crs = st_crs(citymap2),
    datum = NA,
    expand = FALSE
  ) +
  
  # >>> FIX: 稳定的黑白比例尺（显式指定长度，避免 ggspatial 崩溃）
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "bar",
    bar_cols = c("black", "white"),
    line_width = 0.6,
    text_cex = 0.8,
    text_col = "black",
    bar_width = 1000
  ) +
  
  theme(
    axis.text.x = element_text(size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.ticks  = element_line(color = "black"),
    
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = c(0.85, 0.60),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5),
    
    panel.grid.major = element_line(
      color = "grey80",
      linewidth = 0.3,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank(),
    
    
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

print(p_emp)

output_path <- "/Users/libingchen/Desktop/NTUP_BL/maps"

ggsave(
  filename = file.path(output_path, "EMP_2017_city_quartiles.png"),
  plot = p_emp,
  width = 8,
  height = 6,
  dpi = 300
)


