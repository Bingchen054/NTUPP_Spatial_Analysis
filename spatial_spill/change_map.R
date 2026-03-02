############################################
## EMP 变化（百分比）城市层级空间分布图
############################################

## 0. Install required packages (if not installed)
required_pkgs <- c(
  "sf",
  "dplyr",
  "stringr",
  "ggplot2",
  "scico",
  "ggspatial",
  "haven",
  "tidyr"
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
library(tidyr)

## 1. Load data
setwd("/Users/libingchen/Desktop/NTUP_BL/maps")

citymap_raw   <- read_sf("chinacity2013mini/chinacity2013mini.shp")
citylinemap_raw <- read_sf("chinacity2013mini/chinacity2013mini_line.shp")
emp_df <- read_dta("/Users/libingchen/Desktop/NTUP_BL/spatial_spill/spatialspill.dta")

## ======================================================
## MOD 1: 清洗 polygon 图层
## ======================================================
citymap <- citymap_raw %>%
  filter(class == "城市")

## ======================================================
## MOD 2: 清洗 line 图层
## ======================================================
citylinemap <- citylinemap_raw %>%
  filter(class %in% c(
    "城市",
    "九段线",
    "小地图框格",
    "胡焕庸线"
  ))

## 3. City name standardization
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

## ======================================================
## 构造百分比变化（2007→2021）
## ======================================================
emp_delta_df <- emp_df %>%
  filter(year %in% c(2007, 2021)) %>%
  select(city_std, year, EMP) %>%
  pivot_wider(names_from = year, values_from = EMP) %>%
  mutate(
    change = if_else(
      is.na(`2007`) | `2007` == 0,
      0,
      (`2021` - `2007`) / `2007` * 100
    )
  ) %>%
  select(city_std, change)

## 合并
citymap2 <- citymap_city %>%
  left_join(emp_delta_df, by = "city_std") %>%
  mutate(
    change = if_else(is.na(change), 0, change)
  )

## ======================================================
## 新增：识别 treated 城市并生成内部点
## ======================================================
treated_city_df <- emp_df %>%
  filter(treat == 1) %>%
  distinct(city_std)

treated_city_sf <- citymap_city %>%
  filter(city_std %in% treated_city_df$city_std) %>%
  st_point_on_surface()

## ======================================================
## 4. 重新分位分组（完全按原逻辑）
## ======================================================
q_breaks <- quantile(
  citymap2$change,
  probs = seq(0, 1, 0.25),
  na.rm = TRUE
)

q_breaks[1] <- min(citymap2$change, na.rm = TRUE)
q_breaks[length(q_breaks)] <- max(citymap2$change, na.rm = TRUE)

citymap2$change_q <- cut(
  citymap2$change,
  breaks = unique(q_breaks),
  include.lowest = TRUE,
  right = TRUE
)

## ======================================================
## 5. Plot（完全一致 + treated 灰点）
## ======================================================
cols_oslo <- scico::scico(6, palette = "oslo")
cols_oslo <- cols_oslo[-1]
cols_oslo <- rev(cols_oslo)

p_change <- ggplot(citymap2) +
  geom_sf(
    aes(fill = change_q),
    color = "gray",
    linewidth = 0.01
  ) +
  
  geom_sf(
    data = citylinemap,
    color = "black",
    linewidth = 0.3,
    show.legend = FALSE
  ) +
  
  # Treated 灰点
  geom_sf(
    data = treated_city_sf,
    shape = 21,
    size = 1.2,
    stroke = 0.3,
    fill = "grey40",
    color = "black",
    show.legend = FALSE
  ) +
  
  scale_fill_manual(
    values = c(
      "#C97B7B",  # 负大
      "#E7B4B4",  # 负小
      "#F2F2F2",  # 中间
      "#9FD3C7",  # 正小
      "#2F7F75"   # 正大
    ),
    name = "ΔEMP%"
  ) +
  
  labs(
    title = "(C) Spatial Distribution of Percentage Change in EMP (2007–2021)"
  ) +
  
  coord_sf(
    crs = st_crs(citymap2),
    datum = NA,
    expand = FALSE
  ) +
  
  # 修复：删除 bar_width 参数
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "bar",
    bar_cols = c("black", "white"),
    line_width = 0.6,
    text_cex = 0.8,
    text_col = "black"
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 13
    ),
    legend.position = c(0.85, 0.60),
    legend.justification = c(0, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(
      color = "grey80",
      linewidth = 0.3,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6)
  )

print(p_change)

output_path <- "/Users/libingchen/Desktop/NTUP_BL/spatial_spill"
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(output_path, "EMP_change_city_quintiles.png"),
  plot = p_change,
  width = 8,
  height = 6,
  dpi = 600
)
