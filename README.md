# NTUPP, Urban Expansion, and Employment Quality

This repository contains the replication materials for the manuscript:

**"New-type Urbanization Policy, Urban Expansion, and Employment Quality in China: Evidence from a Quasi-natural Experiment"**

The study examines how China’s New-type Urbanization Pilot Policy (NTUPP) affects employment quality and how its impacts are moderated by two- and three-dimensional urban form.

---

## 📦 Project Structure

```
baseline/          Baseline DID estimations (Stata)
Descriptive/       Descriptive statistics and figures (R + Stata)
heterogenity/      Heterogeneity analysis
interaction/       Moderation and marginal effect analysis
maps/              Spatial distribution maps
robust/            Robustness checks (IV, DML, placebo, parallel trend)
spatial_spill/     Spatial spillover analysis
```

---

## 🔧 Technical Stack

This project uses a mixed empirical workflow:

- **Stata (.do files)**  
  - Baseline DID models  
  - IV-2SLS estimation  
  - Parallel trend tests  
  - Placebo tests  
  - Spatial spillover regressions  

- **R (.R scripts)**  
  - Visualization (violin plots, marginal effects, histograms)  
  - Map generation  
  - DML estimation  
  - Figure production  

Main statistical framework:
- Multi-period Difference-in-Differences (DID)
- Instrumental Variables (IV–2SLS)
- Double Machine Learning (DML)
- Spatial buffer-based spillover modeling

---

## 📊 Data

The repository includes:

- Processed city-level panel datasets (.dta)
- Figure-generation scripts
- Spatial spillover analysis scripts

Due to licensing restrictions, raw statistical yearbook data (e.g., WIND, China City Statistical Yearbook) are not redistributed.

Building height indicators (BSF and MBH) are derived from:

Yang et al. (2024), CBHD30 – China Building Height Dataset (30 m resolution).

---

## ▶️ Reproducibility

To reproduce the main results:

1. Run baseline estimations in `baseline/code_1108.do`
2. Run robustness checks in `robust/`
3. Generate figures using corresponding R scripts
4. Spatial spillover results are in `spatial_spill/`

All empirical results reported in the manuscript can be reproduced using the provided scripts.

---

## 📌 Notes

- Tested in Stata 17+
- Tested in R (≥ 4.2)
- Required R packages include: `ggplot2`, `dplyr`, `sf`, `fixest`, `did`, `randomForest`, `xgboost`

---

## 📬 Contact

Prof. Wufan Zhao  
Urban Governance and Design Thrust  
The Hong Kong University of Science and Technology (Guangzhou)  
Email: wufanzhao@hkust-gz.edu.cn
