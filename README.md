# Quantifying Climate Trends and Human Occupation Patterns in the Middle Stone Age of KwaZulu-Natal, South Africa

This repository contains the R scripts and analytical pipelines developed for my Master's thesis investigating how Middle Stone Age (MSA) communities in KwaZulu-Natal responded to climate and environmental change between 50 and 20 ka.

## Project Overview
The primary aim of this research is to explore two contrasting evolutionary models—the Variability Selection Hypothesis (VSH) and demographic stability models—to assess whether prehistoric occupation patterns and technological shifts correlated with climate stress or were facilitated by the security of ecological refugia. By moving away from broad continental averages, this project integrates finer-resolution, localized palaeoclimatic conditions with chronological occupation estimates.

## Analytical Pipeline
The scripts in this repository execute the following geoarchaeological workflow:
1. **Data Curation:** Synthesizing radiocarbon (14C) and optically stimulated luminescence (OSL) datasets alongside documented technological shifts across five key MSA sites (Border Cave, Holley Shelter, Sibudu Cave, Umbeli Belli, and Umhlatuzana Rock Shelter).
2. **Climate Modelling:** Generating continuous, localized climate and net primary productivity (NPP) reconstructions from downscaled General Circulation Models (GCMs) using the `pastclim` package.
3. **Rate of Change (RoC) Analysis:** Quantifying the tempo and magnitude of climatic transitions to separate slow, gradual ecological shifts from abrupt climate changes.
4. **Demographic Modelling:** Constructing continuous demographic models using Summed Probability Distributions (SPDs) and Kernel Density Estimates (KDEs), incorporating Surovell's taphonomic corrections to mitigate preservation bias.
5. **Principal Component Analysis (PCA):** Reducing high-dimension bioclimatic variables to identify the primary drivers of environmental variance.
6. **Statistical Integration:** Applying Spearman’s rank correlation to investigate whether fluctuations in occupational intensity align with phases of ecological stability or acute environmental stress.

## Dependencies
This workflow relies on the following R packages:
* `tidyverse` (Data manipulation and visualization)
* `pastclim` (Palaeoclimate data extraction)
* `rcarbon` (Radiocarbon calibration, taphonomic correction, and SPD generation)
* `Bchron` (Bayesian OSL density modelling)
* `ggplot2` / `patchwork` (High-fidelity data visualization)
* `writexl` (Data export)
