# Late Pleistocene Human-Environment Dynamics in KwaZulu-Natal (50-20 ka)

This repository contains the R scripts and analytical pipelines developed for my Master's thesis investigating Middle Stone Age (MSA) human behavioural adaptation and demographic shifts in KwaZulu-Natal, South Africa.

## Project Overview
The analysis integrates mechanistic General Circulation Models (GCMs) with archaeological chronologies to test whether MSA technological transitions were driven by acute environmental stress or facilitated by ecological stability in coastal and altitudinal refugia.

## Analytical Pipeline
The scripts in this repository execute the following geoarchaeological workflow:
1. **Data Curation:** Cleaning and standardizing macroscopic artefact volumes and chronometric datasets.
2. **Chronological Modelling:** Generating Summed Probability Distributions (SPDs) for radiocarbon datasets (with taphonomic corrections) and Bayesian Kernel Density Estimates (KDEs) for OSL dates.
3. **Palaeoclimatic Extraction:** Utilizing the `pastclim` package to extract 17 localized bioclimatic variables, NPP, and BIOME4 classifications from downscaled global models.
4. **Statistical Analysis:** Running Principal Component Analysis (PCA) to identify local environmental drivers and Rate of Change (RoC) heatmaps to quantify climate velocity.
5. **Integration:** Applying Spearman's rank correlation to cross-reference occupational intensity against environmental baselines.

## Dependencies
This workflow relies on the following R packages:
* `tidyverse` (Data manipulation and visualization)
* `pastclim` (Palaeoclimate data extraction)
* `rcarbon` (Radiocarbon calibration and SPD generation)
* `Bchron` (Bayesian OSL density modelling)
* `ggplot2` / `patchwork` (High-fidelity data visualization)
* `writexl` / `readxl` (Data import/export)
