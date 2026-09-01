library(tidyverse)
library(readxl)

base_dir <- "data/"

sites_config <- list(
  SibuduCave  = list(file = "Sibudu Cave.xlsx",  prefix = "SC"),
  UmbeliBelli = list(file = "Umbeli Belli.xlsx", prefix = "UB"),
  Umhlatuzana = list(file = "Umhlatuzana.xlsx", prefix = "UMH")
)

clean_and_export_site <- function(site_name, file_name, prefix, base_path) {
  excel_path <- file.path(base_path, file_name)
  
  if (!file.exists(excel_path)) {
    warning(paste("File not found:", excel_path))
    return(NULL)
  }
  
  available_sheets <- excel_sheets(excel_path)
  
  if ("OSL" %in% available_sheets) {
    raw_osl <- read_excel(excel_path, sheet = "OSL")
    osl_clean <- raw_osl %>%
      filter(!is.na(Layer), !is.na(`Age (ka)`)) %>%
      distinct()
    write_csv(osl_clean, file.path(base_path, paste0(prefix, "_OSL_Cleaned.csv")))
    cat(sprintf("OSL Data:       %d original rows -> %d cleaned rows\n", nrow(raw_osl), nrow(osl_clean)))
  }
  
  if ("C14" %in% available_sheets) {
    raw_c14 <- read_excel(excel_path, sheet = "C14")
    c14_clean <- raw_c14 %>%
      filter(!is.na(Layer), !is.na(`Age BP`)) %>%
      distinct()
    write_csv(c14_clean, file.path(base_path, paste0(prefix, "_C14_Cleaned.csv")))
    cat(sprintf("C14 Data:       %d original rows -> %d cleaned rows\n", nrow(raw_c14), nrow(c14_clean)))
  }
  
  if ("Artifacts" %in% available_sheets) {
    raw_artifacts <- read_excel(excel_path, sheet = "Artifacts")
    artifacts_clean <- raw_artifacts %>%
      filter(!is.na(`Artifact Type`), !is.na(`Artifact Volume`)) %>%
      distinct()
    write_csv(artifacts_clean, file.path(base_path, paste0(prefix, "_Artifacts_Cleaned.csv")))
    cat(sprintf("Artifact Data:  %d original rows -> %d cleaned rows\n", nrow(raw_artifacts), nrow(artifacts_clean)))
  }
}

pwalk(
  list(names(sites_config), map(sites_config, "file"), map(sites_config, "prefix")),
  ~ clean_and_export_site(site_name = ..1, file_name = ..2, prefix = ..3, base_path = base_dir)
)
cat("\nAll site datasets cleaned and saved successfully!\n")