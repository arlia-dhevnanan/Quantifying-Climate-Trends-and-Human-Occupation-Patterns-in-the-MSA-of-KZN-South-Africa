library(tidyverse)
library(writexl)

fix_messy_headers <- function(df, expected_cols) {
  current_names <- names(df)
  current_names[is.na(current_names) | current_names == ""] <- paste0("temp_col_", seq_along(current_names))
  n <- min(length(expected_cols), length(current_names))
  current_names[1:n] <- expected_cols[1:n]
  names(df) <- make.unique(current_names)
  return(df)
}

extract_mean_age <- function(age_input) {
  sapply(age_input, function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    numbers_found <- as.numeric(str_extract_all(x, "[0-9]+\\.?[0-9]*")[[1]])
    if (length(numbers_found) > 0) mean(numbers_found, na.rm = TRUE) else NA_real_
  }, USE.NAMES = FALSE)
}

process_site_data <- function(file_path, site_name, convert_to_bp = TRUE, drop_mixed_layers = FALSE) {
  cat("--> Working on site:", site_name, "\n")
  raw_data <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
  cleaned_df <- fix_messy_headers(raw_data, c("Artifact_Type", "Artifact_Volume", "Age_ka", "Error", "Reference"))
  
  valid_rows <- cleaned_df %>% filter(!is.na(Age_ka), Age_ka != "")
  
  processed_df <- valid_rows %>%
    filter(!grepl("Multiple|centuries|N/A", Age_ka, ignore.case = TRUE)) %>%
    mutate(
      vol_clean = as.numeric(str_remove_all(Artifact_Volume, "[^0-9.]")),
      age_string_clean = str_remove_all(Age_ka, "(?i)phase 9|[a-zA-Z~()><\\s]"),
      age_numeric = extract_mean_age(age_string_clean),
      age_bp = if (convert_to_bp) age_numeric * 1000 else age_numeric
    ) %>%
    filter(!is.na(vol_clean), !is.na(age_bp))
  
  site_summary <- processed_df %>%
    group_by(Age_BP = age_bp) %>%
    summarise(Total_Volume = sum(vol_clean, na.rm = TRUE), .groups = "drop") %>%
    arrange(Age_BP)
  
  output_filename <- file.path("outputs", paste0(site_name, "_Artifact_Volumes.xlsx"))
  write_xlsx(site_summary, output_filename)
  return(site_summary)
}

base_dir <- "data/"

site_configs <- list(
  BorderCave    = list(path = file.path(base_dir, "BC_Artifacts_Cleaned.csv"), scale = FALSE, drop_mixed = TRUE),
  HolleyShelter = list(path = file.path(base_dir, "HS_Artifacts_Cleaned.csv"), scale = TRUE,  drop_mixed = FALSE),
  SibuduCave    = list(path = file.path(base_dir, "SC_Artifacts_Cleaned.csv"), scale = FALSE, drop_mixed = FALSE),
  UmbeliBelli   = list(path = file.path(base_dir, "UB_Artifacts_Cleaned.csv"), scale = TRUE,  drop_mixed = FALSE),
  Umhlatuzana   = list(path = file.path(base_dir, "UMH_Artifacts_Cleaned.csv"), scale = TRUE, drop_mixed = FALSE)
)

all_results <- imap(site_configs, function(cfg, name) {
  process_site_data(
    file_path         = cfg$path,
    site_name         = name,
    convert_to_bp     = cfg$scale,
    drop_mixed_layers = cfg$drop_mixed
  )
})
cat("\nAll site chronologies successfully processed and saved to outputs/!\n")