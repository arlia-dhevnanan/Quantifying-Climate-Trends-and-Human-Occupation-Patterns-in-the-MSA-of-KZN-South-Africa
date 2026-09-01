library(rcarbon)
library(dplyr)
library(writexl)
library(purrr)

site_files <- c(
  "Border Cave"    = "data/BC_C14_Cleaned.csv",
  "Holley Shelter" = "data/HS_C14_Cleaned.csv",
  "Sibudu Cave"    = "data/SC_C14_Cleaned.csv",
  "Umbeli Belli"   = "data/UB_C14_Cleaned.csv",
  "Umhlatuzana"    = "data/UMH_C14_Cleaned.csv"
)

process_site_spd <- function(site_name, file_path) {
  cat(paste0("\n--- Processing Site: ", toupper(site_name), " ---\n"))
  
  if (!file.exists(file_path)) {
    warning(paste("File missing for", site_name, "at path:", file_path))
    return(NULL)
  }
  
  c14_raw <- read.csv(file_path, stringsAsFactors = FALSE)
  c14_clean <- c14_raw %>% filter(!is.na(Age.BP) & !is.na(Error))
  
  if (nrow(c14_clean) == 0) return(NULL)
  
  calibrated_dates <- calibrate(
    x         = c14_clean$Age.BP, 
    errors    = c14_clean$Error, 
    calCurves = 'shcal20',
    verbose   = FALSE
  )
  
  c14_clean$Site_ID <- site_name
  site_bins <- binPrep(sites = c14_clean$Site_ID, ages = c14_clean$Age.BP, h = 50)
  
  site_spd <- spd(
    x         = calibrated_dates, 
    bins      = site_bins, 
    timeRange = c(50000, 20000),
    verbose   = FALSE
  )
  
  plot_data <- site_spd$grid
  surovell_weights <- 5726000 * ((plot_data$calBP + 2176.4) ^ -1.392)
  plot_data$PrDens_Taphonomic <- plot_data$PrDens / surovell_weights
  plot_data$Site <- site_name
  
  output_csv <- file.path("outputs", paste0(gsub(" Cave| Shelter", "", site_name), "_C14_SPD.csv"))
  write.csv(plot_data, output_csv, row.names = FALSE)
  
  return(plot_data)
}

spd_results_list <- imap(site_files, ~ process_site_spd(.y, .x))
spd_results_list <- compact(spd_results_list)

excel_sheets <- setNames(spd_results_list, gsub(" Cave| Shelter", "", names(spd_results_list)))
write_xlsx(excel_sheets, path = "outputs/AllSites_C14_SPD.xlsx")

combined_spd_df <- bind_rows(spd_results_list)
write.csv(combined_spd_df, "outputs/AllSites_C14_SPD_Combined.csv", row.names = FALSE)