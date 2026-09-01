library(Bchron)
library(tidyverse)
library(writexl)

base_dir <- "data/"

sites_config <- list(
  SibuduCave = list(file = file.path(base_dir, "SC_OSL_Cleaned.csv"), prefix = "Sibudu", title = "Sibudu Cave: OSL Phase Density", color = "#0072B2"),
  UmbeliBelli = list(file = file.path(base_dir, "UB_OSL_Cleaned.csv"), prefix = "UmbeliBelli", title = "Umbeli Belli: OSL Phase Density", color = "#D55E00"),
  Umhlatuzana = list(file = file.path(base_dir, "UMH_OSL_Cleaned.csv"), prefix = "Umhlatuzana", title = "Umhlatuzana: Late/Final MSA OSL Phase Density", color = "#009E73")
)

run_osl_kde <- function(site_key, config) {
  message(paste("\nRunning OSL Bchron Density Model for:", site_key))
  if (!file.exists(config$file)) {
    warning(paste("File missing:", config$file))
    return(NULL)
  }
  
  raw_data <- read.csv(config$file, stringsAsFactors = FALSE, check.names = FALSE)
  names(raw_data) <- make.unique(names(raw_data))
  
  age_col   <- names(raw_data)[str_detect(names(raw_data), "(?i)age")]
  error_col <- names(raw_data)[str_detect(names(raw_data), "(?i)error")]
  
  dates_clean <- raw_data %>%
    filter(!is.na(.data[[age_col[1]]])) %>%
    mutate(
      Age_BP   = as.numeric(.data[[age_col[1]]]) * 1000,
      Error_BP = as.numeric(.data[[error_col[1]]]) * 1000
    ) %>%
    filter(!is.na(Age_BP), !is.na(Error_BP))
  
  if (nrow(dates_clean) == 0) return(NULL)
  
  density_model <- BchronDensity(
    ages      = dates_clean$Age_BP,
    ageSds    = dates_clean$Error_BP,
    calCurves = rep("normal", nrow(dates_clean))
  )
  
  plot_df <- tibble(
    Age     = density_model$ageGrid,
    Density = density_model$densities,
    Site    = site_key
  )
  
  excel_file <- file.path("outputs", paste0(config$prefix, "_OSL_KDE.xlsx"))
  write_xlsx(plot_df, excel_file)
  
  max_age <- ceiling(max(plot_df$Age) / 5000) * 5000
  min_age <- floor(min(plot_df$Age) / 5000) * 5000
  
  p <- ggplot(plot_df, aes(x = Age, y = Density)) +
    geom_area(fill = config$color, color = config$color, alpha = 0.6) +
    scale_x_reverse(limits = c(max_age, min_age), breaks = seq(min_age, max_age, by = 5000), labels = scales::label_comma()) +
    labs(title = config$title, subtitle = "Bayesian Summed Probability Density (OSL)", x = "Years Before Present (BP)", y = "Probability Density") +
    theme_minimal(base_size = 12, base_family = "serif") +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, color = "grey30"), axis.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  
  print(p)
  return(plot_df)
}

kde_results <- imap(sites_config, ~ run_osl_kde(.y, .x))
combined_kde_df <- bind_rows(compact(kde_results))
write.csv(combined_kde_df, "outputs/AllSites_OSL_KDE_Combined.csv", row.names = FALSE)