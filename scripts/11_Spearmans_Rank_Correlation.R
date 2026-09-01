library(dplyr)
library(zoo)
library(purrr)

roc <- read.csv("outputs/RoC.csv") %>% mutate(Site = case_when(Site == "Border" ~ "Border Cave", Site == "Holley" ~ "Holley Shelter", Site == "Sibudu" ~ "Sibudu Cave", Site == "Umbeli" ~ "Umbeli Belli", TRUE ~ Site))

spd_clean <- read.csv("outputs/AllSites_C14_SPD_Combined.csv") %>%
  mutate(Site = case_when(grepl("Sibudu", Site, ignore.case = TRUE) ~ "Sibudu Cave", grepl("Border", Site, ignore.case = TRUE) ~ "Border Cave", grepl("Holley", Site, ignore.case = TRUE) ~ "Holley Shelter", grepl("Umbeli", Site, ignore.case = TRUE) ~ "Umbeli Belli", grepl("Umhlatuzana", Site, ignore.case = TRUE) ~ "Umhlatuzana", TRUE ~ Site)) %>%
  mutate(time_bp = round(-calBP / 2000) * 2000) %>% group_by(Site, time_bp) %>% summarise(PrDens_C14 = mean(PrDens_Taphonomic, na.rm = TRUE), .groups = 'drop')

osl_clean <- read.csv("outputs/AllSites_OSL_KDE_Combined.csv") %>%
  mutate(Site = case_when(grepl("Sibudu", Site, ignore.case = TRUE) ~ "Sibudu Cave", grepl("Border", Site, ignore.case = TRUE) ~ "Border Cave", grepl("Holley", Site, ignore.case = TRUE) ~ "Holley Shelter", grepl("Umbeli", Site, ignore.case = TRUE) ~ "Umbeli Belli", grepl("Umhlatuzana", Site, ignore.case = TRUE) ~ "Umhlatuzana", TRUE ~ Site)) %>%
  mutate(time_bp = round(-Age / 2000) * 2000) %>% group_by(Site, time_bp) %>% summarise(PrDens_OSL = mean(Density, na.rm = TRUE), .groups = 'drop')

final_data <<- roc %>% left_join(spd_clean, by = c("Site", "time_bp")) %>% left_join(osl_clean, by = c("Site", "time_bp")) %>% arrange(Site, time_bp)

site_vars_list <- list(
  "Border Cave" = c("bio01", "bio12", "npp"), "Holley Shelter" = c("bio01", "bio12", "npp"), "Sibudu Cave" = c("bio01", "bio12", "npp"), "Umbeli Belli" = c("bio01", "bio12", "npp"), "Umhlatuzana" = c("bio01", "bio12", "npp")
)

run_correlation_tests <- function(site_name, site_df, target_var, density_col, method_label) {
  clim_vec <- site_df[[target_var]]
  dens_vec <- site_df[[density_col]]
  if (sum(!is.na(clim_vec)) <= 5 || sum(!is.na(dens_vec)) <= 5) return(NULL)
  
  res_sp <- cor.test(clim_vec, dens_vec, method = "spearman", exact = FALSE)
  ts_clim <- na.approx(clim_vec, na.rm = FALSE, rule = 2)
  ts_dens <- na.approx(dens_vec, na.rm = FALSE, rule = 2)
  
  ccf_res <- ccf(ts_clim, ts_dens, lag.max = 5, plot = FALSE, na.action = na.pass)
  max_idx <- which.max(abs(ccf_res$acf))
  
  tibble(Site = site_name, Variable = target_var, Dating_Method = method_label, Spearman_Rho = as.numeric(res_sp$estimate), P_Value = as.numeric(res_sp$p.value), Max_CCF = as.numeric(ccf_res$acf[max_idx]), Lag_Bins = as.numeric(ccf_res$lag[max_idx]))
}

final_results <- map_dfr(unique(final_data$Site), function(s) {
  site_subset <- filter(final_data, Site == s)
  climate_vars <- site_vars_list[[s]]
  if (is.null(climate_vars)) return(NULL)
  
  map_dfr(climate_vars, function(v) {
    if (!v %in% colnames(site_subset)) return(NULL)
    bind_rows(run_correlation_tests(s, site_subset, v, "PrDens_C14", "C14_SPD"), run_correlation_tests(s, site_subset, v, "PrDens_OSL", "OSL_KDE"))
  })
})

output_path <- "outputs/Spearmans_Rank.csv"
write.csv(final_results, output_path, row.names = FALSE)