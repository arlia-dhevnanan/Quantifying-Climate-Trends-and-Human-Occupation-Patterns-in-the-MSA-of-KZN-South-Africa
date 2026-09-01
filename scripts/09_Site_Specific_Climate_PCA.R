library(pastclim)
library(tidyverse)
library(factoextra)
library(patchwork)
library(writexl)

archae_sites <- tibble(
  name      = c("Holley Shelter", "Umbeli Belli", "Sibudu Cave", "Umhlatuzana", "Border Cave"),
  longitude = c(30.539, 30.686, 31.086, 30.754, 31.989),
  latitude  = c(-29.464, -30.311, -29.5231, -29.81, -27.021)
)

bio_vars <- c("bio01", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "npp")

beyer_steps <- available_steps[available_steps >= -50000 & available_steps <= -20000]

pca_raw_archae <- map_dfr(split(archae_sites, archae_sites$name), function(site) {
  location_series(x = site, bio_variables = bio_vars, dataset = "Beyer2020", time_bp = beyer_steps, nn_interpol = FALSE)
})

site_names <- unique(pca_raw_archae$name)
pca_results_by_site <- list()
plot_list           <- list()
tables_by_site      <- list()

for (site in site_names) {
  site_data <- pca_raw_archae %>% filter(name == site) %>% select(all_of(bio_vars)) %>% drop_na()
  variances <- apply(site_data, 2, var)
  site_data_clean <- site_data[, variances > 0]
  
  pca_res <- prcomp(site_data_clean, center = TRUE, scale. = TRUE)
  pca_results_by_site[[site]] <- pca_res
  
  plot_list[[site]] <- fviz_pca_var(pca_res, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, title = paste("Drivers:", site)) + theme_minimal(base_size = 11, base_family = "serif") + theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold"))
  
  vars_info <- get_pca_var(pca_res)
  site_table <- as.data.frame(round(vars_info$contrib, 2))
  site_table$Variable <- rownames(site_table)
  tables_by_site[[site]] <- site_table %>% select(Variable, Dim.1, Dim.2) %>% arrange(desc(Dim.1))
}

combined_pca_grid <- wrap_plots(plot_list, ncol = 3)
print(combined_pca_grid)

write_xlsx(tables_by_site, "outputs/PCA_Values_By_Site.xlsx")

grapher_sheets <- list()
for (site in site_names) {
  site_pca <- pca_results_by_site[[site]]
  site_data_original <- pca_raw_archae %>% filter(name == site) %>% drop_na(all_of(bio_vars))
  
  site_scores <- as.data.frame(site_pca$x[, c("PC1", "PC2")]) 
  site_scores$Time_BP <- site_data_original$time_bp 
  
  site_vars_info <- get_pca_var(site_pca)
  site_loadings  <- as.data.frame(site_vars_info$coord[, c("Dim.1", "Dim.2")]) * 5
  site_loadings$Variable <- rownames(site_loadings)
  site_loadings$Origin_X <- 0  
  site_loadings$Origin_Y <- 0
  
  short_site <- gsub(" Cave| Shelter", "", site)
  grapher_sheets[[paste0(short_site, "_Scores")]]   <- site_scores
  grapher_sheets[[paste0(short_site, "_Loadings")]] <- site_loadings
}

write_xlsx(grapher_sheets, path = "outputs/AllSites_Grapher_Biplot_Data.xlsx")