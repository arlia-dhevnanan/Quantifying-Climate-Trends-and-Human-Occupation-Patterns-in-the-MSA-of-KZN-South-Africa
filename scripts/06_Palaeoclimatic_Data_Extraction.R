library(pastclim)
library(tidyverse)
library(writexl)

archae_sites <- tibble(
  name      = c("Holley Shelter", "Umbeli Belli", "Sibudu Cave", "Umhlatuzana", "Border Cave"),
  longitude = c(30.539, 30.686, 31.086, 30.754, 31.989),
  latitude  = c(-29.464, -30.311, -29.5231, -29.81, -27.021)
)

variables <- c(
  "bio01", "bio04", "bio05", "bio06", "bio07", "bio08", "bio09", "bio10", "bio11",
  "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "npp"
)

beyer_steps <- available_steps[available_steps >= -50000 & available_steps <= 0]

archae_climate_ts <- map_dfr(split(archae_sites, archae_sites$name), function(site) {
  location_series(x = site, bio_variables = variables, dataset = "Beyer2020", time_bp = beyer_steps, nn_interpol = FALSE)
})

site_short_names <- c(
  "Holley Shelter" = "Holley", "Umbeli Belli" = "Umbeli", "Sibudu Cave" = "Sibudu", "Umhlatuzana" = "Umhlatuzana", "Border Cave" = "Border"
)

graphing_data <- archae_climate_ts %>%
  mutate(name = recode(name, !!!site_short_names)) %>%
  pivot_wider(names_from = name, values_from = all_of(variables), names_glue = "{name}_{.value}") %>%
  arrange(time_bp)

write_xlsx(graphing_data, "outputs/Pastclim.xlsx")

pub_theme <- theme_minimal(base_size = 12, base_family = "serif") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), axis.title = element_text(face = "bold"), legend.position = "bottom", legend.title = element_text(face = "bold"), panel.grid.minor = element_blank())

for (var in variables) {
  var_label <- var_labels(var, dataset = "Beyer2020", abbreviated = TRUE)
  p <- ggplot(archae_climate_ts, aes(x = time_bp, y = .data[[var]], color = name, group = name)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) + geom_point(size = 1.5, na.rm = TRUE) +
    scale_x_reverse(labels = scales::comma_format(transform = "abs"), breaks = seq(-50000, 0, by = 10000)) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = paste0("Multi-Site Reconstruction: ", toupper(var)), x = "Years Before Present (BP)", y = var_label, color = "Site") + pub_theme
  print(p)
}