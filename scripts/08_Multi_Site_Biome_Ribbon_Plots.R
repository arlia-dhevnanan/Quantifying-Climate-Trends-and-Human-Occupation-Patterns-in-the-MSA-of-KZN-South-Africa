library(dplyr)
library(ggplot2)

biome_data <- read.csv("outputs/biome_data.csv")

sites_list <- c("Border Cave", "Holley Shelter", "Sibudu Cave", "Umbeli Belli", "Umhlatuzana")

biome_colors <- c(
  "Temperate conifer forest"       = "#0072B2",
  "Temperate sclerophyll woodland" = "#D55E00",
  "Warm mixed forest"              = "#F0E442",
  "Temperate deciduous forest"     = "#009E73",
  "Savanna"                        = "#CC79A7" 
)

for (site_name in sites_list) {
  site_biome <- biome_data %>%
    filter(Site == site_name) %>%
    mutate(Start_Year = abs(Start_Year), End_Year = abs(End_Year))
  
  biome_plot <- ggplot(site_biome) +
    geom_rect(aes(xmin = End_Year, xmax = Start_Year, ymin = 0, ymax = 1, fill = Biome), color = NA) +
    scale_fill_manual(values = biome_colors, drop = FALSE) +
    scale_x_reverse(limits = c(50000, 20000), breaks = seq(50000, 20000, by = -5000)) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      panel.grid = element_blank(), legend.position = "bottom", legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(x = "Years Before Present (ka)", title = paste(site_name, ": Biome Transitions"))
  
  print(biome_plot)
  file_name <- file.path("outputs", paste0(gsub(" ", "_", site_name), "_Biome_Ribbon.png"))
  ggsave(file_name, plot = biome_plot, width = 10, height = 2, dpi = 300)
}