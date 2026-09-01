library(dplyr)
library(ggplot2)

# Read the combined SPD file generated in Step 03
spd_data <- read.csv("outputs/AllSites_C14_SPD_Combined.csv")

normalized_spd <- spd_data %>%
  group_by(Site) %>%
  mutate(
    calBP = abs(calBP),
    Norm_SPD = (PrDens - min(PrDens, na.rm = TRUE)) / 
      (max(PrDens, na.rm = TRUE) - min(PrDens, na.rm = TRUE))
  ) %library(dplyr)
library(ggplot2)

files <- c(
  "Border Cave"    = "outputs/Border_C14_SPD.csv",
  "Holley Shelter" = "outputs/Holley_C14_SPD.csv",
  "Sibudu Cave"    = "outputs/Sibudu_C14_SPD.csv",
  "Umbeli Belli"   = "outputs/Umbeli_Belli_C14_SPD.csv",
  "Umhlatuzana"    = "outputs/Umhlatuzana_C14_SPD.csv"
)

spd_list <- lapply(names(files), function(site_name) {
  df <- read.csv(files[[site_name]])
  df$Site <- site_name
  return(df)
})

spd_data <- bind_rows(spd_list)

normalized_spd <- spd_data %>%
  group_by(Site) %>%
  mutate(
    calBP = abs(calBP),
    Norm_SPD = (PrDens - min(PrDens, na.rm = TRUE)) / (max(PrDens, na.rm = TRUE) - min(PrDens, na.rm = TRUE))
  ) %>%
  ungroup()

site_colors <- c(
  "Border Cave" = "#0072B2", "Holley Shelter" = "#D55E00", 
  "Sibudu Cave" = "#009E73", "Umbeli Belli" = "#CC79A7", "Umhlatuzana" = "#F0E442"
)

regional_plot <- ggplot(normalized_spd, aes(x = calBP, y = Norm_SPD, color = Site)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = site_colors) +
  scale_x_reverse(limits = c(50000, 20000), breaks = seq(50000, 20000, by = -5000)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  labs(
    title = "Normalized Regional Demographic Trends (50–20 ka)",
    x = "Years Before Present (ka)",
    y = "SPD/KDE (%)"
  )

print(regional_plot)
ggsave("outputs/Regional_SPDs.png", plot = regional_plot, width = 8, height = 5, dpi = 300)>%
  ungroup()

site_colors <- c(
  "Border Cave" = "#0072B2",         
  "Holley Shelter" = "#D55E00",      
  "Sibudu Cave" = "#009E73",         
  "Umbeli Belli" = "#CC79A7",        
  "Umhlatuzana" = "#F0E442"          
)

regional_plot <- ggplot(normalized_spd, aes(x = calBP, y = Norm_SPD, color = Site)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = site_colors) +
  scale_x_reverse(limits = c(50000, 20000), breaks = seq(50000, 20000, by = -5000)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  ) +
  labs(
    title = "Normalized Regional Demographic Trends (50–20 ka)",
    x = "Years Before Present (ka)",
    y = "SPD/KDE (%)"
  )

print(regional_plot)
ggsave("outputs/Regional_SPDs.png", plot = regional_plot, width = 8, height = 5, dpi = 300)