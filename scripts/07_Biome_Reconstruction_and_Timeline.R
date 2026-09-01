library(pastclim)
library(dplyr)
library(ggplot2)
library(writexl)

time_steps <- c(seq(-50000, -22000, by = 2000), -21000, -20000)

sites <- tibble(
  name      = c("Holley Shelter", "Umbeli Belli", "Sibudu Cave", "Umhlatuzana", "Border Cave"),
  longitude = c(30.539, 30.686, 31.086, 30.754, 31.989),
  latitude  = c(-29.464, -30.311, -29.5231, -29.81, -27.021)
)

biome_series <- location_series(
  x             = sites, 
  bio_variables = "biome", 
  dataset       = "Beyer2020", 
  time_bp       = time_steps, 
  nn_interpol   = TRUE
)

plot_data <- biome_series %>%
  rename(Site = name, Start_Year = time_bp, Biome = biome) %>%
  arrange(Site, Start_Year) %>%
  group_by(Site) %>%
  mutate(End_Year = coalesce(lead(Start_Year), Start_Year + 1000)) %>%
  ungroup()

write_xlsx(plot_data, "outputs/biome_data.xlsx")
write.csv(plot_data, "outputs/biome_data.csv", row.names = FALSE)

base_theme <- theme_minimal(base_family = "serif", base_size = 12) +
  theme(
    text = element_text(color = "black"), axis.text = element_text(color = "black"), axis.text.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)), plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), legend.position = "right", legend.title = element_text(face = "bold")
  )

ggplot(plot_data) +
  geom_segment(aes(x = Start_Year, xend = End_Year, y = Site, yend = Site, color = Biome), linewidth = 6) +
  scale_x_continuous(limits = c(-50000, -20000), breaks = seq(-50000, -20000, by = 5000), labels = abs) +
  labs(title = "Biome Shifts (50-20ka BP)", x = "Years Before Present", y = NULL, color = "Biome Type") + base_theme