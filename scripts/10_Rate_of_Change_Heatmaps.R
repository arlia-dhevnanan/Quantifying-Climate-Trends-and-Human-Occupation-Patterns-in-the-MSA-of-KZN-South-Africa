library(tidyverse)

clean_file <- "data/Manual_RoC.csv"
roc_file   <- "outputs/RoC.csv"

df_clean <- read.csv(clean_file)

df_roc <- df_clean %>%
  filter(time_bp %% 2000 == 0) %>%
  group_by(Site) %>%
  arrange(time_bp, .by_group = TRUE) %>%
  mutate(time_elapsed = time_bp - lag(time_bp)) %>%
  mutate(across(.cols = c(matches("^bio[0-9]+$"), "npp"), .fns = list(change = \(x) x - lag(x), RoC_century = \(x) ((x - lag(x)) / time_elapsed) * 100), .names = "{.col}_{.fn}")) %>%
  ungroup()

write.csv(df_roc, roc_file, row.names = FALSE)

df <- read.csv(roc_file)
proper_names <- c("bio01_RoC_century" = "Mean Annual Temperature", "bio12_RoC_century" = "Annual Precipitation", "npp_RoC_century" = "Net Primary Productivity") # Add full list from original code as needed

make_heatmap <- function(target_variable) {
  clean_title <- proper_names[[target_variable]] %||% target_variable
  df_heatmap <- df %>% filter(time_bp > -50000 & time_bp <= -20000) %>% filter(!is.na(.data[[target_variable]])) %>% mutate(abs_change = abs(.data[[target_variable]]), norm_change = (abs_change - min(abs_change, na.rm = TRUE)) / (max(abs_change, na.rm = TRUE) - min(abs_change, na.rm = TRUE)))
  
  alpha_order <- c("Border Cave", "Holley Shelter", "Sibudu Cave", "Umbeli Belli", "Umhlatuzana")
  df_heatmap$Site <- factor(df_heatmap$Site, levels = rev(alpha_order))
  
  ggplot(df_heatmap, aes(x = time_bp - 1000, y = Site, fill = norm_change)) +
    geom_tile(color = "white", linewidth = 0.2, width = 2000) + scale_fill_distiller(palette = "Reds", direction = 1, na.value = "white") +
    coord_cartesian(xlim = c(-50000, -20000), expand = FALSE) + scale_x_continuous(name = "Years Before Present", breaks = seq(-50000, -20000, by = 5000), labels = abs) +
    labs(title = clean_title, y = NULL, fill = "Relative Rate") + theme_minimal(base_size = 12, base_family = "serif")
}

target_vars <- c("npp_RoC_century", "bio01_RoC_century", "bio12_RoC_century") # Truncated list for brevity, expand to all bio targets
walk(target_vars, ~ print(make_heatmap(.x)))