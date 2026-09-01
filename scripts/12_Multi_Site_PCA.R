library(tidyverse)
library(ggfortify)

# Relies on `final_data` generated globally in 11_Spearmans_Rank_Correlation.R
pca_df <- final_data %>% drop_na(bio01, bio12, npp)

climate_matrix <- pca_df %>% select(bio01, bio12, npp)
pca_res <- prcomp(climate_matrix, scale. = TRUE)

combined_pca_plot <- autoplot(
  pca_res, data = pca_df, colour = "Site", loadings = TRUE, loadings.label = TRUE, loadings.label.color = "black", frame = TRUE, frame.type = "norm"
) +
  scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
  labs(title = "Multi-Site Climate PCA Reconstructions", color = "Site", fill = "Site") +
  theme_minimal(base_size = 12, base_family = "serif") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), axis.title = element_text(face = "bold"), legend.title = element_text(face = "bold"), panel.grid.minor = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

print(combined_pca_plot)