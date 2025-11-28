library(tidyverse)
library(ggrepel)

file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_68_sample_order_matrix.csv" 
df <- read.csv(file_path, check.names = FALSE)

abundance_df <- df %>% select(-sample.name, -pore_size)
abundance_df <- abundance_df[, apply(abundance_df, 2, function(x) sd(x) != 0)]

pca_res <- prcomp(abundance_df, scale. = TRUE)

pca_scores <- as.data.frame(pca_res$x)
pca_df <- cbind(df %>% select(sample.name, pore_size), pca_scores)

pca_df$pore_size <- str_trim(pca_df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
pca_df$pore_size <- factor(pca_df$pore_size, levels = pore_levels)

pub_colors <- c("purple", "green", "red", "blue", "yellow")

var_pc1 <- round((pca_res$sdev[1]^2 / sum(pca_res$sdev^2)) * 100, 1)
var_pc2 <- round((pca_res$sdev[2]^2 / sum(pca_res$sdev^2)) * 100, 1)

ggplot(pca_df, aes(x = PC1, y = PC2)) +
  # --- Ellipses by pore size ---
  stat_ellipse(
    aes(color = pore_size, group = pore_size),
    type  = "norm",      # multivariate normal ellipse
    level = 0.95,        # 95% CI
    linewidth = 1
  ) +
  # --- Points ---
  geom_point(aes(color = pore_size), size = 4, alpha = 0.8) +
  scale_color_manual(values = pub_colors) +
  labs(
    x = paste0("Principal Component 1 (", var_pc1, "%)"),
    y = paste0("Principal Component 2 (", var_pc2, "%)"),
    color = "Pore Size (μm)"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    legend.position = "right",
    axis.title = element_text(face = "bold", size = 14),
    axis.text  = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks   = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
