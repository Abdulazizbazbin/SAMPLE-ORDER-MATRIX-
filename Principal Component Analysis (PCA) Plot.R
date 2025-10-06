# =============================
# PCA Analysis and Publication-Quality Plot
# =============================

library(tidyverse)
library(ggrepel)

# --- Step 1: Load abundance data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix_copy.csv"
df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Extract abundance data (exclude sample.name and pore_size) ---
abundance_df <- df %>% select(-sample.name, -pore_size)

# Remove constant columns (all zeros or same value)
abundance_df <- abundance_df[, apply(abundance_df, 2, function(x) sd(x) != 0)]

# --- Step 3: PCA ---
pca_res <- prcomp(abundance_df, scale. = TRUE)

# --- Step 4: Prepare PCA scores for plotting ---
pca_scores <- as.data.frame(pca_res$x)
pca_df <- cbind(df %>% select(sample.name, pore_size), pca_scores)

# --- Step 5: Clean and order pore_size ---
pca_df$pore_size <- str_trim(pca_df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
pca_df$pore_size <- factor(pca_df$pore_size, levels = pore_levels)

# --- Step 6: Colors for pore sizes ---
pub_colors <- c("purple", "green", "red", "blue", "yellow")

# --- Step 7: Variance percentages for axis labels ---
var_pc1 <- round((pca_res$sdev[1]^2 / sum(pca_res$sdev^2)) * 100, 1)
var_pc2 <- round((pca_res$sdev[2]^2 / sum(pca_res$sdev^2)) * 100, 1)

# --- Step 8: PCA plot ---
ggplot(pca_df, aes(x = PC1, y = PC2, color = pore_size)) +
  geom_point(size = 4, alpha = 0.8) +
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
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
