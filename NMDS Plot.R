# =============================
# Publication-Quality NMDS Plot by Pore Size (Unique Shapes + Colors)
# =============================

library(tidyverse)

# --- Step 1: Load NMDS data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/nmds_scores_per_sample.csv"
nmds_df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Clean and order pore_size ---
nmds_df$pore_size <- str_trim(nmds_df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
nmds_df$pore_size <- factor(nmds_df$pore_size, levels = pore_levels)

# --- Step 3: Define publication-quality colors and shapes ---
pub_colors <- c("purple", "green", "red", "blue", "yellow")
shape_values <- c(17, 16, 15, 18, 8)  
# 17 = triangle, 16 = circle, 15 = square, 18 = diamond, 8 = star

# --- Step 4: NMDS Plot ---
ggplot(nmds_df, aes(x = NMDS1, y = NMDS2, color = pore_size, shape = pore_size)) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = pub_colors) +
  scale_shape_manual(values = shape_values) +
  labs(
    x = "NMDS1",
    y = "NMDS2",
    color = "Pore Size (μm)",
    shape = "Pore Size (μm)"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 12),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
