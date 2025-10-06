library(tidyverse)

# --- Load your abundance data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix_copy.csv"
df <- read.csv(file_path, check.names = FALSE)

# --- Convert to long format for plotting ---
abundance_long <- df %>%
  pivot_longer(
    cols = Belfryvirales:novel_order, 
    names_to = "vOTU",
    values_to = "abundance"
  ) %>%
  group_by(sample.name) %>%
  mutate(rel_abundance = abundance / sum(abundance)) %>%
  ungroup()

# --- Clean and order pore_size ---
abundance_long$pore_size <- str_trim(abundance_long$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
abundance_long$pore_size <- factor(abundance_long$pore_size, levels = pore_levels)

# --- Colors for pore sizes ---
pub_colors <- c("purple", "green", "red", "blue", "yellow")

# --- Plot relative abundance by vOTU ---
ggplot(abundance_long, aes(x = vOTU, y = rel_abundance, color = pore_size)) +
  geom_point(size = 3, alpha = 0.8, position = position_jitter(width = 0.2, height = 0)) +
  scale_color_manual(values = pub_colors) +
  labs(
    x = "Viral vOTUs (Morphological Groups)",
    y = "Relative Abundance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # remove legend
  )
