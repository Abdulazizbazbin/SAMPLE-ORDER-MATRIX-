# =============================
# Publication-Quality Boxplot of Shannon Evenness by Pore Size (with border and ticks)
# =============================

library(tidyverse)

# --- Step 1: Load Shannon Evenness data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/shannon_evenness_per_sample.csv"
shannon_df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Clean pore_size column ---
shannon_df$pore_size <- str_trim(shannon_df$pore_size)

# --- Step 3: Convert pore_size to factor with desired order (smallest to largest) ---
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
shannon_df$pore_size <- factor(shannon_df$pore_size, levels = pore_levels)

# --- Step 4: Define publication-quality colors ---
pub_colors <- c("yellow", "blue", "red", "green", "purple")

# --- Step 5: Create boxplot with border and ticks ---
ggplot(shannon_df, aes(x = pore_size, y = Shannon_Evenness, fill = pore_size)) +
  geom_boxplot(outlier.shape = 21, color = "black", alpha = 0.8, width = 0.7) +
  geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
  scale_fill_manual(values = pub_colors) +
  labs(x = "Pore Size (μm)", y = "Shannon Evenness (H / ln(S))") +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    legend.position = "none",
    plot.title = element_blank(),  # removed title
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),  # black border
    axis.ticks = element_line(color = "black")  # ticks on both axes
  )
