# ============================= 
# ✅ Publication-Quality Boxplot of Shannon Evenness by Pore Size (with Black Outlined Points)
# =============================

library(tidyverse)

# --- Step 1: Load Shannon-Richness-Evenness data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/shannon_richness_evenness_per_sample 1.csv"
shannon_df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Clean pore_size column ---
shannon_df$pore_size <- str_trim(shannon_df$pore_size)

# --- Step 3: Set pore size order (smallest to largest) ---
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
shannon_df$pore_size <- factor(shannon_df$pore_size, levels = pore_levels)

# --- Step 4: Define publication-quality color palette ---
pub_colors <- c("gold", "steelblue", "firebrick", "forestgreen", "purple")

# --- Step 5: Create publication-quality boxplot (Shannon Evenness by Pore Size) ---
ggplot(shannon_df, aes(x = pore_size, y = Shannon_Evenness, fill = pore_size)) +
  # Boxplot with black border
  geom_boxplot(
    outlier.shape = 21,
    color = "black",
    alpha = 0.85,
    width = 0.7
  ) +

  # ✅ Jittered data points with black outer boundary
  geom_jitter(
    aes(fill = pore_size),
    shape = 21,              # supports both fill and border color
    color = "black",         # solid black outline
    width = 0.15,
    size = 2.5,
    alpha = 0.9
  ) +

  # --- Apply consistent color palette ---
  scale_fill_manual(values = pub_colors) +

  labs(
    x = "Pore Size (μm)",
    y = "Shannon Evenness (H / ln(S))"
  ) +
  theme_minimal(base_size = 14, base_family = "Helvetica") +
  theme(
    legend.position = "none",
    plot.title = element_blank(),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks = element_line(color = "black")
  )

# --- Step 6 (optional): Save the plot ---
# ggsave("C:/Users/Abdulaziz Khalil/Desktop/Dataset/shannon_evenness_boxplot_by_poresize_blackoutline.png",
#        width = 8, height = 6, dpi = 600, bg = "white")
