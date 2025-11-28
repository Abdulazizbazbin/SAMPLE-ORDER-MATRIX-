# =============================  
# Publication-Quality Boxplot of Shannon Evenness by Pore Size
# (with enlarged significance labels)
# =============================

library(tidyverse)
library(ggpubr)
library(rstatix)

# --- Load data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/shannon_richness_evenness_per_sample 1.csv"
shannon_df <- read.csv(file_path, check.names = FALSE)

shannon_df$pore_size <- stringr::str_trim(shannon_df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
shannon_df$pore_size <- factor(shannon_df$pore_size, levels = pore_levels)

# Colors
pub_colors <- c(
  "0.45 filtrate" = "gold",
  "0.45"          = "steelblue",
  "3"             = "firebrick",
  "20"            = "forestgreen",
  "100"           = "purple"
)

# Kruskal–Wallis
kruskal.test(Shannon_Evenness ~ pore_size, data = shannon_df)

# Comparisons (all vs 0.45 filtrate)
my_comparisons_even <- list(
  c("0.45 filtrate", "0.45"),
  c("0.45 filtrate", "3"),
  c("0.45 filtrate", "20"),
  c("0.45 filtrate", "100")
)

y_max_even <- max(shannon_df$Shannon_Evenness, na.rm = TRUE)

# --- Plot ---
p_even <- ggplot(shannon_df, aes(x = pore_size, y = Shannon_Evenness, fill = pore_size)) +
  
  geom_boxplot(
    outlier.shape = NA,
    color = "black",
    alpha = 0.85,
    width = 0.7
  ) +
  
  geom_jitter(
    shape = 21,
    color = "black",
    width = 0.15,
    size = 2.5,
    alpha = 0.9
  ) +
  
  scale_fill_manual(values = pub_colors) +
  
  labs(
    x = "Pore Size (μm)",
    y = "Shannon Evenness (H / ln(S))"
  ) +
  
  # Pairwise significance with **larger stars/ns**
  stat_compare_means(
    comparisons   = my_comparisons_even,
    method        = "wilcox.test",
    label         = "p.signif",
    hide.ns       = FALSE,
    size          = 6,          # <<< Increase significance text size here
    step.increase = 0.07
  ) +
  
  # Global test label on right
  stat_compare_means(
    method  = "kruskal.test",
    label.y = y_max_even * 1.05,
    label.x = 5,
    size    = 4
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title      = element_text(face = "bold", size = 14),
    axis.text       = element_text(face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "black", fill = NA, size = 1.2),
    axis.ticks       = element_line(color = "black")
  ) +
  
  coord_cartesian(ylim = c(0, y_max_even * 1.20))

p_even
