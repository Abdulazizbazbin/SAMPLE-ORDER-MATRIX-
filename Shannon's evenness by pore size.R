# =============================  
# Publication-Quality Boxplot of Shannon Evenness by Pore Size
# (with significance & bold axis text)
# =============================

library(tidyverse)
library(ggpubr)
library(rstatix)

# --- Step 1: Load the data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/shannon_richness_evenness_per_sample 1.csv"
shannon_df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Clean pore_size ---
shannon_df$pore_size <- stringr::str_trim(shannon_df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
shannon_df$pore_size <- factor(shannon_df$pore_size, levels = pore_levels)

# --- Step 3: Color palette ---
pub_colors <- c(
  "0.45 filtrate" = "gold",
  "0.45"          = "steelblue",
  "3"             = "firebrick",
  "20"            = "forestgreen",
  "100"           = "purple"
)

# --- Step 4: Global Kruskal–Wallis test ---
kw_res_even <- kruskal.test(Shannon_Evenness ~ pore_size, data = shannon_df)
kw_res_even

# --- Step 5: (optional) pairwise tests for your info ---
pairwise_even <- shannon_df %>%
  pairwise_wilcox_test(Shannon_Evenness ~ pore_size, p.adjust.method = "BH")
pairwise_even

# --- Step 6: Define the 4 comparisons you want to show ---
my_comparisons_even <- list(
  c("0.45 filtrate", "0.45"),
  c("0.45 filtrate", "3"),
  c("0.45 filtrate", "20"),
  c("0.45 filtrate", "100")   # ✅ this is the one that was missing
)

y_max_even <- max(shannon_df$Shannon_Evenness, na.rm = TRUE)

# --- Step 7: Plot with significance ---
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
  
  # Pairwise lines (all vs 0.45 filtrate, including 100 µm)
  stat_compare_means(
    comparisons   = my_comparisons_even,
    method        = "wilcox.test",
    label         = "p.signif",
    hide.ns       = FALSE,      # show ns as well
    step.increase = 0.07
  ) +
  
  # Global Kruskal–Wallis label on the right
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
