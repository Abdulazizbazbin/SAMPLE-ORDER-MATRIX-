# =============================
# Publication-Quality Heatmap of Viral Order Abundance (No Title)
# =============================

# --- Load Required Libraries ---
library(tidyverse)
library(pheatmap)
library(RColorBrewer)

# --- Step 1: Load Data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix_copy.csv"
df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Clean and order pore_size ---
df$pore_size <- str_trim(df$pore_size)
pore_levels <- c("0.45 filtrate", "0.45", "3", "20", "100")
df$pore_size <- factor(df$pore_size, levels = pore_levels)

# --- Step 3: Prepare Abundance Matrix ---
meta_cols <- c("sample.name", "pore_size")
abundance_df <- df %>%
  select(-all_of(meta_cols)) %>%
  mutate(across(everything(), as.numeric))

rownames(abundance_df) <- df$sample.name

# --- Step 4: Transform (log10) for visualization ---
abundance_log <- log10(abundance_df + 1)
abundance_mat <- as.matrix(abundance_log)

# --- Step 5: Row annotation (pore size) ---
annotation_row <- data.frame(pore_size = df$pore_size)
rownames(annotation_row) <- df$sample.name

# --- Step 6: Define high-quality color palettes ---
abundance_colors <- colorRampPalette(rev(brewer.pal(n = 11, name = "RdYlBu")))(200)
pore_colors <- c(
  "0.45 filtrate" = "#7B2CBF",  # purple
  "0.45" = "#38A169",           # green
  "3" = "#E53E3E",              # red
  "20" = "#3182CE",             # blue
  "100" = "#F6E05E"             # yellow
)
annotation_colors <- list(pore_size = pore_colors)

# --- Step 7: Publication-style heatmap (no title) ---
pheatmap(
  abundance_mat,
  annotation_row = annotation_row,
  annotation_colors = annotation_colors,
  color = abundance_colors,
  border_color = NA,
  fontsize = 12,
  fontsize_row = 10,
  fontsize_col = 10,
  show_rownames = TRUE,
  show_colnames = TRUE,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  angle_col = 45,   # diagonal column names
  cellwidth = 18,   # uniform columns
  cellheight = 12,  # improves readability
  legend = TRUE
)

# --- Step 8 (optional): Save high-resolution version ---
# png("C:/Users/Abdulaziz Khalil/Desktop/viral_order_heatmap_no_title.png", width = 3000, height = 2000, res = 300)
# pheatmap(...)
# dev.off()
