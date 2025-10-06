# =============================
# Bray-Curtis Dissimilarity and NMDS Scores Calculation
# =============================

library(tidyverse)
library(vegan)

# --- Step 1: Load viral vOTU abundance data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix_copy.csv"
df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Extract abundance matrix (exclude sample.name and pore_size) ---
abundance_df <- df %>% select(-sample.name, -pore_size)

# --- Step 3: Compute Bray-Curtis dissimilarity ---
bray_dist <- vegdist(abundance_df, method = "bray")

# --- Step 4: NMDS ordination (2D) ---
set.seed(123)  # for reproducibility
nmds_res <- metaMDS(bray_dist, k = 2, trymax = 100)

# --- Step 5: Extract NMDS scores for each sample ---
nmds_scores <- as.data.frame(scores(nmds_res))
nmds_scores$sample.name <- df$sample.name
nmds_scores$pore_size <- str_trim(df$pore_size)

# --- Step 6: Save NMDS scores to CSV ---
write.csv(nmds_scores,
          "C:/Users/Abdulaziz Khalil/Desktop/Dataset/nmds_scores_per_sample.csv",
          row.names = FALSE)

# --- Step 7: (Optional) Print NMDS stress value ---
cat("NMDS stress:", nmds_res$stress, "\n")
