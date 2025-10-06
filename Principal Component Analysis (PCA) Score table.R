# =============================
# PCA Analysis: Handle constant columns and compute % Variance Explained
# =============================

library(tidyverse)

# --- Step 1: Load data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix_copy.csv"
df <- read.csv(file_path, check.names = FALSE)

# --- Step 2: Extract abundance data (exclude sample.name and pore_size) ---
abundance_df <- df %>% select(-sample.name, -pore_size)

# --- Step 3: Remove constant columns (columns with zero variance) ---
abundance_df <- abundance_df[, apply(abundance_df, 2, var) != 0]

# --- Step 4: Perform PCA ---
pca_res <- prcomp(abundance_df, scale. = TRUE)

# --- Step 5: Calculate % variance explained ---
var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
pc1_var <- var_explained[1] * 100
pc2_var <- var_explained[2] * 100

cat("Variance explained by PC1:", round(pc1_var, 1), "%\n")
cat("Variance explained by PC2:", round(pc2_var, 1), "%\n")

# --- Step 6: Prepare PCA scores for downstream plotting ---
pca_scores <- as.data.frame(pca_res$x)
pca_df <- cbind(df %>% select(sample.name, pore_size), pca_scores)

# --- Step 7: Save PCA scores to CSV ---
write.csv(pca_df,
          "C:/Users/Abdulaziz Khalil/Desktop/Dataset/pca_scores_per_sample.csv",
          row.names = FALSE)
