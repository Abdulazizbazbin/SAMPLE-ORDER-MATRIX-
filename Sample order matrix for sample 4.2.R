# ================================
# Create Sample × Order (prediction) Matrix for 4.2
# Clean and rename order taxon names
# Prevent extra NA row
# ================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Step 1: Read CSV
taxa_data <- read_csv("C:/Users/Abdulaziz Khalil/Desktop/Dataset/4.2_final_assignments.csv")

# Step 2: Clean 'order (prediction)' names robustly
taxa_data <- taxa_data %>%
  mutate(`order (prediction)` = vapply(`order (prediction)`, function(x) {
    
    # Handle NA or blank
    if (is.na(x) || trimws(x) == "") return("Unknown")
    
    # Split at '|'
    parts <- str_split(x, "\\|")[[1]]
    first_part <- parts[1]
    
    # Apply renaming rules
    if (tolower(first_part) == "nan") {
      # nan|OrderName → keep second part
      if (length(parts) > 1) return(parts[2]) else return("Unassigned")
    }
    if (str_detect(first_part, "novel_order")) {
      return("novel_order")  # rename novel orders
    }
    
    return(first_part)  # otherwise keep first part
  }, FUN.VALUE = character(1)))  # ensures output is character, no NAs introduced

# Step 2.1: Optional safety check to remove any rows with missing sample names
taxa_data <- taxa_data %>% filter(!is.na(sample.name))

# Step 3: Create order-level abundance table
sample_order_matrix <- taxa_data %>%
  group_by(sample.name, `order (prediction)`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `order (prediction)`,
    values_from = Count,
    values_fill = 0
  )

# Step 4: View resulting matrix
head(sample_order_matrix)

# Step 5: Save matrix
write.csv(
  sample_order_matrix,
  "C:/Users/Abdulaziz Khalil/Desktop/Dataset/4.2_sample_order_matrix.csv",
  row.names = FALSE
)
