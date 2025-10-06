library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Step 1: Read CSV
taxa_data <- read_csv("C:/Users/Abdulaziz Khalil/Desktop/Dataset/4.1_final_assignments.csv")

# Step 2: Clean 'order (prediction)' names
taxa_data <- taxa_data %>%
  mutate(`order (prediction)` = sapply(`order (prediction)`, function(x) {
    if (is.na(x) || trimws(x) == "") return("Unknown")        # NA or blank → Unknown
    # Split at '|'
    parts <- str_split(x, "\\|")[[1]]
    first_part <- parts[1]
    
    # Apply renaming rules
    if (tolower(first_part) == "nan") {
      # nan|OrderName → keep second part
      if (length(parts) > 1) return(parts[2]) else return("Unassigned")
    }
    if (str_detect(first_part, "novel_order")) {
      return("novel_order")                                   # rename novel orders
    }
    return(first_part)                                         # otherwise keep first part
  }))

# Step 3: Create order-level abundance table
sample_order_matrix <- taxa_data %>%
  group_by(sample.name, `order (prediction)`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = `order (prediction)`,
    values_from = Count,
    values_fill = 0
  )

# Step 4: View matrix
head(sample_order_matrix)

# Step 5: Save matrix
write.csv(sample_order_matrix,
          "C:/Users/Abdulaziz Khalil/Desktop/Dataset/4.1_sample_order_matrix.csv",
          row.names = FALSE)
