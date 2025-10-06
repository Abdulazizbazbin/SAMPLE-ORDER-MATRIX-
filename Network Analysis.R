NETWORK ANALYSIS:
  # ============================= 
# Virus–Sample Network with colored triangles 
# ============================= 
library(tidyverse) 
library(igraph) 
library(ggraph) 
library(RColorBrewer) 
# --- Step 1: Load data --- 
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_23_sample_order_matrix.csv" 
df <- read.csv(file_path, check.names = FALSE) 
# --- Step 2: Rename samples --- 
df <- df %>% 
  mutate(sample.name = paste0("vOTU ", sample.name)) 
# --- Step 3: Long-format data --- 
df_long <- df %>% 
  pivot_longer(-sample.name, names_to = "ViralOrder", values_to = "Abundance") %>% 
  filter(Abundance > 0) 
# --- Step 4: Build network --- 
edges <- df_long %>% select(sample.name, ViralOrder, Abundance) 
nodes_samples <- unique(df_long$sample.name) 
nodes_viruses <- unique(df_long$ViralOrder) 
nodes <- tibble( 
  name = c(nodes_samples, nodes_viruses), 
  type = c(rep("Sample", length(nodes_samples)), rep("Virus", length(nodes_viruses))) 
) 
g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE) 
# --- Step 5: Compute virus sizes --- 
virus_abundance <- df_long %>% 
  group_by(ViralOrder) %>% 
  summarise(total_abundance = sum(Abundance)) %>% 
  mutate(size = sqrt(total_abundance) * 4) # increased virus triangle size 
# --- Step 6: Assign node attributes --- 
V(g)$size <- ifelse( 
  V(g)$type == "Virus", 
  virus_abundance$size[match(V(g)$name, virus_abundance$ViralOrder)], 
  12 # sample triangle size 
) 
# Assign colors 
sample_colors <- RColorBrewer::brewer.pal(n = max(3, length(nodes_samples)), name = "Set1") 
virus_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(length(nodes_viruses)) # 17 unique virus colors 
V(g)$color <- ifelse( 
  V(g)$type == "Virus", 
  virus_colors[match(V(g)$name, nodes_viruses)], # unique color for each virus 
  sample_colors[match(V(g)$name, nodes_samples)] # color for samples 
) 
# --- Step 7: Plot network with filled triangles --- 
set.seed(123) 
ggraph(g, layout = "fr") + 
  geom_edge_link(width = 0.4, alpha = 0.3, color = "gray60") + # connectors 
  geom_node_point(aes(size = size, fill = color), shape = 24, color = "black") + # filled triangles 
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5, fontface = "italic") + # italic all labels 
  scale_fill_identity(name = "Node Color") + # legend now shows actual fill colors 
  scale_size_continuous( 
    name = "vOTU Abundance", 
    breaks = virus_abundance$size, 
    labels = virus_abundance$ViralOrder # only viral order names 
  ) + 
  guides(fill = guide_legend(override.aes = list(shape = 24, size = 6))) + # show filled triangles in legend 
  theme_void() + 
  ggtitle("Predicted Virus Network (Order Level)") + 
  theme( 
    legend.position = "bottom", 
    plot.title = element_text(hjust = 0.5, face = "bold") 
  )
