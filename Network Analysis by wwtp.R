# =============================
#  Publication-Quality Virus–WWTP Interaction Network (Order Level)
#  (With Abundance Represented in Legend Only)
# =============================

# --- Load Libraries ---
library(tidyverse)
library(igraph)
library(ggraph)
library(RColorBrewer)
library(ggrepel)
library(ggpubr)
library(showtext)

# --- Enable Beautiful Font (for publication aesthetics) ---
font_add_google("Lato", "lato")
showtext_auto()

# --- Step 1: Load and Prepare Data ---
file_path <- "C:/Users/Abdulaziz Khalil/Desktop/Dataset/combined_68_sample_order_matrix.csv"
df <- read.csv(file_path, check.names = FALSE)

# ✅ Ensure column names are consistent
if (!"sample.name" %in% names(df) && "sample_name" %in% names(df)) {
  df <- df %>% rename(sample.name = sample_name)
}
if (!"wwtp" %in% names(df)) stop("Input CSV must contain a 'wwtp' column (Gainsville, FL; Oceanside, CA; Orange County, CA; San Jose, CA; Toledo, OH).")

# --- Clean and standardize WWTP names ---
df <- df %>%
  mutate(
    WWTP = trimws(as.character(wwtp)),
    WWTP = case_when(
      WWTP %in% c("Gainsville, FL", "Gainesville, FL") ~ "Gainsville, FL",
      WWTP %in% c("Oceanside, CA", "Ocean Side, CA") ~ "Oceanside, CA",
      WWTP %in% c("Orange County, CA", "Orange, CA") ~ "Orange County, CA",
      WWTP %in% c("San Jose, CA", "SanJosé, CA") ~ "San Jose, CA",
      WWTP %in% c("Toledo, OH") ~ "Toledo, OH",
      TRUE ~ WWTP
    ),
    sample.name = paste0("vOTU ", sample.name)
  )

# --- Step 2: Reshape to Long Format ---
meta_cols <- c("sample.name", "wwtp", "WWTP")
df_long <- df %>%
  pivot_longer(
    cols = -any_of(meta_cols),
    names_to = "ViralOrder",
    values_to = "Abundance"
  ) %>%
  filter(!is.na(Abundance) & Abundance > 0)

# --- Step 3: Aggregate Data ---
df_agg <- df_long %>%
  group_by(WWTP, ViralOrder) %>%
  summarise(Abundance = sum(Abundance), .groups = "drop")

# --- Step 4: Build Graph ---
edges <- df_agg %>%
  rename(from = WWTP, to = ViralOrder) %>%
  select(from, to, Abundance)

nodes_wwtp <- unique(df_agg$WWTP)
nodes_virus <- unique(df_agg$ViralOrder)

nodes <- tibble(
  name = c(as.character(nodes_wwtp), as.character(nodes_virus)),
  type = c(rep("WWTP", length(nodes_wwtp)), rep("Virus", length(nodes_virus)))
)

g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
V(g)$plot_size <- ifelse(V(g)$type == "WWTP", 4.5, 6)

# --- Step 5: Define Color Palettes ---
wwtp_palette <- c(
  "Gainsville, FL" = "#E69F00",
  "Oceanside, CA" = "#56B4E9",
  "Orange County, CA" = "#009E73",
  "San Jose, CA" = "#F0E442",
  "Toledo, OH" = "#CC79A7"
)

missing_wwtp <- setdiff(nodes_wwtp, names(wwtp_palette))
if (length(missing_wwtp) > 0) {
  extra_cols <- colorRampPalette(brewer.pal(8, "Dark2"))(length(missing_wwtp))
  names(extra_cols) <- missing_wwtp
  wwtp_palette <- c(wwtp_palette, extra_cols)
}

virus_colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(nodes_virus))
names(virus_colors) <- nodes_virus

# --- Step 6: Create Layout ---
set.seed(123)
layout <- create_layout(g, layout = "fr")

# --- Step 7: Main Network Plot ---
p <- ggraph(layout) +
  geom_edge_link(
    colour = "gray65",
    width = 0.6,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  geom_node_point(
    data = filter(layout, type == "WWTP"),
    aes(x = x, y = y, fill = name),
    shape = 21,
    size = 6.5,
    color = "black",
    stroke = 0.6
  ) +
  geom_node_point(
    data = filter(layout, type == "Virus"),
    aes(x = x, y = y, fill = name),
    shape = 24,
    size = 7.5,
    color = "black",
    stroke = 0.5
  ) +
  scale_fill_manual(
    values = c(wwtp_palette, virus_colors),
    name = "Node Type"
  ) +
  geom_text_repel(
    data = layout,
    aes(x = x, y = y, label = name),
    size = 4.3,
    family = "lato",
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "gray70",
    max.overlaps = 30
  ) +
  theme_void() +
  ggtitle("Virus–WWTP Interaction Network (Order Level)") +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18,
      family = "lato"
    ),
    legend.position = "none"
  )

# --- Step 8: Unified Legends (WWTP, Viral Orders, and Abundance) ---
legend_wwtp <- ggpubr::get_legend(
  ggplot(data.frame(WWTP = names(wwtp_palette))) +
    geom_point(aes(x = WWTP, y = 1, fill = WWTP),
               shape = 21, size = 6, color = "black") +
    scale_fill_manual(name = "WWTP", values = wwtp_palette) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", family = "lato"),
      legend.text = element_text(size = 11, family = "lato")
    )
)

legend_virus <- ggpubr::get_legend(
  ggplot(data.frame(ViralOrder = names(virus_colors))) +
    geom_point(aes(x = ViralOrder, y = 1, fill = ViralOrder),
               shape = 24, size = 6, color = "black") +
    scale_fill_manual(name = "Viral Orders", values = virus_colors) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", family = "lato"),
      legend.text = element_text(size = 11, family = "lato")
    )
)

# ✅ Abundance Legend (only legend, not plotted)
legend_abundance <- ggpubr::get_legend(
  ggplot(df_agg, aes(x = Abundance, y = 1, color = Abundance)) +
    geom_point(size = 3) +
    scale_color_gradient(low = "#FDE725", high = "#440154", name = "Abundance") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold", family = "lato"),
      legend.text = element_text(size = 11, family = "lato")
    )
)

# --- Step 9: Combine Main Plot + Legends ---
combined_legends <- ggarrange(
  legend_wwtp,
  legend_virus,
  legend_abundance,
  ncol = 1,
  align = "v",
  heights = c(1, 1, 0.8)
)

final_plot <- ggarrange(
  p,
  combined_legends,
  ncol = 1,
  heights = c(10, 3)
)

# --- Step 10: Save Publication-Quality Figure ---
print(final_plot)

ggsave(
  "Virus_WWTP_OrderLevel_Network_with_AbundanceLegend.png",
  final_plot,
  width = 14,
  height = 10,
  dpi = 600,
  bg = "white"
)
