#set working directory
setwd("~/Documents/Work/Fall 2025/ce_art_biblio")

#load libraries
library(jsonlite)
library(openalexR)
library(igraph)
library(bib2df)
library(data.table)
library(ggraph)
library(ggiraph)
library(htmlwidgets)
library(parallel)
library(stringdist)
library(patchwork)

#load processed works object
load("data/works_proc_final.RData")

#define jaccard index
jaccard <- function(vector_a, vector_b){
  if(length(vector_a) == 0 & length(vector_b) == 0){
    return(0)
  } else{
    length(intersect(vector_a, vector_b))/(length(vector_a) + length(vector_b) - length(intersect(vector_a, vector_b)))
  }
}

#compute similarity matrix between reference lists based on jaccard index
combos <- t(combn(nrow(works_proc), 2))
sim_matrix <- matrix(0, nrow = nrow(works_proc), ncol = nrow(works_proc))
rownames(sim_matrix) <- works_proc$id
colnames(sim_matrix) <- works_proc$id
for(x in 1:nrow(combos)){
  sim_matrix[combos[x, 1], combos[x, 2]] <- jaccard(works_proc$references[[combos[x, 1]]], works_proc$references[[combos[x, 2]]])
  sim_matrix[combos[x, 2], combos[x, 1]] <- sim_matrix[combos[x, 1], combos[x, 2]]
}

#convert matrix to an igraph network and get largest connected component
network <- graph_from_adjacency_matrix(sim_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
comps <- components(network)
network <- induced_subgraph(network, V(network)[comps$membership == which.max(comps$csize)])

#cluster with fast greedy algorithm
set.seed(12345)
clusters <- cluster_louvain(network, weights = E(network)$weight, resolution = 1.3)
#clusters$names
#table(clusters$membership)

#save cluster details
out_file <- file("data/cluster_details.txt", "w")
for(x in 1:8){
  writeLines(paste0("CLUSTER ", x, "\n"), out_file)
  pubs <- clusters$names[which(clusters$membership == x)]
  inds <- match(pubs, works_proc$id)
  writeLines(
    paste0(sapply(works_proc$author[inds], function(y){y[1]}), " (", works_proc$year[inds], "). ", works_proc$title[inds], ". ", works_proc$source[inds], "."),
    out_file
  )
  writeLines("\n", out_file)
}
close(out_file)

#gemini 3 pro
#Write short descriptions of these clusters of publications on cultural evolution of the arts, to be used as subfield labels.
labels <- c(
  "Narrative evolution",
  "Cultural phylogenetics",
  "Big data",
  "Evolutionary origins and adaptation",
  "Biology of music",
  "Cultural evolution of music",
  "Dynamics of film and literature"
)

#get large clusters
large_cluster_ids <- which(sizes(clusters) >= 10)

#manage colors
node_colors <- rep("gray50", vcount(network))
palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442", "#56B4E9", "#E69F00")
color_map <- setNames(palette, large_cluster_ids)
membership <- membership(clusters)
nodes_in_large_clusters <- which(membership(clusters) %in% large_cluster_ids)
node_colors[nodes_in_large_clusters] <- color_map[as.character(membership[nodes_in_large_clusters])]
V(network)$color <- node_colors

#compute layout
layout <- layout_with_kk(network, weights = ifelse(crossing(clusters, network), 1, 0.35))
layout <- graphlayouts::layout_rotate(layout, -50)
layout[, 1] <- usefun::normalize_to_range(layout[, 1], range = c(0, 1))
layout[, 2] <- usefun::normalize_to_range(layout[, 2], range = c(0, 1))

#create layout object for interactive plot
layout_df <- as.data.frame(layout)
colnames(layout_df) <- c("x", "y")
layout_df <- cbind(layout_df, color = node_colors)
ids <- match(V(network)$name, works_proc$id)
#layout_df$label <- paste0(sapply(works_proc$author[ids], function(x){x[1]}), " (", works_proc$year[ids], "). ", works_proc$title[ids], ". ", works_proc$source[ids], ".")
layout_df$label <- paste0(
  sapply(works_proc$author[ids], function(x){
    if(length(x) == 1){
      return(last(strsplit(x[1], " ")[[1]]))
    }
    if(length(x) == 2){
      return(paste0(last(strsplit(x[1], " ")[[1]]), " and ", last(strsplit(x[1], " ")[[1]])))
    }
    if(length(x) > 2){
      return(paste0(last(strsplit(x[1], " ")[[1]]), " et al."))
    }
  }), " (", 
  works_proc$year[ids], "). ", 
  works_proc$title[ids], "."
)

#get frequency table for plotting by cluster
cluster_freq_table <- do.call(rbind, lapply(1:7, function(x){
  data.frame(
    table(works_proc$year[which(works_proc$id %in% clusters$names[which(clusters$membership == x)])]), 
    cluster = x,
    color = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442", "#56B4E9", "#E69F00")[x]
  )
}))
colnames(cluster_freq_table) <- c("year", "frequency", "cluster", "color")
cluster_freq_table$cluster <- as.factor(cluster_freq_table$cluster)
cluster_freq_table$year <- as.numeric(as.character(cluster_freq_table$year))

#get frequency table for plotting
freq_table <- data.frame(table(works_proc$year))
colnames(freq_table) <- c("year", "frequency")
freq_table$year <- as.numeric(as.character(freq_table$year))
freq_table$label <- sapply(freq_table$year, function(x){
  inds <- which(works_proc$year == x)
  paste(paste0(sapply(works_proc$author[inds], function(x){x[1]}), " (", works_proc$year[inds], "). ", works_proc$title[inds], ". ", works_proc$source[inds], "."), collapse = "\n")
})

#create data frame of labels
labels_df <- data.frame(
  x = 0, y = c(0.98, 0.94, 0.90, 0.86, 0.82, 0.78, 0.74),
  label = labels,
  fill_color = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442", "#56B4E9", "#E69F00"), hjust = 0
)

#plot graph
plot_a <- ggraph(create_layout(network, layout = layout)) + 
  geom_edge_arc(aes(alpha = log(weight)), strength = 0.1) + 
  geom_node_point(aes(color = color)) + 
  geom_point_interactive(data = layout_df, aes(x = x, y = y, color = color, tooltip = label)) + 
  scale_edge_alpha(range = c(0, 0.05)) +
  scale_color_identity() +
  scale_x_continuous(limits = c(-0.02, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.1, 1.01), expand = c(0, 0)) +
  theme_graph(base_family = "Helvetica") + 
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_b <- ggplot(freq_table) + 
  geom_point_interactive(aes(x = year, y = frequency, tooltip = label)) + 
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  labs(x = "Year", y = "Publications") + 
  theme_linedraw(base_family = "Helvetica") + 
  theme(panel.background = element_blank(), plot.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_c <- ggplot(labels_df) + 
  geom_label_interactive(aes(x = x, y = y, label = label, fill = fill_color, hjust = hjust), color = "white") + 
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_identity() + 
  theme_void(base_family = "Helvetica") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_d <- ggplot(cluster_freq_table) + 
  geom_area(aes(x = year, y = frequency, fill = color)) + 
  scale_x_continuous(breaks = seq(1975, 2025, 5)) +
  scale_fill_identity() + 
  labs(x = "Year", y = "Publications") + 
  theme_linedraw(base_family = "Helvetica") + 
  theme(panel.background = element_blank(), plot.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.margin = unit(c(0, 0, 0, 0), "pt"))

#https://patchwork.data-imaginist.com/reference/area.html
layout_patchwork <- c(
  area(1, 1, 12, 20),
  area(8, 1, 12, 7),
  area(1, 1, 12, 20)
)

#create static plot
plot <- free(plot_a) + free(plot_d) + free(plot_c) + plot_layout(design = layout_patchwork)

#export static plot
png("output/ce_art_biblio.png", width = 10, height = 5.5, units = "in", res = 600); plot; dev.off()
svg("output/ce_art_biblio.svg", width = 10, height = 5.5); plot; dev.off()

#export interactive plot
interactive_plot <- girafe(
  ggobj = plot, 
  fonts = list(sans = "Helvetica"), 
  width_svg = 10,
  height_svg = 5.5,
  options = list(
    opts_tooltip(css = "font-family: Arial, Helvetica, sans-serif; font-style: bold; background-color: black; color: white; padding: 10px; border-radius: 10px")
  )
)
saveWidget(interactive_plot, file = "docs/index.html", selfcontained = TRUE)

# #export the top citations (and manually correct them later)
# freq_table <- data.frame(sort(table(unlist(works_proc$references)), decreasing = TRUE)[2:41])
# colnames(freq_table) <- c("id", "count")
# freq_table$id <- as.character(freq_table$id)
# freq_table$title <- sapply(1:nrow(freq_table), function(x){
#   temp <- paste0("https://openalex.org/", freq_table$id[x])
#   temp <- oa_fetch(entity = "works", identifier = temp, output = "list")
#   paste0(temp$authorships[[1]]$author$display_name, " (", temp$publication_year, "). ", stringr::str_to_sentence(temp$title), ". ", stringr::str_to_title(temp$primary_location$source$display_name), ".")
# })
# freq_table <- freq_table[, c(2, 3)]
# write.csv(freq_table, "output/top_20.csv")
