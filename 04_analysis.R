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
library(quanteda)
library(tidylo)
library(dplyr)
library(cowplot)
library(ggrepel)

#load processed works object
load("data/works_proc_final.RData")

#203% growth between 2012 and 2022, compared to 59% globally in the same time period: https://ncses.nsf.gov/pubs/nsb202333
((length(which(works_proc$year < 2023)) - length(which(works_proc$year < 2013)))/length(which(works_proc$year < 2013)))*100

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
clusters <- cluster_louvain(network, weights = E(network)$weight, resolution = 1.38)
table(clusters$membership)

#get the text for log odds, abstracts for those that have, and otherwise titles
temp <- oa_fetch(entity = "works", identifier = works_proc$id, output = "list")
dois <- rep(NA, length(temp))
for(x in 1:length(temp)){
  temp_temp <- temp[[x]]$doi
  if(!is.null(temp_temp)){
    dois[x] <- temp_temp
  }
}
dois <- gsub("https://doi.org/", "", dois)
text_for_log_odds <- c()
for(i in 1:length(dois)){
  if(is.na(dois[i])){
    text_for_log_odds[i] <- NA
  } else {
    text_for_log_odds[i] <- tryCatch({
      cr_abstract(dois[i])
    }, error = function(e) {
      return(NA) 
    })
  }
}
text_for_log_odds[which(is.na(text_for_log_odds))] <- works_proc$title[which(is.na(text_for_log_odds))]

#get weighted log odds for each cluster
lemma_table <- lexicon::hash_lemmas
lemma_table <- rbind(lemma_table, data.frame(token = "musical", lemma = "music"))
log_odds <- list()
for(x in 1:7){
  pubs <- clusters$names[which(clusters$membership == x)]
  inds <- match(pubs, works_proc$id)
  words <- tokens(corpus(tolower(text_for_log_odds[inds])), remove_punct = TRUE, remove_numbers = TRUE, split_hyphens = TRUE, remove_symbols = TRUE)
  words <- as.tokens(lapply(words, function(x){textstem::lemmatize_words(tolower(x), dictionary = lemma_table)}))
  words <- as.tokens(lapply(words, function(x){x[which(is.na(as.numeric(x)))]}))
  words <- tokens_remove(words, c(stopwords("english"), "cultural", "evolution", "using", "among", "old", "new", "iranian", "build", "italian", "say", "cross", "large", "scale", "use", "culture", "principle", "matter", "density", "application", "investigate"))
  words <- tidyr::pivot_longer(convert(dfm(words), to = "data.frame"), cols = 2:ncol(convert(dfm(words), to = "data.frame")))
  log_odds[[x]] <- cbind(cluster = x, words %>% group_by(word = name) %>% summarise(n = sum(value)) %>% arrange(desc(n)))
}
log_odds <- bind_log_odds(rbindlist(log_odds), set = cluster, feature = word, n = n, unweighted = TRUE)
top_words <- log_odds %>% group_by(cluster) %>% arrange(cluster, desc(log_odds_weighted))
top_words <- top_words %>% group_split()
top_words <- lapply(top_words, function(x){x$word[1:10]})
log_odds <- list(log_odds = log_odds, top_words = top_words)
save(log_odds, file = "data/log_odds.RData")

#create function to produce word plots
word_plot <- function(data, n_to_plot = 10, cluster = 1, max_overlaps = 50, color = "black", title = "Title"){
  plot_data <- data.frame(data)[which(data$cluster == cluster), ]
  plot_data <- plot_data[order(plot_data$log_odds_weighted, decreasing = TRUE)[1:n_to_plot], ]
  ggplot(data = plot_data, aes(x = n, y = log_odds_weighted, label = word)) + 
    geom_text_repel(size = 3, force = 7, direction = "both", max.iter = 1000000, max.overlaps = max_overlaps, family = "Avenir", color = color, fontface = "bold", segment.size = 0.3) + 
    labs(x = NULL, y = NULL, title = title) + 
    scale_x_continuous(expand = expansion(mult = 0.2)) + 
    scale_y_continuous(expand = expansion(mult = 0.2)) + 
    theme_linedraw(base_family = "Avenir") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(color = color, face = "bold"))
}

#set colors and create plot
colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#D5C711", "#56B4E9", "#E69F00")
plot <- plot_grid(
  plot_grid(
    word_plot(log_odds$log_odds, cluster = 1, color = colors[1], title = "Narrative evolution"),
    word_plot(log_odds$log_odds, cluster = 2, color = colors[2], title = "Cultural phylogenetics"),
    word_plot(log_odds$log_odds, cluster = 3, color = colors[3], title = "Big data"),
    word_plot(log_odds$log_odds, cluster = 4, color = colors[4], title = "Evolutionary origins"),
    nrow = 1
  ),
  plot_grid(
    word_plot(log_odds$log_odds, cluster = 5, color = colors[5], title = "CE of music"),
    word_plot(log_odds$log_odds, cluster = 6, color = colors[6], title = "Biology of music"),
    word_plot(log_odds$log_odds, cluster = 7, color = colors[7], title = "Film and literature"),
    nrow = 1
  ),
  nrow = 2
)

#save plot
set.seed(1234); svg("output/log_odds.svg", width = 10, height = 5); plot; dev.off()

#export them to a text file
out_file <- file("data/cluster_log_odds.txt", "w")
for(x in 1:7){
  writeLines(paste0("CLUSTER ", x, "\n"), out_file)
  writeLines(paste0(paste0(top_words[[x]], collapse = ", "), "\n"), out_file)
}
close(out_file)

#save cluster details
out_file <- file("data/cluster_details.txt", "w")
for(x in 1:7){
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

#get large clusters
large_cluster_ids <- which(sizes(clusters) >= 10)

#manage colors
node_colors <- rep("gray50", vcount(network))
palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#D5C711", "#56B4E9", "#E69F00")
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
      return(paste0(last(strsplit(x[1], " ")[[1]]), " and ", last(strsplit(x[2], " ")[[1]])))
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
    color = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#D5C711", "#56B4E9", "#E69F00")[x]
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

#plot graph
plot_a <- ggraph(create_layout(network, layout = layout_df)) + 
  geom_edge_arc(aes(alpha = log(weight)), strength = 0.1) + 
  geom_node_point(aes(color = color)) + 
  geom_point_interactive(data = layout_df, aes(x = x, y = y, color = color, tooltip = label)) + 
  scale_edge_alpha(range = c(0, 0.05)) +
  scale_color_identity() +
  scale_x_continuous(limits = c(-0.02, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.1, 1.01), expand = c(0, 0)) +
  theme_graph(base_family = "Helvetica") + 
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_b <- ggplot(freq_table, aes(x = year, y = frequency)) + 
  geom_smooth(fill = "#009E73", color = "#009E73", linewidth = 0.5, alpha = 0.2) + 
  geom_point(color = "#009E73") + 
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0, 31)) + 
  labs(x = "Year", y = "Publications") + 
  theme_linedraw(base_family = "Helvetica") + 
  theme(panel.background = element_blank(), panel.porder = element_blank(), axis.line = element_line(color = "black"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.margin = unit(c(0, 0, 0, 0), "pt"), panel.grid = element_blank())

#https://patchwork.data-imaginist.com/reference/area.html
layout_patchwork <- c(
  area(1, 1, 12, 20),
  area(8, 1, 12, 7),
  area(1, 1, 12, 20)
)

#export interactive plot
plot <- free(plot_a)
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

#create static plot
pad <- 10
plot <- plot_grid(
  plot_a + xlim(0.12, 1) + ylim(0, 1) + theme(plot.margin = unit(c(pad, pad, pad, pad), "pt"), text = element_text(family = "Avenir")), 
  plot_b + xlim(1962, 2026) + theme(plot.margin = unit(c(pad, pad, pad, pad), "pt"), text = element_text(family = "Avenir")), 
  labels = "AUTO", rel_widths = c(60, 40)
)

#export static plot
svg("output/ce_art_biblio.svg", width = 10, height = 4); plot; dev.off()

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

# #export all papers used (and manually correct the NULL authors)
# temp <- paste0(
#   sapply(works_proc$author, function(x){
#     if(length(x) == 1){
#       return(last(strsplit(x[1], " ")[[1]]))
#     }
#     if(length(x) == 2){
#       return(paste0(last(strsplit(x[1], " ")[[1]]), " and ", last(strsplit(x[2], " ")[[1]])))
#     }
#     if(length(x) > 2){
#       return(paste0(last(strsplit(x[1], " ")[[1]]), " et al."))
#     }
#   }), " (", 
#   works_proc$year, "). ", 
#   works_proc$title, ". ",
#   works_proc$source, "."
# )
# temp <- gsub("\\\"", "", temp)
# temp <- gsub("<i>", "", temp)
# temp <- gsub("</i>", "", temp)
# temp <- gsub("\u0098The \u009cWorld Of Music/\u0098The\u009c World Of Music", "The World of Music", temp)
# temp <- gsub("&amp;#x2028;j", "J", temp)
# temp <- sort(temp)
# sink("output/analyzed_papers.txt")
# writeLines(temp)
# sink
