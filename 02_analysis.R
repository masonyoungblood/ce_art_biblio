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
library(htmltools)
library(parallel)
library(stringdist)
library(patchwork)

#get put into polite pool
options(openalexR.mailto = "masonyoungblood@gmail.com")

#load in extracted references
data <- fromJSON("data/references.json")

#get dois
source_data <- do.call(rbind, lapply(data, function(x){
  temp_doi <- sapply(x$doi, function(y){
    if(is.null(y)){
      return(NA)
    } else{
      return(y[1])
    }
  })
  temp_title <- sapply(x$title, function(y){
    if(is.null(y)){
      return(NA)
    } else{
      return(y[1])
    }
  })
  data.frame(doi = temp_doi, title = temp_title, stringsAsFactors = FALSE)
}))

#delete final periods and )
source_data$doi <- sapply(1:length(source_data$doi), function(x){
  final <- substr(source_data$doi[x], nchar(source_data$doi[x]), nchar(source_data$doi[x]))
  if(final %in% c(".", ")")){
    return(substr(source_data$doi[x], 1, nchar(source_data$doi[x])-1))
  } else{
    return(source_data$doi[x])
  }
})

#add the source pdfs themselves to the dataset
source_data <- rbind(source_data, read.csv("data/sources/sources.csv"))

#collect dois from bibtex files and add to other dois
biblio_files <- list.files(path = "data/biblio", pattern = "\\.bib$", recursive = TRUE, full.names = TRUE)
biblio_data <- do.call(rbind, lapply(biblio_files, function(file) {
  bib_data <- tryCatch({
    bib2df(file)
  }, error = function(e) {
    warning(paste("Could not parse file:", file, "\nError:", e$message))
    return(NULL)
  })
  if (is.null(bib_data) || nrow(bib_data) == 0) {
    return(NULL)
  }
  if (!"DOI" %in% names(bib_data)) {
    bib_data$DOI <- NA_character_
  }
  if (!"TITLE" %in% names(bib_data)) {
    bib_data$TITLE <- NA_character_
  }
  data.frame(doi = bib_data$DOI, title = gsub("\\}", "", gsub("\\{", "", bib_data$TITLE)), stringsAsFactors = FALSE)
}))

#combine both datasets into a single dataset
collect_data <- rbind(source_data, biblio_data)

#clean up and remove duplicates
collect_data$title <- tolower(collect_data$title)
collect_data <- unique(collect_data)

#remove rows with NA in title and doi
collect_data <- collect_data[-which(is.na(collect_data$title) & is.na(collect_data$doi)), ]

#write to file
write.csv(collect_data, "data/collect_data.csv")

#collect variable "relevant" (0/1) using gemini 2.5 pro with the following prompt
#Add a third column, "relevant", which is 1 if the article is related to the cultural 
#evolution of the arts in humans and 0 if the article is unrelated or has a nonsense title.

#read in file with relevance coding
collect_data <- read.csv("data/collect_data_proc.csv")
collect_data <- collect_data[which(collect_data$relevant == 1), ]

#collect works from openalex
#lexical errors occur when 404
works <- vector("list", nrow(collect_data))
for(x in 1:10){
  message(paste("Processing row", x, "of", nrow(collect_data)))
  works[[x]] <- tryCatch({
    if(is.na(collect_data$doi[x])){
      oa_fetch(entity = "works", search = collect_data$title[x], per_page = 1, pages = 1, output = "list")
    } else{
      oa_fetch(entity = "works", doi = collect_data$doi[x], output = "list")
    }
  }, 
  error = function(e) {
    message(paste("An error occurred on row", x, ":", collect_data$title[x]))
    message("Error details: ", e$message)
    return(NULL)
  })
}

doi_matches <- oa_fetch(entity = "works", doi = collect_data$doi[-which(is.na(collect_data$doi))], output = "list")

title_matches <- list()
for(x in 1:length(collect_data$title)){
  title_matches[[x]] <- oa_fetch(entity = "works", search = collect_data$title[x], output = "list")[[1]]
  message(paste(x, "of", nrow(collect_data)))
}

#collect results from titles alone
title_matches <- list()
no_results <- c()
errors <- c()
for(x in 1:length(collect_data$title)){
  message(paste("Processing row", x, "of", nrow(collect_data)))
  tryCatch({
    api_result <- oa_fetch(entity = "works", search = collect_data$title[x], per_page = 1, pages = 1, output = "list")
    if(is.list(api_result) && length(api_result) > 0){
      title_matches[[x]] <- api_result[[1]] 
    } else{
      no_results <<- c(no_results, x)
      title_matches[[x]] <- collect_data$title[x]
    }
  }, 
  error = function(e){
    message(paste("An error occurred on row", x, ":", e$message))
    errors <<- c(errors, x)
    title_matches[[x]] <- collect_data$title[x]
  })
}

save(title_matches, file = "title_matches.RData")

#get only the titles
just_titles <- sapply(1:length(title_matches), function(x){
  if(x %in% c(errors, no_results)){
    return(NA)
  } else{
    temp <- title_matches[[x]]$title
  }
})

#identify an appropriate similarity cutoff, in this case levenshtein of 24 or less is a solid match
#title_dists <- stringdist(tolower(just_titles), tolower(collect_data$title), method = "lv")
#just_titles[which(title_dists == 24)]
#collect_data$title[which(title_dists == 24)]

#find the poor matches or not retrievable that do have a doi we can try
doi_inds <- unique(c(which(title_dists > 24 & !is.na(collect_data$doi)), no_results[which(!is.na(collect_data$doi[no_results]))]))

#collect results from dois
doi_matches <- list()
no_results <- c()
errors <- c()
for(x in 1:length(doi_inds)){
  message(paste("Processing row", x, "of", length(doi_inds)))
  tryCatch({
    api_result <- oa_fetch(entity = "works", doi = collect_data$doi[doi_inds[x]], output = "list")
    if(is.list(api_result) && length(api_result) > 0){
      doi_matches[[x]] <- api_result[[1]] 
    } else{
      no_results <<- c(no_results, x)
      doi_matches[[x]] <- collect_data$doi[x]
    }
  }, 
  error = function(e){
    message(paste("An error occurred on row", x, ":", e$message))
    errors <<- c(errors, x)
    doi_matches[[x]] <- collect_data$doi[x]
  })
}

#replace missing results from title matches with correct ones based on doi
title_matches[doi_inds[which(lengths(doi_matches) > 1)]] <- doi_matches[which(lengths(doi_matches) > 1)]

#0.06039326 of references couldn't be retrieved
#length(which(lengths(title_matches) == 1))/length(title_matches)
works <- title_matches[-which(lengths(title_matches) == 1)]

#save the works object
save(works, file = "data/works.RData")

#convert works into a data table that includes referenced works
works_proc <- rbindlist(lapply(works, function(x){
  data.table(
    id = gsub("https://openalex.org/", "", x$id),
    type = x$type,
    title = stringr::str_to_sentence(x$display_name),
    year = x$publication_year,
    source = stringr::str_to_title(x$primary_location$source$display_name),
    authors = list(sapply(x$authorships, function(y){y$author$display_name})),
    references = list(gsub("https://openalex.org/", "", unlist(x$referenced_works)))
  )
}), fill = TRUE)

#get rid of duplicates
works_proc <- works_proc[-which(duplicated(works_proc$id))]

#save processed works object
save(works_proc, file = "data/works_proc.RData")

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
clusters <- cluster_louvain(network, weights = E(network)$weight)

clusters$names
clusters$membership

out_file <- file("data/cluster_details.txt", "w")
for(x in 1:5){
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

#gemini 2.5 pro
#Write short descriptions of these clusters of publications on cultural evolution of the arts, to be used as subfield labels.
#1: Foundations of Cultural Evolution
#2: Cognitive Science of Music
#3: Cognitive Science of Narrative and Fiction
#4: Cultural Phylogenetics and Macroevolution
#5: Computational Humanities and Social Science

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
layout <- layout_with_kk(network, weights = ifelse(crossing(clusters, network), 1, 0.2))
layout <- graphlayouts::layout_rotate(layout, 180)
layout[, 1] <- usefun::normalize_to_range(layout[, 1], range = c(0, 1))
layout[, 2] <- usefun::normalize_to_range(layout[, 2], range = c(0, 1))

#create layout object for interactive plot
layout_df <- as.data.frame(layout)
colnames(layout_df) <- c("x", "y")
layout_df <- cbind(layout_df, color = node_colors)
ids <- match(V(network)$name, works_proc$id)
layout_df$label <- paste0(sapply(works_proc$author[ids], function(x){x[1]}), " (", works_proc$year[ids], "). ", works_proc$title[ids], ". ", works_proc$source[ids], ".")

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
  x = 0, y = c(0.98, 0.94, 0.90, 0.86, 0.82),
  label = c("Foundations of Cultural Evolution",
            "Cognitive Science of Music",
            "Cognitive Science of Narrative and Fiction",
            "Cultural Phylogenetics and Macroevolution",
            "Computational Humanities and Social Science"),
  fill_color = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#F0E442"), hjust = 0
)

#plot graph
plot_a <- ggraph(create_layout(network, layout = layout)) + 
  geom_edge_arc(aes(alpha = log(weight)), strength = 0.1) + 
  geom_node_point(aes(color = color)) + 
  geom_point_interactive(data = layout_df, aes(x = x, y = y, color = color, tooltip = label)) + 
  scale_edge_alpha(range = c(0, 0.05)) +
  scale_color_identity() +
  scale_x_continuous(limits = c(-0.1, 1.01), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.1, 1.01), expand = c(0, 0)) +
  theme_graph(base_family = "Helvetica") + 
  theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_b <- ggplot(freq_table) + 
  geom_point_interactive(aes(x = year, y = frequency, tooltip = label)) + 
  scale_x_continuous(breaks = seq(1900, 2020, 20)) +
  labs(x = "Year", y = "# Publications") + 
  theme_linedraw(base_family = "Helvetica") + 
  theme(panel.background = element_blank(), plot.background = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.margin = unit(c(0, 0, 0, 0), "pt"))
plot_c <- ggplot(labels_df) + 
  geom_label_interactive(aes(x = x, y = y, label = label, fill = fill_color, hjust = hjust), color = "white") + 
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_identity() + 
  theme_void(base_family = "Helvetica") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))

#https://patchwork.data-imaginist.com/reference/area.html
layout_patchwork <- c(
  area(1, 1, 12, 20),
  area(9, 1, 12, 5),
  area(1, 1, 12, 20)
)

#create static plot
plot <- free(plot_a) + free(plot_b) + free(plot_c) + plot_layout(design = layout_patchwork)

#export static plot
png("output/ce_art_biblio.png", width = 12, height = 6.5, units = "in", res = 600); plot; dev.off()
svg("output/ce_art_biblio.svg", width = 12, height = 6.5); plot; dev.off()

#export interactive plot
interactive_plot <- girafe(
  ggobj = plot, 
  fonts = list(sans = "Helvetica"), 
  width_svg = 12,
  height_svg = 6.5,
  options = list(
    opts_tooltip(css = "font-family: Arial, Helvetica, sans-serif; font-style: bold; background-color: black; color: white; padding: 10px; border-radius: 10px")
  )
)
save_html(interactive_plot, file = "output/ce_art_biblio.html")
