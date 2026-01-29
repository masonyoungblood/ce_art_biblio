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
for(x in 1:nrow(collect_data)){
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
