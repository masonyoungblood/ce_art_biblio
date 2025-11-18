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

#collect missing papers that oleg identified
collect_data <- bib2df("data/oleg_check/missing_papers.bib")
collect_data <- collect_data[, -c(which(colnames(collect_data) %in% c("AUTHOR", "EDITOR")))]
colnames(collect_data) <- tolower(colnames(collect_data))

#clean up and remove duplicates
collect_data$title <- tolower(collect_data$title)
collect_data <- unique(collect_data)

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

#manually add the missing ones
works[[2]] <- list(oa_fetch(entity = "works", identifier = "https://openalex.org/W2973262226", output = "list"))
works[[19]] <- list(oa_fetch(entity = "works", identifier = "https://openalex.org/W1972390081", output = "list"))
works[[22]] <- list(oa_fetch(entity = "works", identifier = "https://openalex.org/W2097315895", output = "list"))

#load in old works file and combine
new_works <- works
load("data/works.RData")
works <- c(works, new_works)
rm(new_works)

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

#get rid of irrelevant works
oleg_check <- read.csv("data/oleg_check/works_proc_oleg.csv")
works_proc <- works_proc[-which(works_proc$id %in% oleg_check$id[which(oleg_check$to_keep == 0)]), ]

#save the works object
save(works_proc, file = "data/works_proc_final.RData")
