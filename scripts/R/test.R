library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(RColorBrewer)

# Used for community detection
library(gmp)
library(slam)

#' # Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')

# Let's keep only the variables that are present in both posts and comments

clean_posts <- subset(posts, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))
clean_comments <- subset(comments, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))

total_items <- rbindlist(list(clean_posts, clean_comments),
                         use.names = T,
                         fill = TRUE) %>%
  arrange(as.numeric(ItemCode))

# Reproduce process of graph creation
total_graph <- copy(total_items)
total_graph <- total_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

total_graph <- merge(total_graph, total_items, by.x = 'ItemCode', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = ItemCode, Item2 = Commentors, Author1 = Author)]

total_graph <- merge(total_graph, total_items, by.x = 'Item2', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = Item1, Item2 = Item2, Author1 = Author1, Author2 = Author)]

final_graph <- total_graph[, .(Author1, Author2)]

### final_graph <- final_graph[, .(weight = .N), by = .(Author1 = pmax(Author1, Author2), Author2 = pmin(Author1, Author2))] 

g <- graph_from_data_frame(final_graph, directed = T)

edge_counts <- count_multiple(g, eids = E(g))

E(g)$weight <- edge_counts

g <- igraph::simplify(g, remove.loops = T)

shortest_paths(g, from = 'k00b', to = 'DarthCoin')

diameter(g)

