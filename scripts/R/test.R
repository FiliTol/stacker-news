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

contrib <- c("k00b",
             "kr",
             "ekzyis",
             "WeAreAllSatoshi",
             "rleed",
             "bitcoinplebdev",
             "benthecarman"
)


## Retrieve stacked amounts in the period for every user
posts_author_stacked <- posts[, .(Sats = sum(Sats)), by = .(Author)]
comments_author_stacked <- comments[, .(Sats = sum(Sats)), by = .(Author)]

author_stacked <- rbindlist(list(posts_author_stacked, comments_author_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]


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

# Invert order because the directed link between the users is "comment->post"
final_graph <- total_graph[, .(Author2, Author1)]

# Compute weights based on the direction comment->post, that is Author2, Author1
final_graph <- final_graph[, .(weight = .N), by = .(Author2, Author1)] 

g <- graph_from_data_frame(final_graph, directed = T)

# Assign user type: contributor is a forum administrator and github mantainer
nyms <- ifelse(V(g)$name %in% contrib, "contributor", "other")
g <- set_vertex_attr(g, "type", value = nyms)

# Add stacked amount as attribute
V(g)$Sats <- author_stacked$Sats[match(V(g)$name, author_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g)$catSats <- cut(author_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g)$name, author_stacked$Author)]













