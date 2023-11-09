library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)
library(RColorBrewer)

#' # Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')

p_first_period <- readRDS(file = 'RDS_files/p_first_period')

c_first_period <- readRDS(file = 'RDS_files/c_first_period')

contrib <- c("k00b",
             "kr",
             "ekzyis",
             "WeAreAllSatoshi",
             "rleed",
             "bitcoinplebdev",
             "benthecarman"
)

# Network Analysis

## Retrieve stacked amounts in the period for every user
p_author_first_stacked <- p_first_period[, .(Sats = sum(Sats)), by = .(Author)]
c_author_first_stacked <- c_first_period[, .(Sats = sum(Sats)), by = .(Author)]

author_first_stacked <- rbindlist(list(p_author_first_stacked, c_author_first_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
p_first_posts_graph <- copy(p_first_period)
p_first_posts_graph <- p_first_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_first_posts_graph <- merge(p_first_posts_graph, p_first_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_first_posts_graph <- p_first_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_first_posts_graph <- merge(p_first_posts_graph, c_first_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_first_posts_graph <- p_first_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_first_posts_graph <- p_first_posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_p_first_posts_graph <- final_p_first_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_first_posts <- graph_from_data_frame(final_p_first_posts_graph, directed = F)

g_first_posts <- igraph::simplify(g_first_posts, remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_first_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_first_posts <- set_vertex_attr(g_first_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_first_posts)$Sats <- author_first_stacked$Sats[match(V(g_first_posts)$name, author_first_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_first_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_first_posts)$catSats <- cut(author_first_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_first_posts)$name, author_first_stacked$Author)]

### Graph 

coul  <- brewer.pal(3, "Reds") 

my_color = coul[as.numeric(as.factor(V(g_first_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_first_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_first_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_first_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_first_posts)$catSats=="Q2"], alpha.f = 0.50)
my_color[V(g_first_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_first_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_first_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_first_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/first_period_graph.png')

plot(g_first_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()
