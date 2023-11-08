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
p_second_period <- readRDS(file = 'RDS_files/p_second_period')
p_third_period <- readRDS(file = 'RDS_files/p_third_period')
p_fourth_period <- readRDS(file = 'RDS_files/p_fourth_period')
p_fifth_period <- readRDS(file = 'RDS_files/p_fifth_period')

c_first_period <- readRDS(file = 'RDS_files/c_first_period')
c_second_period <- readRDS(file = 'RDS_files/c_second_period')
c_third_period <- readRDS(file = 'RDS_files/c_third_period')
c_fourth_period <- readRDS(file = 'RDS_files/c_fourth_period')
c_fifth_period <- readRDS(file = 'RDS_files/c_fifth_period')
  
contrib <- c("k00b",
               "kr",
               "ekzyis",
               "WeAreAllSatoshi",
               "rleed",
               "bitcoinplebdev",
               "benthecarman"
  )

#' Network Analysis
#' 
#' ## First Period

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

## Delete variables regarding the first period

rm(author_first_stacked,
   c_author_first_stacked,
   p_author_first_stacked, 
   coul,
   my_color, 
   nyms, 
   quartiles,
   c_first_period,
   p_first_period,
   final_p_first_posts_graph,
   g_first_posts,
   p_first_posts_graph
   )

#' 
#' 
#' ## Second Period
## ----------------------------------------------------------------------------------------------------------------------------

## Retrieve stacked amounts in the period for every user
p_author_second_stacked <- p_second_period[, .(Sats = sum(Sats)), by = .(Author)]
c_author_second_stacked <- c_second_period[, .(Sats = sum(Sats)), by = .(Author)]

author_second_stacked <- rbindlist(list(p_author_second_stacked, c_author_second_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
p_second_posts_graph <- copy(p_second_period)
p_second_posts_graph <- p_second_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_second_posts_graph <- merge(p_second_posts_graph, p_second_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_second_posts_graph <- p_second_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_second_posts_graph <- merge(p_second_posts_graph, c_second_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_second_posts_graph <- p_second_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_second_posts_graph <- p_second_posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_p_second_posts_graph <- final_p_second_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_second_posts <- igraph::simplify(graph_from_data_frame(final_p_second_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_second_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_second_posts <- set_vertex_attr(g_second_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_second_posts)$Sats <- author_second_stacked$Sats[match(V(g_second_posts)$name, author_second_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_second_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_second_posts)$catSats <- cut(author_second_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_second_posts)$name, author_second_stacked$Author)]

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
coul  <- brewer.pal(3, "Reds") 

my_color = coul[as.numeric(as.factor(V(g_second_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_second_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_second_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_second_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_second_posts)$catSats=="Q2"], alpha.f = 0.50)
my_color[V(g_second_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_second_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_second_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_second_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/second_period_graph.png')

plot(g_second_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()


## Delete variables regarding the second period

rm(author_second_stacked,
   c_author_second_stacked,
   p_author_second_stacked, 
   coul,
   my_color, 
   nyms, 
   quartiles,
   c_second_period,
   p_second_period,
   final_p_second_posts_graph,
   g_second_posts,
   p_second_posts_graph
)


#' 
#' ## Third Period
## ---------------------------------------------------------------------------------------------------------------------------

## Retrieve stacked amounts in the period for every user
p_author_third_stacked <- p_third_period[, .(Sats = sum(Sats)), by = .(Author)]
c_author_third_stacked <- c_third_period[, .(Sats = sum(Sats)), by = .(Author)]

author_third_stacked <- rbindlist(list(p_author_third_stacked, c_author_third_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
p_third_posts_graph <- copy(p_third_period)
p_third_posts_graph <- p_third_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_third_posts_graph <- merge(p_third_posts_graph, p_third_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_third_posts_graph <- p_third_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_third_posts_graph <- merge(p_third_posts_graph, c_third_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_third_posts_graph <- p_third_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_third_posts_graph <- p_third_posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_p_third_posts_graph <- final_p_third_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_third_posts <- igraph::simplify(graph_from_data_frame(final_p_third_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_third_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_third_posts <- set_vertex_attr(g_third_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_third_posts)$Sats <- author_third_stacked$Sats[match(V(g_third_posts)$name, author_third_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_third_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_third_posts)$catSats <- cut(author_third_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_third_posts)$name, author_third_stacked$Author)]

#' 
## ---------------------------------------------------------------------------------------------------------------------------------

coul  <- brewer.pal(3, "Reds") 

my_color = coul[as.numeric(as.factor(V(g_third_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_third_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_third_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q2"], alpha.f = 0.50)
my_color[V(g_third_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_third_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/third_period_graph.png')

plot(g_third_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_components,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()

## Delete variables regarding the third period

rm(author_third_stacked,
   c_author_third_stacked,
   p_author_third_stacked, 
   coul,
   my_color, 
   nyms, 
   quartiles,
   c_third_period,
   p_third_period,
   final_p_third_posts_graph,
   g_third_posts,
   p_third_posts_graph
)

#' 
#' ### Fourth Period
## --------------------------------------------------------------------------------------------------------------------------

## Retrieve stacked amounts in the period for every user
p_author_fourth_stacked <- p_fourth_period[, .(Sats = sum(Sats)), by = .(Author)]
c_author_fourth_stacked <- c_fourth_period[, .(Sats = sum(Sats)), by = .(Author)]

author_fourth_stacked <- rbindlist(list(p_author_fourth_stacked, c_author_fourth_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
p_fourth_posts_graph <- copy(p_fourth_period)
p_fourth_posts_graph <- p_fourth_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_fourth_posts_graph <- merge(p_fourth_posts_graph, p_fourth_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_fourth_posts_graph <- merge(p_fourth_posts_graph, c_fourth_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_p_fourth_posts_graph <- final_p_fourth_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_fourth_posts <- igraph::simplify(graph_from_data_frame(final_p_fourth_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_fourth_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_fourth_posts <- set_vertex_attr(g_fourth_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_fourth_posts)$Sats <- author_fourth_stacked$Sats[match(V(g_fourth_posts)$name, author_fourth_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_fourth_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_fourth_posts)$catSats <- cut(author_fourth_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_fourth_posts)$name, author_fourth_stacked$Author)]
#' 
#' ### Graph 
## -----------------------------------------------------------------------

coul  <- brewer.pal(3, "Reds")

my_color = coul[as.numeric(as.factor(V(g_fourth_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_fourth_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_fourth_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_fourth_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_fourth_posts)$catSats=="Q2"], alpha.f = 0.5)
my_color[V(g_fourth_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_fourth_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_fourth_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_fourth_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/fourth_period_graph.png')

plot(g_fourth_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_components,
     vertex.size = 4,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

dev.off()

## Delete variables regarding the fourth period

rm(author_fourth_stacked,
   c_author_fourth_stacked,
   p_author_fourth_stacked, 
   coul,
   my_color, 
   nyms, 
   quartiles,
   c_fourth_period,
   p_fourth_period,
   final_p_fourth_posts_graph,
   g_fourth_posts,
   p_fourth_posts_graph
)

#' 
#' ### Fifth Period
## --------------------------------------------------------------------------------------------------------------------------

## Retrieve stacked amounts in the period for every user
p_author_fifth_stacked <- p_fifth_period[, .(Sats = sum(Sats)), by = .(Author)]
c_author_fifth_stacked <- c_fifth_period[, .(Sats = sum(Sats)), by = .(Author)]

author_fifth_stacked <- rbindlist(list(p_author_fifth_stacked, c_author_fifth_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
p_fifth_posts_graph <- copy(p_fifth_period)

p_fifth_posts_graph[1, CommentsItemCode := NA]

p_fifth_posts_graph <- p_fifth_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_fifth_posts_graph <- merge(p_fifth_posts_graph, p_fifth_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_fifth_posts_graph <- merge(p_fifth_posts_graph, c_fifth_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_p_fifth_posts_graph <- final_p_fifth_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_fifth_posts <- igraph::simplify(graph_from_data_frame(final_p_fifth_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_fifth_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_fifth_posts <- set_vertex_attr(g_fifth_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_fifth_posts)$Sats <- author_fifth_stacked$Sats[match(V(g_fifth_posts)$name, author_fifth_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_fifth_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_fifth_posts)$catSats <- cut(author_fifth_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_fifth_posts)$name, author_fifth_stacked$Author)]
#' 
#' ### Graph 
## -----------------------------------------------------------------------

coul  <- brewer.pal(3, "Reds")

my_color = coul[as.numeric(as.factor(V(g_fifth_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_fifth_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_fifth_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_fifth_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_fifth_posts)$catSats=="Q2"], alpha.f = 0.5)
my_color[V(g_fifth_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_fifth_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_fifth_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_fifth_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/fifth_period_graph.png')

plot(g_fifth_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_components,
     vertex.size = 4,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

dev.off()

## Delete variables regarding the fifth period

rm(author_fifth_stacked,
   c_author_fifth_stacked,
   p_author_fifth_stacked, 
   coul,
   my_color, 
   nyms, 
   quartiles,
   c_fifth_period,
   p_fifth_period,
   final_p_fifth_posts_graph,
   g_fifth_posts,
   p_fifth_posts_graph
)
