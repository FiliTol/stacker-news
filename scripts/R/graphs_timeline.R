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
## -------------------------------------------------------------------------
p_first_posts_graph <- copy(p_first_period)
p_first_posts_graph <- p_first_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_first_posts_graph <- merge(p_first_posts_graph, p_first_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_first_posts_graph <- p_first_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_first_posts_graph <- merge(p_first_posts_graph, c_first_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_first_posts_graph <- p_first_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_first_posts_graph <- p_first_posts_graph[, .(PostAuthor, CommentAuthor)]

g_first_posts <- graph_from_data_frame(final_p_first_posts_graph, directed = F)

g_first_posts <- igraph::simplify(g_first_posts, remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_first_posts)$name %in% contrib, "contributor", "other")

g_first_posts <- set_vertex_attr(g_first_posts, "type", value = nyms)

### Graph 

coul  <- brewer.pal(3, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_first_posts)$type))]

my_color[V(g_first_posts)$type=="other"] <- adjustcolor(my_color[V(g_first_posts)$type=="other"], alpha.f = 0.2)

png(filename = 'images/first_period_graph.png')

plot(g_first_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_kk,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()

#' 
#' 
#' ## Second Period
## ---------------------------------------------------------------------------------------------------------------------------------
p_second_posts_graph <- copy(p_second_period)
p_second_posts_graph <- p_second_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_second_posts_graph <- merge(p_second_posts_graph, p_second_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_second_posts_graph <- p_second_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_second_posts_graph <- merge(p_second_posts_graph, c_second_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_second_posts_graph <- p_second_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_second_posts_graph <- p_second_posts_graph[, .(PostAuthor, CommentAuthor)]

g_second_posts <- igraph::simplify(graph_from_data_frame(final_p_second_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_second_posts)$name %in% contrib, "contributor", "other")

g_second_posts <- set_vertex_attr(g_second_posts, "type", value = nyms)
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
coul  <- brewer.pal(3, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_second_posts)$type))]

my_color[V(g_second_posts)$type=="other"] <- adjustcolor(my_color[V(g_second_posts)$type=="other"], alpha.f = 0.2)

png(filename = 'images/second_period_graph.png')

plot(g_second_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_kk,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()

#' 
#' ## Third Period
## ---------------------------------------------------------------------------------------------------------------------------------
p_third_posts_graph <- copy(p_third_period)
p_third_posts_graph <- p_third_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_third_posts_graph <- merge(p_third_posts_graph, p_third_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_third_posts_graph <- p_third_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_third_posts_graph <- merge(p_third_posts_graph, c_third_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_third_posts_graph <- p_third_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_third_posts_graph <- p_third_posts_graph[, .(PostAuthor, CommentAuthor)]

g_third_posts <- igraph::simplify(graph_from_data_frame(final_p_third_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_third_posts)$name %in% contrib, "contributor", "other")

g_third_posts <- set_vertex_attr(g_third_posts, "type", value = nyms)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------

coul  <- brewer.pal(3, "Set1")
my_color = coul[as.numeric(as.factor(V(g_third_posts)$type))]

my_color[V(g_third_posts)$type=="other"] <- adjustcolor(my_color[V(g_third_posts)$type=="other"], alpha.f = 0.2)

png(filename = 'images/third_period_graph.png')

plot(g_third_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout=layout_with_kk,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()

#' 
#' ### Fourth Period
## ---------------------------------------------------------------------------------------------------------------------------------
p_fourth_posts_graph <- copy(p_fourth_period)
p_fourth_posts_graph <- p_fourth_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_fourth_posts_graph <- merge(p_fourth_posts_graph, p_fourth_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_fourth_posts_graph <- merge(p_fourth_posts_graph, c_fourth_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_fourth_posts_graph <- p_fourth_posts_graph[, .(PostAuthor, CommentAuthor)]

g_fourth_posts <- igraph::simplify(graph_from_data_frame(final_p_fourth_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_fourth_posts)$name %in% contrib, "contributor", "other")

g_fourth_posts <- set_vertex_attr(g_fourth_posts, "type", value = nyms)
#' 
#' ### Graph 
## -----------------------------------------------------------------------
coul  <- brewer.pal(3, "Set1")
my_color = coul[as.numeric(as.factor(V(g_fourth_posts)$type))]

my_color[V(g_fourth_posts)$type=="other"] <- adjustcolor(my_color[V(g_fourth_posts)$type=="other"], alpha.f = 0.2)

png(filename = 'images/fourth_period_graph.png')

plot(g_fourth_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_kk,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()

#' 
#' ### Fifth Period
## ---------------------------------------------------------------------------------------------------------------------------------
p_fifth_posts_graph <- copy(p_fifth_period)

p_fifth_posts_graph[1, CommentsItemCode := NA]

p_fifth_posts_graph <- p_fifth_posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

p_fifth_posts_graph <- merge(p_fifth_posts_graph, p_fifth_period, by.x = 'ItemCode', by.y = 'ItemCode')

p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

p_fifth_posts_graph <- merge(p_fifth_posts_graph, c_fifth_period, by.x = 'CommentItemCode', by.y = 'ItemCode')

p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_p_fifth_posts_graph <- p_fifth_posts_graph[, .(PostAuthor, CommentAuthor)]

g_fifth_posts <- igraph::simplify(graph_from_data_frame(final_p_fifth_posts_graph, directed = F), remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_fifth_posts)$name %in% contrib, "contributor", "other")

g_fifth_posts <- set_vertex_attr(g_fifth_posts, "type", value = nyms)

#' 
#' ### Graph 
## ---------------------------------------------------------------------------------------------------------------------------------
coul  <- brewer.pal(3, "Set1")
my_color = coul[as.numeric(as.factor(V(g_fifth_posts)$type))]

my_color[V(g_fifth_posts)$type=="other"] <- adjustcolor(my_color[V(g_fifth_posts)$type=="other"], alpha.f = 0.2)

png(filename = 'images/fifth_period_graph.png')

plot(g_fifth_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_kk,
     vertex.size=4,
     edge.width=0.5,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

dev.off()
