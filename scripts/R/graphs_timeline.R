library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
library(lubridate)
library(tidyverse)

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

#' Network Analysis
#' 
#' ## First Period
## ---------------------------------------------------------------------------------------------------------------------------------
first_graph <- copy(p_first_period)
first_graph <- first_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]
first_graph <- merge(first_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')
first_graph <- first_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]
first_graph <- merge(first_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')
first_graph <- first_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]
first_graph <- first_graph[, .(PostAuthor, CommentAuthor)]

#' 
#' ### Graph 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g <- graph_from_data_frame(first_graph, directed = F)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g1=igraph::simplify(g, remove.multiple = T, remove.loops = T)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
plot( g1,vertex.label=NA,
      edge.width = 1,
      edge.arrow.width = 0.3,
      edge.arrow.size = 0.5,
      vertex.label.cex = 1,
      asp = 0.35,
      margin = -0.1)

#' 
#' 
#' ## Second Period
## ---------------------------------------------------------------------------------------------------------------------------------
second_graph <- copy(p_second_period)
second_graph <- second_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]
second_graph <- merge(second_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')
second_graph <- second_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]
second_graph <- merge(second_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')
second_graph <- second_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]
second_graph <- second_graph[, .(PostAuthor, CommentAuthor)]

#' 
#' ### Graph 
## ---------------------------------------------------------------------------------------------------------------------------------
g2 <- graph_from_data_frame(second_graph, directed = F)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g2=igraph::simplify(g2, remove.multiple = T, remove.loops = T)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
plot( g2,vertex.label=NA,
      edge.width = 1,
      edge.arrow.width = 0.3,
      edge.arrow.size = 0.5,
      vertex.label.cex = 1,
      asp = 0.35,
      margin = -0.1)

#' 
#' ## Third Period
## ---------------------------------------------------------------------------------------------------------------------------------
third_graph <- copy(p_third_period)
third_graph <- third_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]
third_graph <- merge(third_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')
third_graph <- third_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]
third_graph <- merge(third_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')
third_graph <- third_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]
third_graph <- third_graph[, .(PostAuthor, CommentAuthor)]

#' 
#' ### Graph 
## ---------------------------------------------------------------------------------------------------------------------------------
g3 <- graph_from_data_frame(third_graph, directed = F)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g3=igraph::simplify(g3, remove.multiple = T, remove.loops = T)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
plot( g3,vertex.label=NA,
      edge.width = 1,
      edge.arrow.width = 0.3,
      edge.arrow.size = 0.5,
      vertex.label.cex = 1,
      asp = 0.35,
      margin = -0.1)

#' 
#' ### Fourth Period
## ---------------------------------------------------------------------------------------------------------------------------------
fourth_graph <- copy(p_fourth_period)
fourth_graph <- fourth_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]
fourth_graph <- merge(fourth_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')
fourth_graph <- fourth_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]
fourth_graph <- merge(fourth_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')
fourth_graph <- fourth_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]
fourth_graph <- fourth_graph[, .(PostAuthor, CommentAuthor)]

#' 
#' ### Graph 
## ---------------------------------------------------------------------------------------------------------------------------------
g4 <- graph_from_data_frame(fourth_graph, directed = F)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g4=igraph::simplify(g4, remove.multiple = T, remove.loops = T)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
plot( g4,vertex.label=NA,
      edge.width = 1,
      edge.arrow.width = 0.3,
      edge.arrow.size = 0.5,
      vertex.label.cex = 1,
      asp = 0.35,
      margin = -0.1)

#' 
#' ### Fifth Period
## ---------------------------------------------------------------------------------------------------------------------------------
fifth_graph <- copy(p_fifth_period)
fifth_graph <- fifth_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]
fifth_graph <- merge(fifth_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')
fifth_graph <- fifth_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]
fifth_graph <- merge(fifth_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')
fifth_graph <- fifth_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]
fifth_graph <- fifth_graph[, .(PostAuthor, CommentAuthor)]

#' 
#' ### Graph 
## ---------------------------------------------------------------------------------------------------------------------------------
g5 <- graph_from_data_frame(fifth_graph, directed = F)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
g5=igraph::simplify(g5, remove.multiple = T, remove.loops = T)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
plot( g5,vertex.label=NA,
      edge.width = 1,
      edge.arrow.width = 0.3,
      edge.arrow.size = 0.5,
      vertex.label.cex = 1,
      asp = 0.35,
      margin = -0.1)

