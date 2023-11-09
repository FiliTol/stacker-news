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

# Network Analysis

## Retrieve stacked amounts in the period for every user
posts_author_stacked <- posts[, .(Sats = sum(Sats)), by = .(Author)]
comments_author_stacked <- comments[, .(Sats = sum(Sats)), by = .(Author)]

author_stacked <- rbindlist(list(posts_author_stacked, comments_author_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]

# Create table for graph generation 
posts_graph <- copy(posts)
posts_graph <- posts_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

posts_graph <- merge(posts_graph, posts, by.x = 'ItemCode', by.y = 'ItemCode')

posts_graph <- posts_graph[, .(PostItemCode = ItemCode, CommentItemCode = Commentors, PostAuthor = Author)]

posts_graph <- merge(posts_graph, comments, by.x = 'CommentItemCode', by.y = 'ItemCode')

posts_graph <- posts_graph[, .(PostItemCode, CommentItemCode, PostAuthor, CommentAuthor = Author)]

final_posts_graph <- posts_graph[, .(PostAuthor, CommentAuthor)]

# Computing and inserting weights as the number of interactions between users
final_posts_graph <- final_posts_graph[, .(weight = .N), by = .(Author1 = pmax(PostAuthor, CommentAuthor), Author2 = pmin(PostAuthor, CommentAuthor))]

g_posts <- graph_from_data_frame(final_posts_graph, directed = F)

g_posts <- igraph::simplify(g_posts, remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_posts <- set_vertex_attr(g_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_posts)$Sats <- author_stacked$Sats[match(V(g_posts)$name, author_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_posts)$catSats <- cut(author_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_posts)$name, author_stacked$Author)]

### Graph 

coul  <- brewer.pal(4, "Set1") 

my_color = coul[as.numeric(as.factor(V(g_posts)$catSats))]

# Set alpha of vertex based on the stacked amount category
my_color[V(g_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q1"], alpha.f = 0.25)
my_color[V(g_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q2"], alpha.f = 0.5)
my_color[V(g_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q3"], alpha.f = 0.75)
my_color[V(g_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/general_graph.png')

plot(g_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=1,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)

dev.off()

## -----------------------------------------------------------------------------
## Analysis of graph parameters and stats

### Degree of nodes and degree distribution
degree_tab <- data.table(author = V(g_posts)$name,
                         degr = degree(
                           g_posts, 
                           V(g_posts)$name,
                           loops = F
                         )
) %>%
  arrange(desc(degr))

degree_tab %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(degr), median = median(degr))

# Plot degree distribution with log(x) scale
ggplot(data = degree_tab)+
  geom_histogram(aes(x = degr))

ggsave('images/general_degree_distribution.png')

degree_tab %>%
  filter(author %in% contrib)

##------------------------------------------------------------------------------
## Components

component_distribution(g_posts)

largest_component(g_posts)

count_components(g_posts)

components(g_posts)$csize

######################################
## Visualize only the big component ##
######################################

## First quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_posts)$catSats))]
my_color[V(g_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q1"], alpha.f = 1)
my_color[V(g_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/general_Q1.png')
plot(decompose(g_posts)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()

## Second quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_posts)$catSats))]
my_color[V(g_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q2"], alpha.f = 1)
my_color[V(g_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/general_Q2.png')
plot(decompose(g_posts)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()

## Third quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_posts)$catSats))]
my_color[V(g_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q3"], alpha.f = 1)
my_color[V(g_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/general_Q3.png')
plot(decompose(g_posts)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()

## Fourth quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g_posts)$catSats))]
my_color[V(g_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/general_Q4.png')
plot(decompose(g_posts)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)


## -----------------------------------------------------------------------------
## Path

## NB: in a weighted graph distances (and so diameter) are computed using the sum
## of weights. Therefore diameter=12 is the sum of the weights along the edges 

### Diameter
diameter(g_posts, directed = F, unconnected = T)

#### Diameter complete path
get_diameter(g_posts, directed = F, unconnected = T)

#### Diameter vertices and distance
farthest_vertices(g_posts, directed = F, unconnected = T)

mean_distance(g_posts, directed = F, unconnected = T)

#View(distances(g_posts))

# Takes upper triangle of the matrix (excluding diagonal entries)
distances_vector <- data.frame(distances = as.vector(distances(g_posts))[upper.tri(distances(g_posts))])

# Eliminate inf values, that are vertex couples not connected by any path
distances_vector <- data.frame(distances = distances_vector[is.finite(rowSums(distances_vector)),])

distances_vector %>%
  summarise(mean = mean(distances), median = median(distances))

###############################
## Degree of separation plot ##
###############################

ggplot(data = distances_vector)+
  geom_bar(aes(x = distances))

ggsave('images/general_degree_of_separation.png')
## -----------------------------------------------------------------------------
## Clustering and partitioning

betweenness(g_posts)

### Community detection algorithms aim to find the division of a network that maximizes its modularity
### Modularity ranges from -1 to 1:
### - Higher modularity score suggests a better division of the network into communities
### - Positive values indicate a good community structure
### - Negative values indicate that the network is not well divided into communities

#################################
## Edge betweenness clustering ##
#################################

# CommunityBetweenness <- cluster_edge_betweenness(g_posts)
# 
# print(CommunityBetweenness)

# Clustering isolated 36 groups
# Mudularity equal to 0.05 indicates a 'neutral' community structure, meaning
# that the network is barely divided into communities 


########################
## Louvian clustering ##
########################

CommunityLouvian <- cluster_louvain(g_posts)

print(CommunityLouvian)

# Clustering isolated 27
# Mudularity equal to 0.25 indicates a relevant community structure, meaning
# that the network is divided into communities 


#########################
## Walktrap clustering ##
#########################

CommunityWalktrap <- cluster_walktrap(g_posts)

print(CommunityWalktrap)

# Clustering isolated 2623
# Mudularity equal to 0.1 indicates a 'neutral' community structure, meaning
# that the network is barely divided into communities


############################
## Fast Greedy clustering ##
############################

CommunityFastGreedy <- cluster_fast_greedy(g_posts)

print(CommunityFastGreedy)

# Clustering isolated 25
# Mudularity equal to 0.24 indicates a relevant community structure, meaning
# that the network is divided into communities 


####################################
## Leading Eigenvector clustering ##
####################################

CommunityLeadingEigenvector <- cluster_leading_eigen(g_posts)

print(CommunityLeadingEigenvector)

# Clustering isolated 15
# Mudularity equal to 0.16 indicates a relevant community structure, meaning
# that the network is divided into communities 

# plot_dendrogram(CommunityBetweenness)
# plot_dendrogram(CommunityFastGreedy)
# plot_dendrogram(CommunityLeadingEigenvector)
# plot_dendrogram(CommunityWalktrap)

## -----------------------------------------------------------------------------
## Comparing community structures

### Using Variation of Information (VI)
### Measures the information lost and gained when moving from one partition to 
### another. Lower values indicate better similarity.

compare(CommunityLouvian,
        CommunityFastGreedy,
        method = c("vi")
)

compare(CommunityLeadingEigenvector,
        CommunityFastGreedy,
        method = c("vi")
)

compare(CommunityLeadingEigenvector,
        CommunityLouvian,
        method = c("vi")
)

### Using Normalized Mutual Information (NMI)
### Measures the mutual information between two partitions, normalized by the entropy
### of each partition. It ranges from 0 (no similarity) to 1 (perfect similarity).

compare(CommunityLouvian,
        CommunityFastGreedy,
        method = c("nmi")
)

compare(CommunityLeadingEigenvector,
        CommunityFastGreedy,
        method = c("nmi")
)

compare(CommunityLeadingEigenvector,
        CommunityLouvian,
        method = c("nmi")
)














