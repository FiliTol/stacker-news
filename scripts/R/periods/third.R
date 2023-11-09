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


# Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')

p_third_period <- readRDS(file = 'RDS_files/p_third_period')

c_third_period <- readRDS(file = 'RDS_files/c_third_period')

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

g_third_posts <- graph_from_data_frame(final_p_third_posts_graph, directed = F)

g_third_posts <- igraph::simplify(g_third_posts, remove.multiple = T, remove.loops = T)

nyms <- ifelse(V(g_third_posts)$name %in% contrib, "contributor", "other")

# Add user type as attribute
g_third_posts <- set_vertex_attr(g_third_posts, "type", value = nyms)

# Add stacked amount as attribute
V(g_third_posts)$Sats <- author_third_stacked$Sats[match(V(g_third_posts)$name, author_third_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g_third_posts)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g_third_posts)$catSats <- cut(author_third_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g_third_posts)$name, author_third_stacked$Author)]

### Graph 

coul  <- brewer.pal(4, "Set1") 

my_color = coul[as.numeric(as.factor(V(g_third_posts)$catSats))]

# # Set alpha of vertex based on the stacked amount category
# my_color[V(g_third_posts)$catSats=="Q1"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q1"], alpha.f = 0.25)
# my_color[V(g_third_posts)$catSats=="Q2"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q2"], alpha.f = 0.50)
# my_color[V(g_third_posts)$catSats=="Q3"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q3"], alpha.f = 0.75)
# my_color[V(g_third_posts)$catSats=="Q4"] <- adjustcolor(my_color[V(g_third_posts)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/third/third_period_graph.png')

plot(g_third_posts,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=1,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_third_posts)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)

dev.off()

## -----------------------------------------------------------------------------
## Analysis of graph parameters and stats

### Degree of nodes and degree distribution
degree_tab <- data.table(author = V(g_third_posts)$name,
                         degr = degree(
                           g_third_posts, 
                           V(g_third_posts)$name,
                           loops = F
                         )
) %>%
  arrange(desc(degr))

degree_tab %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(degr), median = median(degr) )

# Plot degree distribution with log(x) scale
ggplot(data = degree_tab)+
  geom_histogram(aes(x = degr))

ggsave('images/third/degree_distribution.png')

degree_tab %>%
  filter(author %in% contrib)

##------------------------------------------------------------------------------
## Components

component_distribution(g_third_posts)

largest_component(g_third_posts)

count_components(g_third_posts)

components(g_third_posts)$csize

# Visualize only the big component
plot(decompose(g_third_posts)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=1,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g_third_posts)$catSats),
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
diameter(g_third_posts, directed = F, unconnected = T)

#### Diameter complete path
get_diameter(g_third_posts, directed = F, unconnected = T)

#### Diameter vertices and distance
farthest_vertices(g_third_posts, directed = F, unconnected = T)

mean_distance(g_third_posts, directed = F, unconnected = T)

#View(distances(g_third_posts))

# Takes upper triangle of the matrix (excluding diagonal entries)
distances_vector <- data.frame(distances = as.vector(distances(g_third_posts))[upper.tri(distances(g_third_posts))])

# Eliminate inf values, that are vertex couples not connected by any path
distances_vector <- data.frame(distances = distances_vector[is.finite(rowSums(distances_vector)),])

distances_vector %>%
  summarise(mean = mean(distances), median = median(distances))

###############################
## Degree of separation plot ##
###############################

ggplot(data = distances_vector)+
  geom_bar(aes(x = distances))

ggsave('images/third/degree_of_separation.png')
## -----------------------------------------------------------------------------
## Clustering and partitioning

betweenness(g_third_posts)

### Community detection algorithms aim to find the division of a network that maximizes its modularity
### Modularity ranges from -1 to 1:
### - Higher modularity score suggests a better division of the network into communities
### - Positive values indicate a good community structure
### - Negative values indicate that the network is not well divided into communities

#################################
## Edge betweenness clustering ##
#################################

#CommunityBetweenness <- cluster_edge_betweenness(g_third_posts)

#print(CommunityBetweenness)

# Clustering isolated 38 groups
# Modularity equal to 0.01 indicates a 'neutral' community structure, meaning
# that the network is barely divided into communities 


########################
## Louvian clustering ##
########################

CommunityLouvian <- cluster_louvain(g_third_posts)

print(CommunityLouvian)

# Clustering isolated 15
# Modularity equal to 0.2 indicates a considerably relevant community structure, meaning
# that the network is divided into communities 


#########################
## Walktrap clustering ##
#########################

CommunityWalktrap <- cluster_walktrap(g_third_posts)

print(CommunityWalktrap)

# Clustering isolated 755
# Modularity equal to 0.14 indicates a relevant community structure, meaning
# that the network is barely divided into communities


############################
## Fast Greedy clustering ##
############################

CommunityFastGreedy <- cluster_fast_greedy(g_third_posts)

print(CommunityFastGreedy)

# Clustering isolated 22
# Modularity equal to 0.2 indicates a considerably relevant community structure, meaning
# that the network is divided into communities 


####################################
## Leading Eigenvector clustering ##
####################################

CommunityLeadingEigenvector <- cluster_leading_eigen(g_third_posts)

print(CommunityLeadingEigenvector)

# Clustering isolated 11
# Mudularity equal to 0.13 indicates a relevant community structure, meaning
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














