library(data.table)
library(igraph)
library(ggplot2)
#library(stringr)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)

# Used for community detection
library(gmp)
library(slam)

# Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')

p_fourth_period <- readRDS(file = 'RDS_files/p_fourth_period')

c_fourth_period <- readRDS(file = 'RDS_files/c_fourth_period')

contrib <- c("k00b",
             "kr",
             "ekzyis",
             "WeAreAllSatoshi",
             "rleed",
             "bitcoinplebdev",
             "benthecarman"
)


## Retrieve stacked amounts in the period for every user
posts_author_stacked <- p_fourth_period[, .(Sats = sum(Sats)), by = .(Author)]
comments_author_stacked <- c_fourth_period[, .(Sats = sum(Sats)), by = .(Author)]

author_stacked <- rbindlist(list(posts_author_stacked, comments_author_stacked))[, lapply(.SD, sum, na.rm = TRUE), by = Author]


# Let's keep only the variables that are present in both posts and comments
clean_posts <- subset(p_fourth_period, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))
clean_comments <- subset(c_fourth_period, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))

total_items <- rbindlist(list(clean_posts, clean_comments),
                         use.names = T,
                         fill = TRUE) %>%
  arrange(as.numeric(ItemCode))


## ----------------------------------------------------------------------------
## Data frame creation
total_graph <- copy(total_items)
total_graph[1, CommentsItemCode := NA]
total_graph <- total_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

total_graph <- merge(total_graph, total_items, by.x = 'ItemCode', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = ItemCode, Item2 = Commentors, Author1 = Author)]

total_graph <- merge(total_graph, total_items, by.x = 'Item2', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = Item1, Item2 = Item2, Author1 = Author1, Author2 = Author)]

# Invert order because the directed link between the users is "comment->post"
final_graph <- total_graph[, .(Author2, Author1)]

# Compute weights based on the direction comment->post, that is Author2, Author1
final_graph <- final_graph[, .(weight = .N), by = .(Author2, Author1)] 

g <- igraph::simplify(graph_from_data_frame(final_graph, directed = T), remove.multiple = T, remove.loops = T)

# Assign user type: contributor is a forum administrator and github mantainer
nyms <- ifelse(V(g)$name %in% contrib, "contributor", "other")
g <- set_vertex_attr(g, "type", value = nyms)

# Add stacked amount as attribute
V(g)$Sats <- author_stacked$Sats[match(V(g)$name, author_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g)$Sats, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g)$catSats <- cut(author_stacked$Sats, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g)$name, author_stacked$Author)]


## ---------------------------------------------------------------------------

### Graph 

coul  <- brewer.pal(4, "Set1") 

my_color = coul[as.numeric(as.factor(V(g)$catSats))]

# # Set alpha of vertex based on the stacked amount category
# my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 0.25)
# my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.5)
# my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.75)
# my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/directed/fourth/general_graph.png')

plot(g,
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_fr,
     vertex.size=4,
     edge.width=1,
     edge.color="lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)

dev.off()


## -----------------------------------------------------------------------------
## Analysis of graph parameters and stats

### In-degree, out-degree and total degree of nodes and degree distribution
degree_tab <- data.table(author = V(g)$name,
                         in_degr = degree(
                           g, 
                           V(g)$name,
                           loops = F,
                           mode = "in"
                         ),
                         out_degr = degree(
                           g,
                           V(g)$name,
                           loops = F,
                           mode = "out"
                         ),
                         tot_degr = degree(
                           g, 
                           V(g)$name,
                           loops = F,
                           mode = "total"
                         )
)

## General overview - tot_degr ranking
degree_tab %>%
  arrange(desc(tot_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(tot_degr), median = median(tot_degr))

ggplot(data = degree_tab)+
  geom_histogram(aes(x = tot_degr))

ggsave('images/directed/fourth/general_total_degree_distribution.png')

## General overview - in_degr ranking
degree_tab %>%
  arrange(desc(in_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(in_degr), median = median(in_degr))

# Plot degree distribution with log(x) scale
ggplot(data = degree_tab)+
  geom_histogram(aes(x = in_degr))

## General overview - out_degr ranking
degree_tab %>%
  arrange(desc(out_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(out_degr), median = median(out_degr))

# Plot degree distribution with log(x) scale
ggplot(data = degree_tab)+
  geom_histogram(aes(x = out_degr))

degree_tab %>%
  filter(author %in% contrib) %>%
  ggplot(aes(x = in_degr, y = out_degr))+
  geom_point()+
  geom_label_repel(aes(label = author),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(x = "In-degree", y = " Out-degree",
       title = "Forum contributors in-degree and out-degree")+
  theme_classic()

ggsave('images/directed/fourth/general_contributors_degree.png')


##----------------------------------------------------------------------------
## Components

component_distribution(g)

largest_component(g)

count_components(g)

components(g)$csize

######################################
## Visualize only the big component ##
######################################


## First quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g)$catSats))]
my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 1)
my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/directed/fourth/general_Q1.png')
plot(decompose(g)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_lgl,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()


## Second quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g)$catSats))]
my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 1)
my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/directed/fourth/general_Q2.png')
plot(decompose(g)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_lgl,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()


## Third quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g)$catSats))]
my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 1)
my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/directed/fourth/general_Q3.png')
plot(decompose(g)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_lgl,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()


## Fourth quartile

coul  <- brewer.pal(4, "Set1") 
my_color = coul[as.numeric(as.factor(V(g)$catSats))]
my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/directed/fourth/general_Q4.png')
plot(decompose(g)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_lgl,
     vertex.size = 2,
     edge.width = 0.5,
     edge.color = "lightgrey",
     vertex.frame.width = 0
)

legend('topright',
       legend = unique(V(g)$catSats),
       fill = unique(my_color),
       title = "Quartiles",
       bty = "n",
       inset = c(0.02, 0.02)
)
dev.off()


## -----------------------------------------------------------------------------
## Path

## NB: in a weighted graph distances (and so diameter) are computed using the sum
## of weights. Therefore diameter is the sum of the weights along the edges 

### Diameter
diameter(g, directed = T, unconnected = T)

#### Diameter complete path
get_diameter(g, directed = T, unconnected = T)

#### Diameter vertices and distance
farthest_vertices(g, directed = T, unconnected = T)

mean_distance(g, directed = T, unconnected = T)

#View(distances(g))

# Takes upper triangle of the matrix (excluding diagonal entries)
distances_vector <- data.frame(distances = as.vector(distances(g))[upper.tri(distances(g))])

# Eliminate inf values, that are vertex couples not connected by any path
distances_vector <- data.frame(distances = distances_vector[is.finite(rowSums(distances_vector)),])

distances_vector %>%
  summarise(mean = mean(distances), median = median(distances))

###############################
## Degree of separation plot ##
###############################

ggplot(data = distances_vector)+
  geom_bar(aes(x = distances))

ggsave('images/directed/fourth/general_degree_of_separation.png')
















