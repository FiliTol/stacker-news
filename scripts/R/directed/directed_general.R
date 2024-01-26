library(data.table)
library(igraph)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
#install.packages("tidyverse")
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(Rglpk)
library(viridisLite)

# Used for community detection
library(gmp)
library(slam)

set.seed(879597)

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


## Extract the total stacked from each user by looking at the 'users' table
## Left join between the tables to extract also the rewards + forwarded amounts

author_stacked <- merge(author_stacked, users, by.x = 'Author', by.y = 'User', all.x = T) %>%
  select(Author, Sats, TotalStacked)
author_stacked[, TotalStacked := ifelse(is.na(TotalStacked), Sats, TotalStacked )]

## If an user has a 'reward' amount smaller than 0 it means that he/she is a forwarder, meaning that he/she created posts in which the stacked amount was intentionally forwarded to other users. This is a forum feature that indicates a strong willingness to be a collaborative and active user.
author_stacked[, rewards := TotalStacked - Sats]

# Let's keep only the variables that are present in both posts and comments
clean_posts <- subset(posts, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))
clean_comments <- subset(comments, select = c(ItemCode, Sats, Author, Timestamp, CommentsItemCode))

total_items <- rbindlist(list(clean_posts, clean_comments),
                         use.names = T,
                         fill = TRUE) %>%
  arrange(as.numeric(ItemCode))

## ----------------------------------------------------------------------------
## Data frame creation
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

g <- igraph::simplify(graph_from_data_frame(final_graph, directed = T), remove.multiple = T, remove.loops = T)

# Assign user type: contributor is a forum administrator and github mantainer
nyms <- ifelse(V(g)$name %in% contrib, "contributor", "other")
g <- set_vertex_attr(g, "type", value = nyms)

# Add stacked amount as attribute
V(g)$Sats <- author_stacked$Sats[match(V(g)$name, author_stacked$Author)]
V(g)$TotStacked <- author_stacked$TotalStacked[match(V(g)$name, author_stacked$Author)]
V(g)$Rewards <- author_stacked$rewards[match(V(g)$name, author_stacked$Author)]

# Add categorical amount by dividing the users according to the quartiles of Sats distribution
quartiles <- quantile(V(g)$TotStacked, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

V(g)$catSats <- cut(author_stacked$TotalStacked, breaks = quartiles, labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE)[match(V(g)$name, author_stacked$Author)]

## ---------------------------------------------------------------------------

### Graph 

coul  <- brewer.pal(4, "Set1") 

my_color = coul[as.numeric(as.factor(V(g)$catSats))]

# # Set alpha of vertex based on the stacked amount category
# my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 0.25)
# my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.5)
# my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.75)
# my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 1)

png(filename = 'images/directed/general/general_graph.png')

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

### Creation of a table that contains all the attribute values for every vertex
### (node) in the general graph.
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
                         ),
                         stacked = V(g)$Sats,
                         totstacked = V(g)$TotStacked,
                         rewards = V(g)$Rewards,
                         role = V(g)$type
)
                         
## General overview - tot_degr ranking
degree_tab %>%
  select(author, tot_degr, in_degr, out_degr) %>%
  arrange(desc(tot_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(tot_degr), median = median(tot_degr))

ggplot(data = degree_tab, aes(x = tot_degr)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  scale_y_sqrt() +
  labs(x = "Total degree", y = "Density",
       title = "Degree Distribution",
       subtitle = "sqrt scaled values") +
  theme_classic() +
  theme(text = element_text(size = 14))

#ggplot(data = degree_tab, aes(x = tot_degr)) +
#  stat_density(aes(y = after_stat(..density..)), geom = "point", color = "blue", size = 3, alpha = 1)+
#  scale_y_sqrt() +
#  labs(x = "Total degree", y = "density",
#       title = "Degree distribution",
#       subtitle = "sqrt scaled values")+
#  theme_classic()+
#  theme(text = element_text(size = 14))

ggsave('images/directed/general/general_total_degree_distribution.png', width = 6, height = 4)

## General overview - in_degr ranking
degree_tab %>%
  select(author, tot_degr, in_degr, out_degr) %>%
  arrange(desc(in_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(in_degr), median = median(in_degr))

# Plot degree distribution with log(x) scale
ggplot(data = degree_tab)+
  geom_histogram(aes(x = in_degr))

## General overview - out_degr ranking
degree_tab %>%
  select(author, tot_degr, in_degr, out_degr) %>%
  arrange(desc(out_degr)) %>%
  head(10)

degree_tab %>%
  summarise(mean = mean(out_degr), median = median(out_degr))

# Plot degree distribution of contributors
ggplot(data = degree_tab)+
  geom_histogram(aes(x = out_degr))

degree_tab %>%
  filter(role=='contributor') %>%
  select(author, tot_degr, in_degr, out_degr, stacked, totstacked, rewards) %>%
  arrange(desc(tot_degr))

degree_tab %>%
  filter(author %in% contrib) %>%
  ggplot(aes(x = in_degr, y = out_degr))+
  geom_point(aes(size = totstacked))+
  geom_label_repel(aes(label = author),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  labs(x = "In-degree", y = " Out-degree",
       title = "Forum contributors in-degree and out-degree")+
  theme_classic()

ggsave('images/directed/general/general_contributors_degree.png')


##############################################################################
# Plot degree distribution of all users
# degree_tab %>%
#   filter(totstacked>1000000) %>%
#   #mutate(normalized = (totstacked - mean(totstacked)) / sd(totstacked) ) %>%
#   ggplot(aes(x = in_degr, y = out_degr))+
#   geom_point(aes(size = totstacked,
#                  #alpha = normalized,
#                  color = role))+
#   geom_label_repel(aes(
#     label = ifelse(totstacked>1000000, author, '')),
#     force = 1,
#     box.padding   = 5, 
#     point.padding = 0.5,
#     segment.color = 'grey50',
#     max.overlaps = 6000)+
#   labs(x = "In-degree", y = " Out-degree",
#        title = "Forum users in-degree and out-degree",
#        subtitle = "Users with more than 1mln sats stacked")+
#   theme_classic()

### Probability table for in-degree
# This shows the fact that only 1% of the users have a in-degree bigger than 10
sum(ifelse(degree_tab$in_degr > 10, 1, 0))/length(degree_tab$in_degr)

### Probability table for out-degree
# This shows the fact that only 1% of users have an out-degree bigger than 10
sum(ifelse(degree_tab$out_degr > 10, 1, 0))/length(degree_tab$out_degr)

### Probability table for total_degree
# This shows that only 5.7% of users scored +50 degree in both in and out degree
sum(ifelse(degree_tab$out_degr > 10 & degree_tab$in_degr > 10, 1, 0))/length(degree_tab$out_degr)

# Placing the red lines at in-degree=10 and out-degree=10 we visualize the 1% of users
# by degree distribution
# degree_tab %>%
#   filter(totstacked > 100000) %>%
#   mutate(normalized_rewards = (rewards - min(rewards)) / (max(rewards) - min(rewards)),
#          col_totstacked = ifelse(totstacked > 1000000, '+1mln', '100k - 1mln')) %>%
#   ggplot(aes(x = in_degr, y = out_degr))+
#   geom_point(aes(size = normalized_rewards,
#                  color = col_totstacked))+
#   geom_hline(yintercept = 10, color = 'red')+
#   geom_vline(xintercept = 10, color = 'red')+
#   geom_label_repel(aes(
#     label = ifelse(totstacked>1000000, author, '')),
#     force = 1,
#     box.padding   = 5, 
#     point.padding = 0.5,
#     segment.color = 'grey50',
#     max.overlaps = 6000)+
#   labs(x = "In-degree", y = " Out-degree",
#        title = "Forum users in-degree and out-degree",
#        subtitle = "Rewards for users with more than 100k sats stacked")+
#   theme_classic() +
#   scale_y_log10() +
#   scale_x_log10()
# 
# ggsave('images/directed/general/rewards_nodes_degree.png')

## There are some anomalies in this graph. First of all the user 'utxoclub' seems to have +1mln of stacked sats all deriving from a single comment (item 84146). That could ben example of fat fingering and the user is legit because he/she has not been jailed.
## Indeed utxoclub is an outlier, just as 'tech5'. 'anarkio' seem to have achieved all his/her sats from a single comment (item 12115) that was probably well rewarded by the network. Overall 'anarkio' profile seems legit, with several comments and posts that stacked big amounts of sats.


# ## Rewards accumulation
# degree_tab %>%
#   filter(out_degr > 500 | in_degr > 500) %>%
#   mutate(normalized_rewards = (rewards - min(rewards)) / (max(rewards) - min(rewards))) %>%
#   ggplot(aes(x = in_degr, y = out_degr))+
#   geom_point(aes(size = totstacked,
#                  color = normalized_rewards))+
#   geom_label_repel(aes(
#     label = ifelse(out_degr > 500 | in_degr > 500 , author, '')),
#     force = 1,
#     box.padding   = 5,
#     point.padding = 0.5,
#     segment.color = 'grey50',
#     max.overlaps = 6000)+
#   theme_classic()


### Majority of users
# 
# degree_tab %>%
#   filter(totstacked < 1000000) %>%
#   #mutate(normalized = (totstacked - mean(totstacked)) / sd(totstacked) ) %>%
#   ggplot(aes(x = in_degr, y = out_degr))+
#   geom_point(aes(color = role), alpha = 0.3)+
#   geom_abline(slope=1, intercept = 0, color = 'red') +
#   #geom_smooth(method = 'lm')+
#   labs(x = "In-degree", y = " Out-degree",
#        title = "Forum users in-degree and out-degree",
#        subtitle = "Users with less than 1mln sats stacked")+
#   theme_classic() +
#   scale_y_log10() +
#   scale_x_log10()
  
### All users
# 
# degree_tab %>%
#   ggplot(aes(x = in_degr, y = out_degr))+
#   geom_point(aes(alpha = totstacked))+
#   geom_abline(slope = 1, intercept = 0, color = 'red') +
#   #geom_smooth(method = 'lm')+
#   geom_label_repel(aes(
#     label = ifelse(totstacked>500000, author, '')),
#     force = 1,
#     box.padding   = 5, 
#     point.padding = 0.5,
#     segment.color = 'grey50',
#     max.overlaps = 6000)+
#   labs(x = "In-degree", y = " Out-degree",
#        title = "Forum users in-degree and out-degree",
#        subtitle = "Labeled users stacked >500k")+
#   theme_classic() +
#   scale_y_log10() +
#   scale_x_log10()
# 
# ggsave('images/directed/general/blurred_reward_degree.png')


### Stacked distribution

ggplot(data = degree_tab, aes(x = totstacked)) +
  geom_histogram()  +
  xlim(0, 100000)

sum(ifelse(degree_tab$totstacked >= 10000, 1, 0))/length(degree_tab$totstacked)

sum(ifelse(degree_tab$totstacked >= 10000 & degree_tab$in_degr >=10 & degree_tab$out_degr >=10, 1, 0))/length(degree_tab$tot_degr)


sum(ifelse(degree_tab$totstacked >= 10000 & degree_tab$tot_degr >=10, 1, 0))/length(degree_tab$tot_degr)

degree_tab %>%
  filter(totstacked >= 10000) %>%
  mutate(normalized_rewards = (rewards - min(rewards)) / (max(rewards) - min(rewards)),
         col_totstacked = ifelse(totstacked > 100000, '+100k', '10k - 100k')) %>%
  ggplot(aes(x = in_degr, y = out_degr))+
  geom_point(aes(color = col_totstacked), size = 0.5)+
 # geom_hline(yintercept = 10, color = 'red')+
 # geom_vline(xintercept = 10, color = 'red')+
  geom_label_repel(aes(
    label = ifelse(totstacked>1000000, author, '')),
    force = 5,
    box.padding = 1, 
    point.padding = 5,
    segment.color = 'grey50',
    max.overlaps = 6000)+
  labs(x = "In-degree", y = " Out-degree",
       title = "Forum users in-degree and out-degree",
       subtitle = "Rewards for users with more than 10k sats stacked")+
  theme_classic() +
  scale_y_log10() +
  scale_x_log10()+
  theme(text = element_text(size = 14))+
  guides(color = guide_legend(title = "Total Stacked"))

ggsave('images/directed/general/stacked_degree_onepercent.png', width = 6, height = 5)



ggplot(data = degree_tab, aes(x = totstacked)) +
  stat_density(aes(y = after_stat(..density..)), geom = "point", color = "blue", size = 3, alpha = 1)+
  scale_y_sqrt() +
  # labs(x = "Total degree", y = "density",
  #      title = "Degree distribution",
  #      subtitle = "sqrt scaled values")+
  theme_classic()

##----------------------------------------------------------------------------
## Analysis of content and rewards

# The following section looks at two parameters:
# - Percentage of posts on the total interactions
# - Percentage of links in the total of posts
# These measures are useful for two purposes:
# 1. Based on how many posts the user created, is maybe useful to observe if posting
#    is more rewarded than commenting (even though this is somehow described by the in-degree)
# 2. Based on how many links the user posted, we can understand which type of post
#    is more rewarding
# This section assumes that every post not belonging to the "link" item type is 
# a post that stimulates more the interaction between users. This assumption is
# straightforward because links tend to lead users 'out of the platform', whereas
# discussion posts, bounties or poll are platform-centric items.

items_degr_tab = copy(degree_tab)
items_posts = copy(posts)
items_comments = copy(comments)

items_posts = items_posts %>%
  mutate(isLink = ifelse(Category=='link', 1, 0)) %>%
  group_by(Author) %>%
  summarise(link_perc = sum(isLink)/n(), num_post = n())

items_comments = items_comments %>%
  group_by(Author) %>%
  summarise(num_comments = n())
  
## Now we join these two tables with the one that carries the degree and rewards data.

items_degr_tab = merge(items_degr_tab, items_posts, by.x = 'author', by.y = 'Author')
items_degr_tab = merge(items_degr_tab, items_comments, by.x = 'author', by.y = 'Author')

## Now compute the total number of posts+comments and compute the percentage of posts on the total 
items_degr_tab = items_degr_tab %>%
  mutate(total_items = num_post + num_comments,
         post_perc = num_post/total_items) %>%
  select(-num_post, -num_comments)

# Now observe the correlation between post-rate and link-rate with the other
# parameters

# ------------- QUESTIONS---------------------------------
## Is the number of items correlated with the total degree
## YES, they are linearly correlated
items_degr_tab %>%
  ggplot(aes(x = total_items, y = tot_degr)) +
  geom_point() +
  geom_smooth(method = 'lm')

## Is the number of items correlated with the totstacked
# It seems so, but there is little group of outliers that earned a lot even with
# a miserably small amount of items posted
items_degr_tab %>%
  ggplot(aes(x = total_items, y = totstacked)) +
  geom_point() +
  geom_label_repel(aes(
    label = ifelse(total_items < 150 & totstacked > 500000, author, '')),
    force = 5,
    box.padding = 1, 
    point.padding = 0.5,
    segment.color = 'grey50',
    max.overlaps = 6000) 

## Is the perc of link correlated with the tot stacked
# NO, posting links doesn't seem to be correlated with the rewards
items_degr_tab %>%
  ggplot(aes(x = link_perc, y = totstacked)) +
  geom_point()

## Is the perc of post correlated with the total stacked
# NO, posting merely posts doesn't seem to be correlated with the rewards
items_degr_tab %>%
  ggplot(aes(x = post_perc, y = totstacked)) +
  geom_point()

## Is the perc of 'productive posts' meaningful with regards to interactions?
## 'Productive posts' are the posts that create discussions
# This outlines that if I post an high amount of productive posts I tend to obtain
# more interactions and thus more degree (more users reached)
## THIS POST OUTLINES AN INTERESTING INSIGHT:
## If I post a significant amount of 'productive posts', I tend to achieve more interactions
## than the ones that post links (at least if I'm 100% on productive posts only).
## The result of this is that, given an higher tot degree, I also tend to earn more
## even if I normalize the earnings for the inverse of the number of items.
## In fact this graph allocates an alpha to points based on the 
## totstacked*(1 - 'normalized number of posts'). This normalization means that 
## we are weighting more the users that generated more earnings from less posts,
## as opposed to who earned a lot and posted a lot.
## This provides a 'post-to-earning ratio' whereby if we post only productive posts
## we will inevitably tend to receive more earnings-per-post.
## 
## post-to-earning ratio = sqrt(totstacked * (1 - normalized total items))
## 
##  
items_degr_tab %>%
  mutate(normalized_totitems = (total_items - min(total_items)) / (max(total_items) - min(total_items)),
         post_to_earning_ratio =sqrt(totstacked*(1-normalized_totitems))) %>%
  ggplot(aes(x = 1-post_perc, y = tot_degr)) +
  geom_point(aes(alpha = post_to_earning_ratio), color = 'black') +
  scale_y_log10() +
  labs(x = "% productive posts (1 - links)", y = "Total degree",
       title = "Percentage of productive posts vs total degree",
       subtitle = "Darker observations have an higher ept ratio")+
  theme_classic()+
  theme(text = element_text(size = 14)) +
  guides(alpha= guide_legend(title = "etp ratio")) 

ggsave('images/directed/general/post_to_earning_graph.png', width = 6, height = 5)


items_degr_tab %>%
  mutate(normalized_totitems = (total_items - min(total_items)) / (max(total_items) - min(total_items)),
         post_to_earning_ratio =sqrt(totstacked*(1-normalized_totitems))) %>%
  ggplot(aes(x = 1-post_perc, y = tot_degr)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F)+
  # geom_point(aes(alpha = post_to_earning_ratio), color = 'black') +
  scale_y_log10() +
  # labs(x = "% productive posts (1 - links)", y = "Total degree",
       # title = "Percentage of productive posts vs total degree",
       # subtitle = "Darker observations have an higher earnings-per-post ratio")+
  # theme_classic()
  theme(
    legend.position='none'
    )




##----------------------------------------------------------------------------
## Components

component_distribution(g)

largest_component(g)

count_components(g)

components(g)$csize

decompose(g)[[9]]

######################################
## Visualize only the big component ##
######################################


## First quartile

coul  <- brewer.pal(4, "Set1") 
my_color <- coul[as.numeric(as.factor(V(g)$catSats))]

my_color[V(g)$catSats=="Q1"] <- adjustcolor(my_color[V(g)$catSats=="Q1"], alpha.f = 1)
my_color[V(g)$catSats=="Q2"] <- adjustcolor(my_color[V(g)$catSats=="Q2"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q3"] <- adjustcolor(my_color[V(g)$catSats=="Q3"], alpha.f = 0.1)
my_color[V(g)$catSats=="Q4"] <- adjustcolor(my_color[V(g)$catSats=="Q4"], alpha.f = 0.1)

png(filename = 'images/directed/general/general_Q1.png')
plot(decompose(g)[[1]],
     vertex.label = NA,
     vertex.color = my_color,
     layout = layout_with_lgl,
     vertex.size = 3,
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

png(filename = 'images/directed/general/general_Q2.png')
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

png(filename = 'images/directed/general/general_Q3.png')
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

png(filename = 'images/directed/general/general_Q4.png')
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

ggsave('images/directed/general/general_degree_of_separation.png')

################################################################################

## -----------------------------------------------------------------------------
## Clustering and partitioning

### Community detection algorithms aim to find the division of a network that maximizes its modularity
### Modularity ranges from -1 to 1:
### - Higher modularity score suggests a better division of the network into communities
### - Positive values indicate a good community structure
### - Negative values indicate that the network is not well divided into communities

## Transform graph into undirected to compute the several community detection algorithms

# undir_g <- as.undirected(g)

## Betweenness
# betweenness__ <- betweenness(g, directed = T)

# ggplot()+
  # geom_point(aes(x = betweenness__, y = degree(g,loops = F,mode = 'in'), color = 'red'))
  
# ggplot()+
  # geom_point(aes(x = betweenness__, y = V(g)$Rewards, color = 'red'))

# ggplot()+
  # geom_point(aes(x = betweenness__, y = V(g)$Sats, color = 'red'))


# ## Betweenness clustering
# 
# BetweennessCommunity <- cluster_edge_betweenness(g, weights = NULL, directed = TRUE, modularity = TRUE)
# 
# ## Leiden algorithm
# 
# LeidenCommunity <- cluster_leiden(undir_g)
# 
# length(LeidenCommunity)
# 
# 
# ## Walktrap algorithm
# 
# WalktrapCommunity <- cluster_walktrap(g)
# 
# length(WalktrapCommunity)












