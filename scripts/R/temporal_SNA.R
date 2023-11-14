library(networkDynamic)
library(sna)
library(tsna)
library(ndtv)
library(data.table)
library(dplyr)

# Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')


clean_posts <- subset(posts, select = c(ItemCode, Author, Timestamp, CommentsItemCode))
clean_comments <- subset(comments, select = c(ItemCode, Author, Timestamp, CommentsItemCode))

total_items <- rbindlist(list(clean_posts, clean_comments),
                         use.names = T,
                         fill = TRUE)

total_graph <- copy(total_items)

total_graph <- total_graph[, .(Commentors = unlist(tstrsplit(CommentsItemCode, ", "))), by = "ItemCode"]

total_graph <- merge(total_graph, total_items, by.x = 'ItemCode', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = ItemCode, Item2 = Commentors, Author1 = Author)]

total_graph <- merge(total_graph, total_items, by.x = 'Item2', by.y = 'ItemCode')

total_graph <- total_graph[, .(Item1 = Item1, Item2 = Item2, Author1 = Author1, Author2 = Author, Interaction = Timestamp)]

# Final graph is an undirected edge list
final_graph <- total_graph[, .(Author2, Author1, Interaction)]

final_graph <- final_graph[pmin(Author2, Author1) != pmax(Author2, Author1)]

final_graph <- final_graph[, ':=' ( FirstInteraction = min(Interaction), LastInteraction = max(Interaction) ),
                           by = .(Author2 = pmax(Author2, Author1), Author1 = pmin(Author2, Author1))]

final_graph <- subset(final_graph, select = c(Author2, Author1, FirstInteraction, LastInteraction))

final_graph <- final_graph[!duplicated(final_graph)]

# Remove duplicated couples of users.
# For every user combination now there is only one edge
# The edge now indicates the timestamp of the first interaction and the last one
# between the two users.
mn <- pmin(final_graph$Author2, final_graph$Author1)
mx <- pmax(final_graph$Author2, final_graph$Author1)
int <- as.numeric(interaction(mn, mx))
final_graph <- final_graph[match(unique(int), int),]






