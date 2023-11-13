library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
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

