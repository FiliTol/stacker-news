library(remotes)
#remotes::install_github("edgararuiz/connections")
library(connections)
library(RSQLite)
library(data.table)
library(igraph)
library(ggplot2)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(dplyr)



#' 
#' ## Data importing, cleaning and manipulation
#' 
## ----Query to database-------------------------------------------------------------------------------------------------------------------
conn <- connection_open(SQLite(), "data/stacker_news.sqlite")

users <- setDT(dbGetQuery(conn, "SELECT * FROM user"))
comments <- setDT(dbGetQuery(conn, "SELECT * FROM comments"))
posts <- setDT(dbGetQuery(conn, "SELECT * FROM post"))

connection_close(conn)

#' 
#' Please note that while scraping data were all saved in 'string` form to better
#' manage datatypes in the database
#' 
#' ### Users
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
str(users)

#' 
#' Users table contains five variables.
#' 
#' - `User` is a string, the username;
#' - `TotalStacked` is the total amount of sat ever stacked by the user -> need it as integer;
#' - `FirstItem` is the number of the first post that the user created, if any;
#' - `HatStreak` is the number of cowboy hat streaks that the user achieved, if any -> need it as integer;
#' - `NumItems` is the total number of items created by the user in the forum, if any -> need it as integer.
#' 
#' The following chunck changes the needed variables into the needed datatype
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
change_columns <- c("TotalStacked", "HatStreak", "NumItems")  
users[ , (change_columns) := lapply(.SD, as.numeric), .SDcols = change_columns]

#' 
#' Result of the transformation
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
sapply(users, class)

#' 
#' #### Adding user label
#' 
#' In order to analyse the user base, here we procede to add a new column to the table called `Role`. The variable has the value `contributor` if the user is a forum administrator, `other` otherwise.
#' The roles of users are extracted by looking at [this file](https://github.com/stackernews/stacker.news/blob/master/contributors.txt) in the github page of the project.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
contrib <- c("k00b",
             "kr",
             "ekzyis",
             "WeAreAllSatoshi",
             "rleed",
             "bitcoinplebdev",
             "benthecarman"
)

users[, Role := ifelse(User %in% contrib, "contributor", "other")]

#' 
#' 
#' ### Posts
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
str(posts)

#' 
#' Post table contains 13 variables.
#' 
#' - `Title` is the post title;
#' - `Category` is the post category;
#' - `ItemCode` is the unique identifier for the post;
#' - `Sats` is the number of sat stacked by the post -> need it as integer;
#' - `Boost` is the number of boosts assigned to the post. Boosts are needed to speed up the post diffusion in the forum (kind of advertising) -> need it as integer;
#' - `Comments` is the number of comments -> need it as integer;
#' - `Author` is the username of the post author;
#' - `Tag` is the tag assigned to the post by the author;
#' - `Timestamp` is the timestamp of the creation of the post -> need it in datetime;
#' - `MainLink` is the main link displayed by the post, a post has a main link if it has the *link* category, if not then the value is NA;
#' - `BodyLinks` is the field containing a string in which all the links in the body post and comments are contained. If the post contains no link, then the string is empty;
#' - `SatsReceivedComments` is the total number of sat received by the comments to the post -> need it as integer;
#' - `CommentsItemCode` is the list of item codes of the comments.
#' 
#' Multiple variables of this table must be transformed into the proper data type, however some of those cannot be transformed straightforwardly because of multiple formatting errors. A more profund data cleaning is needed before the datatype transformation.
#' 
#' #### Sats variable
#' 
#' This variable has to be transformed into numeric type, however beforehand we must cut away the postfix 'sats' or 'sat' from the stings.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
# Substitute rounding of thousands 'k' and millions 'm' into integers
posts[ , Sats := (str_replace_all(Sats, "k sats$", "000") %>%
                    str_replace_all(., "m sats$", "000000") %>%
                    str_replace_all(., " sats$", "") %>%
                    str_replace_all(., " sat$", "")
)
]

posts[, Sats := ifelse(str_detect(Sats, "\\."), str_sub(str_replace_all(Sats, "\\.", ""), end = -2), Sats)]

# Turn variable in numeric
posts[ , Sats := as.numeric(Sats)] 

#' 
#' Please note that we are left some NAs. These items with Sats=NA are items that don't have the Sats field in the post template because they are created by the forum administrator and they cannot be tipped.
#' Also 37 of them are created by the user 'ad', who is a bot defined by the forum creator to broadcast ads as posts. Those items cannot be tipped too.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
# Test if we managed None values correctly
nrow(posts[is.na(Sats)]) == nrow(posts[is.na(Sats) & Author=='saloon']) + nrow(posts[is.na(Sats) & Author=='ad'])

#' 
#' Now the `Sats` column contains only numeric values (the amount of sat stacked by the post) and the NAs are all related to posts that actually didn't receive Sats because of structural reasons.
#' 
#' The following graph shows the distribution of sats within the posts. The distribution is clearly asymmetric and has the majority of the posts with an amount of sats stacked very close to zero.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts %>%
  ggplot()+
  geom_density(aes(x = Sats))

#' 
#' By filtering the amount of sats to a reasonable value is possible to observe the distribution, with the median value of sats stacked by a post equal to `r na.omit(posts, "Sats")[, median(Sats)]`
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[Sats<1000] %>%
  ggplot()+
  geom_histogram(aes(x = Sats))

#' 
#' 
#' #### Boost variable
#' 
#' In order to deal with the thousands (k) and the decimals, we use the same technique already applied for the Sats variable.
#' Then the Boost variable is turned into numeric and some NAs are introduced. The majority of the posts, more precisely `r nrow(posts[is.na(Boost)])` posts, do not have boost (because they are not sponsored posts). Therefore the majority of the Boost values will be NAs.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
# Substitute rounding of thousands 'k'
posts[ , Boost := (str_replace_all(Boost, "k boost$", "000") %>%
                     str_replace_all(., " boost$", "")
)
]

posts[, Boost := ifelse(str_detect(Boost, "\\."), str_sub(str_replace_all(Boost, "\\.", ""), end = -2), Boost)]

posts[, Boost := as.numeric(Boost)]

#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts %>%
  ggplot()+
  geom_histogram(aes(x=Boost), bins=30)

#' 
#' #### Comments variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[, Comments := (str_replace_all(Comments, " comments", "") %>%
                       str_replace_all(., " comment", "")
)
]

posts[, Comments := as.numeric(Comments)]

#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts %>%
  ggplot()+
  geom_histogram(aes(x=Comments))

#' 
#' #### Timestamp variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[, Timestamp := ymd_hms(Timestamp)]

#' 
#' Let's visualize the transformation
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
sapply(posts, class)$Timestamp

#' 
#' #### MainLink and BodyLinks variable
#' 
#' This variable shows the main link for 'link' posts.
#' This variable has been massively affected by an error during the scraping. The result of that process are widely diffused miss-classification of links that are consistent throughout the dataset, so we can deal with those straightforwardly.
#' 
#' Namely:
#' 
#' - If the value of MainLink is a string with a link, then the scraping had the desired outcome;
#' - If the value of MainLink is 'None', then the item has a Link but it was wrongfully stored as the first element of the BodyLinks variable;
#' - If the value of MainLink is NA, then the item does not have a main link (because it does not belong to the category 'link')
#' 
#' The result of this is that:
#' 
#' - for MainLink='None' we need to move the first link in BodyLinks to MainLink;
#' - All the other cases are correct.
#' 
#' Since BodyLinks values are in the string form, we need firstly to deal with BodyLinks datatype, turning it into a vector. Then we can execute the needed procedure.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[, BodyLinks := (str_replace_all(BodyLinks, '^\\[', "") %>%
                        str_replace_all(., '\\]$', "")
)
]

posts[, BodyLinks := list(strsplit(BodyLinks, split = ", "))]

#' 
#' The following code then extract the first link from the BodyLinks column and put it in the MainLink column, for posts that are links.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[, MainLink := ifelse(MainLink=='None', sapply(BodyLinks, `[`, 1), MainLink)]

#' 
#' 
#' #### SatsReceivedComments variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[ , SatsReceivedComments := (str_replace_all(SatsReceivedComments, "k sats$", "000") %>%
                                    str_replace_all(., "m sats$", "000000") %>%
                                    str_replace_all(., " sats$", "") %>%
                                    str_replace_all(., " sat$", "")
)
]

posts[, SatsReceivedComments := ifelse(str_detect(SatsReceivedComments, "\\."), str_sub(str_replace_all(SatsReceivedComments, "\\.", ""), end = -2), SatsReceivedComments)]

posts[, SatsReceivedComments := as.numeric(SatsReceivedComments)]

#' 
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[SatsReceivedComments<1000] %>%
  ggplot()+
  geom_histogram(aes(x = SatsReceivedComments), bins = 30)

#' 
#' #### CommentsItemCode variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
posts[, CommentsItemCode := (str_replace_all(CommentsItemCode, '^\\[', "") %>%
                               str_replace_all(., '\\]$', "")
)
]

#' 
#' 
#' ### Comments
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
str(comments)

#' 
#' Comments table contains 8 variables:
#' 
#' - `ItemCode` is a sting, the unique code assigned to the comment;
#' - `Sats` is the amount of sats stacked by the comment -> need it as integer;
#' - `Boost` is the number of boosts received by the comment -> need it as integer;
#' - `Comments` is the number of replies that the comment had -> need it as integer;
#' - `Author` is the name of the author;
#' - `Tag` is the tag assigned to the post by the author;
#' - `Timestamp` is the timestamp of the creation of the post -> need it in datetime;
#' - `CommentsItemCode` is the list of item codes of the comments.
#' 
#' #### Sats variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
comments[ , Sats := (str_replace_all(Sats, "k sats$", "000") %>%
                       str_replace_all(., "m sats$", "000000") %>%
                       str_replace_all(., " sats$", "") %>%
                       str_replace_all(., " sat$", "")
)
]

comments[, Sats := ifelse(str_detect(Sats, "\\."), str_sub(str_replace_all(Sats, "\\.", ""), end = -2), Sats)]

comments[ , Sats := as.numeric(Sats)] 

#' 
#' The median of the Sats distribution is `r na.omit(comments, "Sats")[, median(Sats)]`
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
comments[Sats<1000] %>%
  ggplot()+
  geom_histogram(aes(x = Sats))

#' 
#' #### Boost variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
comments[ , Boost := (str_replace_all(Boost, "k boost$", "000") %>%
                        str_replace_all(., " boost$", "")
)
]

comments[, Boost := ifelse(str_detect(Boost, "\\."), str_sub(str_replace_all(Boost, "\\.", ""), end = -2), Boost)]

comments[, Boost := as.numeric(Boost)]

#' 
#' Note that the previous procedure introduced `r sum(is.na(comments[,Boost]))` NA values for the Boost variable. In fact the majority of the comments didn't receive boosts, which is quite reasonable since boosts are used to increase visibility of posts just like sponsored features in other social media, and usually comments are not so worth to be promoted because they are strictly related to the post they refer to.
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
comments %>%
  ggplot()+
  geom_histogram(aes(x = Boost))

#' 
#' #### Timestamp variable
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
comments[, Timestamp := ymd_hms(Timestamp)]

#' 
## ----------------------------------------------------------------------------------------------------------------------------------------
sapply(comments, class)$Timestamp

## ----------------------------------------------------------------------------------------------------------------------------------------
saveRDS(object = posts, file = 'RDS_files/posts')
saveRDS(object = comments, file = 'RDS_files/comments')
saveRDS(object = users, file = 'RDS_files/users')











