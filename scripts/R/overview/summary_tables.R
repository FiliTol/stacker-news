library(data.table)
library(igraph)
library(ggplot2)
#library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)

#' # Load RDS files
comments <- readRDS(file = 'RDS_files/comments')
posts <- readRDS(file = 'RDS_files/posts')
users <- readRDS(file = 'RDS_files/users')

#' # PART 2. Summary Table
#' ### Divide in different periods
## ---------------------------------------------------------------------------------------------------------------------------------
### Divide in different periods
p_first_period = posts[Timestamp %between% c("2021-06-01","2021-12-31")]
p_second_period = posts[Timestamp %between% c("2022-01-01","2022-05-31")]
p_third_period = posts[Timestamp %between% c("2022-06-01","2022-12-31")]
p_fourth_period = posts[Timestamp %between% c("2023-01-01","2023-05-31")]
p_fifth_period = posts[Timestamp %between% c("2023-06-01","2023-12-31")]

saveRDS(object = p_first_period, file = 'RDS_files/p_first_period')
saveRDS(object = p_second_period, file = 'RDS_files/p_second_period')
saveRDS(object = p_third_period, file = 'RDS_files/p_third_period')
saveRDS(object = p_fourth_period, file = 'RDS_files/p_fourth_period')
saveRDS(object = p_fifth_period, file = 'RDS_files/p_fifth_period')

c_first_period = comments[Timestamp %between% c("2021-06-01","2021-12-31")]
c_second_period = comments[Timestamp %between% c("2022-01-01","2022-05-31")]
c_third_period = comments[Timestamp %between% c("2022-06-01","2022-12-31")]
c_fourth_period = comments[Timestamp %between% c("2023-01-01","2023-05-31")]
c_fifth_period = comments[Timestamp %between% c("2023-06-01","2023-12-31")]

saveRDS(object = c_first_period, file = 'RDS_files/c_first_period')
saveRDS(object = c_second_period, file = 'RDS_files/c_second_period')
saveRDS(object = c_third_period, file = 'RDS_files/c_third_period')
saveRDS(object = c_fourth_period, file = 'RDS_files/c_fourth_period')
saveRDS(object = c_fifth_period, file = 'RDS_files/c_fifth_period')

#' 
#' ## Create the variables
#' ### Periods
## ---------------------------------------------------------------------------------------------------------------------------------
Period = c("First", "Second", "Third", "Fourth", "Fifth")

#' 
#' ### Unique Poster 
## ---------------------------------------------------------------------------------------------------------------------------------
Posters = c(
  uniqueN(p_first_period$Author),
  uniqueN(p_second_period$Author),
  uniqueN(p_third_period$Author),
  uniqueN(p_fourth_period$Author),
  uniqueN(p_fifth_period$Author))

#' 
#' ### Total Sats from posts and from comments
## ---------------------------------------------------------------------------------------------------------------------------------
Post_Sats = c(
  sum(p_first_period$Sats,na.rm = TRUE),
  sum(p_second_period$Sats,na.rm = TRUE),
  sum(p_third_period$Sats,na.rm = TRUE),
  sum(p_fourth_period$Sats,na.rm = TRUE),
  sum(p_fifth_period$Sats,na.rm = TRUE))

Comment_Sats = c(
  sum(c_first_period$Sats,na.rm = TRUE),
  sum(c_second_period$Sats,na.rm = TRUE),
  sum(c_third_period$Sats,na.rm = TRUE),
  sum(c_fourth_period$Sats,na.rm = TRUE),
  sum(c_fifth_period$Sats,na.rm = TRUE))

#' 
#' ### Only commentors
## ---------------------------------------------------------------------------------------------------------------------------------
first_commentors = anti_join(p_first_period, c_first_period, by = "Author")
second_commentors = anti_join(p_second_period, c_second_period, by = "Author")
third_commentors = anti_join(p_third_period, c_third_period, by = "Author")
fourth_commentors = anti_join(p_fourth_period, c_fourth_period, by = "Author")
fifth_commentors = anti_join(p_fifth_period, c_fifth_period, by = "Author")
Only_commentors=c(nrow(first_commentors),nrow(second_commentors),nrow(third_commentors),nrow(fourth_commentors),nrow(fifth_commentors))

#' 
#' ### N posts and N comments
## ---------------------------------------------------------------------------------------------------------------------------------
N_posts=c(nrow(p_first_period),
          nrow(p_second_period),
          nrow(p_third_period),
          nrow(p_fourth_period),
          nrow(p_fifth_period))

N_comments=c(nrow(c_first_period),
             nrow(c_second_period),
             nrow(c_third_period),
             nrow(c_fourth_period),
             nrow(c_fifth_period))

#' 
#' ### Type of post
## ---------------------------------------------------------------------------------------------------------------------------------
# link
N_link=c(nrow(p_first_period[Category == "link"]),
         nrow(p_second_period[Category == "link"]),
         nrow(p_third_period[Category == "link"]),
         nrow(p_fourth_period[Category == "link"]),
         nrow(p_fifth_period[Category == "link"]))

# discussion
N_discussion=c(nrow(p_first_period[Category == "discussion"]),
               nrow(p_second_period[Category == "discussion"]),
               nrow(p_third_period[Category == "discussion"]),
               nrow(p_fourth_period[Category == "discussion"]),
               nrow(p_fifth_period[Category == "discussion"]))

# bounty
N_bounty=c(nrow(p_first_period[Category == "bounty"]),
           nrow(p_second_period[Category == "bounty"]),
           nrow(p_third_period[Category == "bounty"]),
           nrow(p_fourth_period[Category == "bounty"]),
           nrow(p_fifth_period[Category == "bounty"]))
# poll

N_poll=c(nrow(p_first_period[Category == "poll"]),
         nrow(p_second_period[Category == "poll"]),
         nrow(p_third_period[Category == "poll"]),
         nrow(p_fourth_period[Category == "poll"]),
         nrow(p_fifth_period[Category == "poll"]))


#' 
#' 
#' ## Creating the Summary Table
## ---------------------------------------------------------------------------------------------------------------------------------
summary_table=data.frame(Period,N_posts,N_comments,Posters,Post_Sats,Comment_Sats,Only_commentors,N_link,N_discussion,N_bounty,N_poll)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
summary_table

#' # LINK SUMMARY
#' ### Check the links
## ---------------------------------------------------------------------------------------------------------------------------------
twitter=c(nrow(p_first_period[grepl("twitter", MainLink, ignore.case = TRUE)]),
          nrow(p_second_period[grepl("twitter", MainLink, ignore.case = TRUE)]),
          nrow(p_third_period[grepl("twitter", MainLink, ignore.case = TRUE)]),
          nrow(p_fourth_period[grepl("twitter", MainLink, ignore.case = TRUE)]),
          nrow(p_fifth_period[grepl("twitter", MainLink, ignore.case = TRUE)]))

youtube=c(nrow(p_first_period[grepl("youtube", MainLink, ignore.case = TRUE)]),
          nrow(p_second_period[grepl("youtube", MainLink, ignore.case = TRUE)]),
          nrow(p_third_period[grepl("youtube", MainLink, ignore.case = TRUE)]),
          nrow(p_fourth_period[grepl("youtube", MainLink, ignore.case = TRUE)]),
          nrow(p_fifth_period[grepl("youtube", MainLink, ignore.case = TRUE)]))

linkedin=c(nrow(p_first_period[grepl("linkedin", MainLink, ignore.case = TRUE)]),
           nrow(p_second_period[grepl("linkedin", MainLink, ignore.case = TRUE)]),
           nrow(p_third_period[grepl("linkedin", MainLink, ignore.case = TRUE)]),
           nrow(p_fourth_period[grepl("linkedin", MainLink, ignore.case = TRUE)]),
           nrow(p_fifth_period[grepl("linkedin", MainLink, ignore.case = TRUE)]))

bitcoinmagazine=c(nrow(p_first_period[grepl("bitcoinmagazine", MainLink, ignore.case = TRUE)]),
                  nrow(p_second_period[grepl("bitcoinmagazine", MainLink, ignore.case = TRUE)]),
                  nrow(p_third_period[grepl("bitcoinmagazine", MainLink, ignore.case = TRUE)]),
                  nrow(p_fourth_period[grepl("bitcoinmagazine", MainLink, ignore.case = TRUE)]),
                  nrow(p_fifth_period[grepl("bitcoinmagazine", MainLink, ignore.case = TRUE)]))

github=c(nrow(p_first_period[grepl("github", MainLink, ignore.case = TRUE)]),
         nrow(p_second_period[grepl("github", MainLink, ignore.case = TRUE)]),
         nrow(p_third_period[grepl("github", MainLink, ignore.case = TRUE)]),
         nrow(p_fourth_period[grepl("github", MainLink, ignore.case = TRUE)]),
         nrow(p_fifth_period[grepl("github", MainLink, ignore.case = TRUE)]))

yahoo=c(nrow(p_first_period[grepl("yahoo", MainLink, ignore.case = TRUE)]),
        nrow(p_second_period[grepl("yahoo", MainLink, ignore.case = TRUE)]),
        nrow(p_third_period[grepl("yahoo", MainLink, ignore.case = TRUE)]),
        nrow(p_fourth_period[grepl("yahoo", MainLink, ignore.case = TRUE)]),
        nrow(p_fifth_period[grepl("yahoo", MainLink, ignore.case = TRUE)]))

#' 
#' ## Creating the Link Summary Table
## ---------------------------------------------------------------------------------------------------------------------------------
link_table=data.frame(Period,N_link, twitter,youtube,linkedin, bitcoinmagazine,github,yahoo)

#' 
## ---------------------------------------------------------------------------------------------------------------------------------
link_table







remove(list=ls())
