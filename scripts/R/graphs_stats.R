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

