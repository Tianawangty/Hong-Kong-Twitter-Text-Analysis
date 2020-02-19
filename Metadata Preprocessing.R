
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#                  1. Cleaning Dataset
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


###########################################################
#
#                  1.1. Import the data sets
#
###########################################################

df <- read.csv("Data/Jun_raw.csv")
# check the class of each column in df
sapply(df, class)
# convert id-related numeric variable as character variables to ensure nothing
# changs in id, which can be used as an unique index for further result
cols.num <- c("id","in_reply_to_status_id", "in_reply_to_user_id",
              "retweet_or_quote_id", "retweet_or_quote_user_id",
              "user_id")
df[cols.num] <- sapply(df[cols.num], as.character)
sapply(df, class)

# Until now, the dataset contains 27055 obversations with 37 variables

############################################################
#
#                  1.2. Clean up "Source" variable
#
############################################################

# using regular expression to clean up "source" by removing the hyperlinks
df$source <- substr(df$source, regexpr('>', df$source) + 1,
                    regexpr('</a>', df$source) - 1)

############################################################
#
#                  1.3. Only keep English tweets
#
############################################################

# becuase the protest attracted international attentions, tweets are writtern
# in various language. To keep to this presentation simple, I will keep tweets 
# that writtern in English. Here, I used "tidyverse" package to filter "lang=en"

# install.package("tidyverse")
library(tidyverse)
df_en <- filter(df, df$lang == "en")

# Until now, the dataset contains 15898 obversations with 37 variables

############################################################
#
#                  1.4. Cleaning Date and Days
#
############################################################

# convert datetime variables from factor to as.POSIXct class
df_en$parsed_created_at <- as.POSIXct(df_en$parsed_created_at, tz = "UTC", 
                                      "%Y-%m-%d %H:%M:%OS")
df_en$created_at  <- as.POSIXct(df_en$created_at, tz = "UTC", 
                                "%a %b %e %H:%M:%S %z %Y")
df_en$user_created_at  <- as.POSIXct(df_en$user_created_at, tz = "UTC", 
                                     "%a %b %e %H:%M:%S %z %Y")

# Becuase UTC is the orginal timezoen for Twitter API when scraping,
# for the research purpose, convert UTC to HK local time
df_en$HK_localtime <- format(df_en$created_at, tz="Hongkong", 
                             "%Y-%m-%d %H:%M:%OS")

# I am here also interested in consider accumulated days for the protest, so I
# set the start date as March 29, 2019 for calculation
duration <- data.frame(df_en$HK_localtime, starttime="2019-03-29")
colnames(duration)
colnames(duration) <- c("HK_localtime", "starttime")

duration$HK_localtime <- as.POSIXct(duration$HK_localtime, 
                                    tz = "Hongkong",
                                    "%Y-%m-%d")
duration$starttime <- as.POSIXct(duration$starttime, 
                                 tz = "Hongkong", 
                                 "%Y-%m-%d")
class(duration$starttime)
class(duration$HK_localtime)
duration$date_diff <- difftime(duration$HK_localtime,
                               duration$starttime, 
                               tz = "Hongkong",
                               units = "days")
df_en$daycount <- duration$date_diff

# Until now, the dataset contains 15898 obversations with 39 variables

############################################################
#
#                  1.5. Split the dataset to ensure the time frame
#
############################################################

# 6/1/19 ~ 6/30/19
split_date_1 <-  as.character("2019/06/01 00:00:00")
split_date_1 <-  as.POSIXct(split_date_1,tz = "Hongkong")
restset <- filter(df_en, df_en$HK_localtime > split_date_1)

split_date_2 <-  as.character("2019/06/30 23:59:59")
split_date_2 <-  as.POSIXct(split_date_2,tz = "Hongkong")
anti_jun <- filter(restset, restset$HK_localtime <= split_date_2)
restset <- filter(restset, restset$HK_localtime > split_date_2)

# Until now, I create two new datasets:
#       1) anti_jun: contains 12894 obversations with 39 variables
#       2) restset: contains 3004 obversations with 39 variables
# We will use anti_jun for the next step