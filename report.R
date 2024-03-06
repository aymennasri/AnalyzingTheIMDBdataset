library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(anytime)
library(stringr)
# Reading the data
data <- read_csv("imdb_data.csv")
# Checking the data
glimpse(data)
# Noticed that there's two columns for the title, checking whether it's duplicated or not
summary(data$primaryTitle == data$originalTitle)
# Checking for duplicated observations
summary(duplicated(data))
# We'll have to keep these two as there's 145 titles that are different
summary(data$isAdult == 0)
# The column isAdult is filled with zeros therefore it won't be used
clean_data <- data %>% select(-isAdult)
# Checking crucial columns for NAs
summary(is.na(data$runtimeMinutes))
summary(is.na(data$directors))
summary(is.na(data$genres))
summary(is.na(clean_data$release_date))
# 5 movies are without a release date which could be compensated easily
# Extracting dates of the films using Google (ordered the same order as the print result for the names)
movie_names <- unlist(as.vector(clean_data %>% 
                                  select(primaryTitle) %>%
                                  filter(is.na(clean_data$release_date))))
movie_names
dates <- c("February 12, 1993", "September 14, 1994", "October 1, 2004",
           "October 7, 2005", "April 21, 2016")
full_data <- clean_data
for(i in 1:length(movie_names)){
  full_data$release_date[full_data$primaryTitle == movie_names[i]] <- dates[i]
}
# Verifying the results
full_data %>% filter(primaryTitle %in% movie_names)
summary(is.na(full_data$release_date))
# Sampling the data revealed that the dates don't share the same format
# (I discovered this while trying to compensate the 5 missing dates)
sample(full_data$release_date, 50, replace = T)
summary(is.na(anydate(full_data$release_date)))
summary(is.na(as.Date(full_data$release_date, format = c("%Y", "%B %Y", "%B %d, %Y"))))
# All the other functions won't work, so I had to search for a better solution
# which I found thanks to this
# [page](https://stackoverflow.com/questions/13764514/how-to-change-multiple-date-formats-in-same-column)

# parse_date_time tries to parse the dates using the patterns in orders; the orders
# vector is composed of the possible formats that could be found in the release_date column
# but the downside is that movies having only a year as their date will be parsed into the
# date of the first day of that year (same thing with month, year format)
new_data <- full_data %>%
  mutate(release_date = parse_date_time(release_date,
                                        orders = c('mdy', 'dmy','my','y')))
# Checking the results of the parse_date_date
sample(new_data$release_date, 50)
# Adding a net_profit column(despite having 51 movies without a gross value)
summary(is.na(new_data$budget))
summary(is.na(new_data$gross))
new_data <- new_data %>% mutate(net_profit = gross - budget)
summary(new_data$net_profit)
# After exploring the summary, the values seem to be too large to only display in USD
# so we'll be adding new columns containing the values in millions of USD
new_data_v1 <- new_data %>%
  mutate(budget_M = budget / 1000000,
         gross_M = gross / 1000000,
         net_profit_M = net_profit / 1000000)%>%
  select(-c(budget, gross, net_profit))
# It now looks better and more readable
summary(new_data_v1$net_profit_M)
# Extracting the genres and counting their occurences and their performances
genres_list <- new_data_v1 %>%
  #seperate_longer_delim is used to dedicate new rows for each genre in the column
  separate_longer_delim(genres, delim = ",")%>%
  select(genres, averageRating, net_profit_M)

genres <- genres_list %>% 
  group_by(genres)%>%
  summarise(count = n(),
            avg_rating = mean(averageRating, na.rm = T),
            avg_profit = mean(net_profit_M, na.rm = T),
            total_profit = sum(net_profit_M, na.rm = T))%>%
  arrange(desc(count))

genres
# In the same way, extracting the directors and counting their occurences and their performances
directors_list <- new_data_v1 %>%
  separate_longer_delim(directors, delim = ',')%>%
  select(directors, averageRating, net_profit_M)

directors <- directors_list %>%
  group_by(directors)%>%
  summarise(count = n(),
            avg_rating = mean(averageRating, na.rm = T),
            avg_profit = mean(net_profit_M, na.rm = T),
            total_profit = sum(net_profit_M, na.rm = T))%>%
  arrange(desc(count))
directors
# Does a bigger budget mean better profits?
cor(new_data_v1$budget_M, new_data_v1$net_profit_M, use = "complete.obs")
# Does a bigger budget mean better ratings?
cor(new_data_v1$averageRating, new_data_v1$budget_M, use = "complete.obs")
# Do longer movies perform better in terms of profits?
cor(new_data_v1$runtimeMinutes, new_data_v1$net_profit_M, use = "complete.obs")
# Do longer movies have higher ratings?
cor(new_data_v1$runtimeMinutes, new_data_v1$averageRating, use = "complete.obs")


new_data %>% ggplot(aes(runtimeMinutes))+
  geom_bar()+
  scale_x_binned()
  
# Most of the movies are between 80 and 180 mins
