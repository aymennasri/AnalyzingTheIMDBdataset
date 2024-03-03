library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(anytime)
library(stringr)
# Reading the data
data <- read_csv("imdb_data.csv")
# Exploring different columns of the dataset
# Checking the data
glimpse(data)
# Noticed that there's two columns for the title, checking whether it's duplicated or not
summary(data$primaryTitle == data$originalTitle)
# We'll have to keep these two as there's 145 titles that are different
summary(data$isAdult == 0)
# The column isAdult is filled with zeros therefore it won't be used
clean_data <- data %>% select(-isAdult)
# Checking the release dates
head(clean_data$release_date, n = 20)
summary(is.na(clean_data$release_date))
sample(clean_data$release_date, 50)
# Sampling the data revealed that some dates are just the year of release (I discovered this while trying to compensate the 5 missing dates)
summary(is.na(data$release_date))
summary(anydate(clean_data$release_date))
new_data <- clean_data %>% 
  mutate() %>%
  

# 5 movies are without a release date which could be compensated easily
dates <- c("February 12, 1993", "September 14, 1994", "October 1, 2004", "October 7, 2005", "April 21, 2016")
new_data <- clean_data %>%
  arrange(release_date)%>%
  mutate(release_date = ifelse(is.na(release_date), dates, release_date))
new_data %>% filter(release_date == dates)
anydate(clean_data$release_date)
summary(is.na(clean_data$runtimeMinutes))
clean_data %>% ggplot(aes(runtimeMinutes))+
  geom_bar()+
  scale_x_binned()
# Most of the movies are between 80 and 180 mins
clean_data %>% ggplot(aes(averageRating, runtimeMinutes))+
  geom_smooth(se = F)+
  geom_jitter()
cor(clean_data$averageRating, clean_data$runtimeMinutes)
# Despite the plot stating that a longer movie is more likely to be rated higher, the correlation value states otherwise with 0.36 being a low value.
clean_data %>% ggplot(aes(anydate(as.Date(release_date))))+
  geom_bar()
