# LOAD DATA -------------------------------

# housekeeping
rm(list=ls())


# load libraries
library(tidyverse)
library(mice)
library(corrplot)
library(car)
library(gridExtra)
library(MASS)
options(scipen=10000)

# read data
sales <- read.csv("../data/00-video-game-sales.csv", na.strings = c("", "N/A", "tbd", NA),
                  colClasses = c("character", "factor", "integer", "factor", 
                                 "character", "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "integer", "integer",
                                 "numeric", "integer", "character", "factor"))



# change variable names to lower case
names(sales) <- tolower(names(sales))

# data summary
head(sales)
str(sales)
summary(sales)

# EDA -----------------------------------

# discard all sales variables except global sales
sales$na_sales <- NULL
sales$eu_sales <- NULL
sales$jp_sales <- NULL
sales$other_sales <- NULL

# also discard publisher and developer - both of these have around 1000 categories
sales$developer <- NULL
sales$publisher <- NULL




# missing data analyses
md.pattern(sales, rotate.names = TRUE)

# percentage missing values in each column
sapply(sales, function(x) sum(is.na(x))*100/length(x))

# 60% rows have missing data, therefore, drop rows with missing value(s)
sales <- na.omit(sales)



# change scale of global_sales from number of units in million to number of units sold
sales$global_sales <- sales$global_sales * 1e6



# change buckets of rating
table(sales$rating)
sales <- sales[!sales$rating %in% c("AO", "K-A", "RP"), ]
sales$rating <- as.character(sales$rating)
sales$rating[sales$rating == "E"] <- "Everyone"
sales$rating[sales$rating == "E10+"] <- "10+"
sales$rating[sales$rating == "M"] <- "17+"
sales$rating[sales$rating == "T"] <- "13+"
sales$rating <- factor(sales$rating)

# make scale of user_score equal to critic_score which is 0-100
sales$user_score <- sales$user_score*10







# data summary
md.pattern(sales, rotate.names = TRUE)
head(sales)
str(sales)
summary(sales)

# correlation matrix
numeric <- sapply(sales, is.numeric)
cordata <- sales[ , numeric]
cor_matrix <- cor(cordata)
cor_matrix
corrplot(cor_matrix, method = "square", type = "lower") # to visualize correlation matrix






# global sales
quantile(sales$global_sales, probs = seq(0, 1, 0.01))

sales %>%
  ggplot(aes(x=global_sales/1e6)) +
  geom_histogram(fill="orange") +
  labs(title="Distribution of Global Sales", x="Global Sales (in million units)", y="Number of Video Games") +
  theme_minimal()

## global sales on log scale - looks better
sales %>%
  ggplot(aes(x=log(global_sales))) +
  geom_histogram(fill="orange") +
  labs(title="Distribution of Log of Global Sales")

## global sales on square root scale
sales %>%
  ggplot(aes(x=sqrt(global_sales))) +
  geom_histogram(fill="orange")

## global sales on cube root scale
sales %>%
  ggplot(aes(x=global_sales^(1/3))) +
  geom_histogram(fill="orange")

## global sales on tenth root scale
sales %>%
  ggplot(aes(x=global_sales^(1/10))) +
  geom_histogram(fill="orange")






# platform and sales -- too many platforms
sales %>%
  ggplot(aes(x=platform, y=log(global_sales))) +
  geom_boxplot() +
  labs(title = "Platforms and Sales", x="Platform", y="Log Global Sales")

table(sales$platform)

# bucket all playstations in one category, all xbox in one category and discard all others
consoles <- levels(sales$platform)[levels(sales$platform) %in% c("PS", "PS2", "PS3", 
                                                                 "PS4", "X360", "XB", "XOne")]
sales <- sales[sales$platform %in% consoles, ]
sales$platform <- factor(sales$platform)
sales$platform <- as.character(sales$platform)
sales$platform[sales$platform == "PS"] <- "Playstation"
sales$platform[sales$platform == "PS2"] <- "Playstation"
sales$platform[sales$platform == "PS3"] <- "Playstation"
sales$platform[sales$platform == "PS4"] <- "Playstation"
sales$platform[sales$platform == "PSP"] <- "Playstation"
sales$platform[sales$platform == "PSV"] <- "Playstation"
sales$platform[sales$platform == "X360"] <- "Xbox"
sales$platform[sales$platform == "XB"] <- "Xbox"
sales$platform[sales$platform == "XOne"] <- "Xbox"
sales$platform <- factor(sales$platform)

sales %>%
  ggplot(aes(x=platform, y=log(global_sales))) +
  geom_boxplot() +
  labs(title = "Platforms and Sales", x="Platform", y="Log Global Sales")

# consider games only after 2002 -- when both consoles were available in the market for the first time simultaneously
sales <- sales[sales$year_of_release >= 2002, ]


# year of release and global sales
sales %>%
  ggplot(aes(x=year_of_release, y=log(global_sales))) +
  geom_point()

# average sales per year
d1 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(mean_sales = mean(global_sales)))

d2 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales_min = min(global_sales)))

d3 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales10 = quantile(global_sales, probs = 0.1)))


d4 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales90 = quantile(global_sales, probs = 0.9)))

d5 <- as.data.frame(sales %>%
                      group_by(year) %>%
                      summarize(sales_max = max(global_sales)))


time_data <- merge(d1, d2, by = c("year"))
time_data <- merge(time_data, d3, by = c("year"))
time_data <- merge(time_data, d4, by = c("year"))
time_data <- merge(time_data, d5, by = c("year"))

gather(time_data, quantile, sales, mean_sales:sales_max) %>%
  ggplot(aes(x=year+1994, y=sales, color=quantile)) +
  geom_line() +
  theme_minimal()


# age of the game and global sales
sales %>%
  ggplot(aes(x=2017-year_of_release, y=log(global_sales))) +
  geom_point()





# user count -- rating seems to have a interaction with user_count
sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=platform)) +
  geom_point()

sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=genre)) +
  geom_point() +
  labs(title = "User Counts and Sales By Video Game Genre", x="User Count", y="Log Sales")

sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales), color=rating)) +
  geom_point()


# user score -- no pattern in any categorical variable
sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=platform)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=genre)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales), color=rating)) +
  geom_jitter()


# critic count -- rating seems to have a interaction with critic_count
sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=platform)) +
  geom_point()

sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=genre)) +
  geom_point()


sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=genre)) +
  geom_point()


sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=genre)) +
  geom_point()



sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales), color=rating)) +
  geom_point() +
  labs(title = "Sales, Critic Count and Rating", y="Log Global Sales", x="Critic Count")


# critic score -- no pattern in any categorical variable
sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=platform)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=genre)) +
  geom_jitter()

sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales), color=rating)) +
  geom_jitter()




# check if platforms have different different slopes and intercepts
sales %>%
  ggplot(aes(x=critic_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)

sales %>%
  ggplot(aes(x=critic_count, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


sales %>%
  ggplot(aes(x=log(user_count), y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


sales %>%
  ggplot(aes(x=user_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)

sales %>%
  ggplot(aes(x=user_score, y=log(global_sales))) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~platform)


write.csv(sales, "../data/01-sales-for-modeling.csv")
