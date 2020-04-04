#Setup
library(ggplot2)
library(tidyr)
library(dplyr)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#head(ny)

#head(wash)

#head(chi)

#creat missing columns for Washington
wash$Gender <- NA
wash$Birth.Year <- NA

#Generating dummies for dataframe
ny$city = 1
ny$city_str = "New-York"
wash$city = 2
wash$city_str = "Washington"
chi$city = 3
chi$city_str = "Chicago"
#Appending data
df <- rbind(ny, wash, chi)

#checking for missing (ignore Gender and Bithyear in Washington)
for (i in 1:3) {
    missings <- colSums(apply(X = subset(df, df$city == i), MARGIN = 2, is.na))
    print(paste("City: ", i))
    print(missings[which(missings > 0)])
}
#Missings in Trip.Duration in NY and Washington
#Birth.Year has missings in Chicago

#drop missing Trip Duration
df_clean <- subset(df, df$Trip.Duration != 'NA')

#Tidy up Start.Time 
df_clean$Start_clean <- substr(df_clean$Start.Time, 1, 10)
df_clean <- separate(df_clean, Start_clean, into = c("year", "month", "day"), sep = c("-"))

#check for duplicates in the ID
dups <- (xtabs(~df_clean$X, data = df_clean))
table(dups)

#Trip.Duration is in seconds --> calculate minutes
df_clean$duration_min = df_clean$Trip.Duration/60
#calculate the age
df_clean$age = 2017-df_clean$Birth.Year

attach(df_clean)
names(df_clean)

print(table(year, month))
#only data 6 month in 2017

#preparing data for plot
bike_freq <- df_clean %>%
    select (X, month,city, city_str) %>%
    group_by(city_str, month) %>%
    summarise(X.count=n())
bike_freq

plot_bike_freq <- ggplot(data = bike_freq, aes(x = month, y = X.count, fill = city_str)) +
    geom_bar(stat = "identity") +
    labs(title = "Bikesharing frequeny per month", x = "Month", y ="Number of usage") +
    facet_wrap(~city_str) +
    scale_fill_brewer(palette = "Greens") +
    theme_bw() +
    theme(legend.position = "bottom")
plot_bike_freq

#preparing data for plot
bike_freq_perc <- df_clean %>%
    select (X, month,city, city_str) %>%
    group_by(month, city_str) %>%
    summarise(X.count=n()) %>%
    mutate(X.count = X.count/sum(X.count)*100)

plot_bike_freq_perc <- ggplot(data = bike_freq_perc, aes(x = month, y = X.count, fill = city_str)) +
    geom_bar(stat = "identity") +
    labs(title = "Bikesharing per Month (in Percentage)", x = "Month", y ="Percentage of Bikesharing") +
    scale_fill_brewer(palette = "Greens") +
    theme_bw() +
    theme(legend.position = "bottom")
plot_bike_freq_perc

summary(duration_min)

f <- function(x) {
    list(min (x), mean(x), max(x))
}
sapply(split(duration_min, city_str), f)

#There are some outlier
#check for p99
quantile(duration_min, probs = 0.99)
#cut there
df_duration <- subset(df_clean, duration_min < 122)

#new descriptives
sapply(split(df_duration$duration_min, city_str), f)
# new max now around 2 hours in each city

box_duration <- ggplot(data = df_duration, aes(x = city_str, y = duration_min)) +
    geom_boxplot() +
    labs(title = "Distribution of trip duration in Minutes", y = "Duration in Minutes") +
    theme_bw()
box_duration

#Histogram
hist_duration <- ggplot(data = df_duration, aes(x = duration_min, y = ..density..)) +
    geom_histogram() +
    labs(title = "Distribution of trip duration in Minutes", y = "Duration in Minutes") +
    facet_grid(city_str~.) +
    theme_bw()
hist_duration

summary(age)

#new dataset without missings
df_age <- subset(df_clean, age != 'NA')

#building age ggroups
df_age <- df_age %>%
    mutate(age_group = case_when(
        age %in% 10:19 ~ 10,
        age %in% 20:29 ~ 20,
        age %in% 30:39 ~ 30,
        age %in% 40:49 ~ 40,
        age %in% 50:59 ~ 50,
        age %in% 60:69 ~ 60,
        age %in% 70:79 ~ 70,
        age %in% 80:89 ~ 80,
        age %in% 90:132 ~ 90
    ))

#checking the age groups in Percent
df_age_group <- df_age %>%
    select(X, age_group, city_str) %>%
    group_by(city_str, age_group) %>%
    summarise(X.count=n()) %>%
    mutate(X.count = X.count/sum(X.count)*100)
df_age_group

box_age <- ggplot(data = df_age, aes(x = city_str, y = age)) +
    geom_boxplot() +
    labs(title = "Distribution of the age in years", y = "Years") +
    theme_bw()
box_age

#Histogram
hist_age <- ggplot(data = df_age, aes(x = age, y = ..density..)) +
    geom_histogram() +
    labs(title = "Distribution of the age in years", y = "Years") +
    facet_grid(city_str~.) +
    theme_bw()
hist_age

#preparing the data
#summary(df_age)
#drop the outlier from duration in minutes
df_user <- subset(df_age, df_age$duration_min < quantile(df_age$duration_min, probs = 0.99))
#drop the outlier from age
df_user <- subset(df_user, df_user$age < quantile(df_user$age, probs = 0.99))
#missings in Gender
df_user <- subset(df_user, df_user$Gender == 'Male' | df_user$Gender == 'Female')
#missing in user type
df_user <- subset(df_user, df_user$User.Type == 'Customer' | df_user$User.Type == 'Subscriber')
#checking the observations
dim(df_user)
#around 55000 obs left

#Looking at gender and gender with age groups
gender_use <- df_user %>%
    select(X, Gender) %>%
    group_by(Gender) %>%
    summarize(X.count=n())%>%
    mutate(X.perc = X.count/sum(X.count)*100)
gender_use

gender_age_use <- df_user %>%
    select (X, age_group, Gender) %>%
    group_by(age_group, Gender) %>%
    summarize(X.count=n()) %>%
    mutate(X.perc = X.count/sum(X.count)*100)
gender_age_use

#Duration for gender
#Looking at gender and gender with age groups
gender_duration <- df_user %>%
    select(X, Gender, duration_min) %>%
    group_by(Gender) %>%
    summarize(duration_mean=mean(duration_min)) 
gender_duration

#What are the Customer types?
customer_use <- df_user %>%
    select(X, User.Type) %>%
    group_by(User.Type) %>%
    summarize(X.count=n()) %>%
    mutate(X.perc = X.count/sum(X.count)*100)
customer_use

#data for plot
user_prep <- df_user %>%
    select(X, Gender, duration_min, age) %>%
    group_by(Gender, age) %>%
    summarize(
        X.count=n(),
        duration_mean = mean(duration_min)
    )
#dropping the outlier
user_prep <- subset(user_prep, age>18)
#user_prep

#Plot Gender, Age and Duration
plot_user <- ggplot(data = user_prep, aes(x = age, y = duration_mean, group = Gender)) +
    geom_jitter(aes(color = Gender)) +
    geom_smooth(method = lm, se = FALSE) +
    labs(title = "Distribution of duration and age by gender", x = "Age", y = "Duration") +
    theme_bw()
plot_user

system('python -m nbconvert Explore_bikeshare_data.ipynb')
