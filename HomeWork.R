library(ggplot2)
library(tidyverse)
library(reshape2)
library(gridExtra)

df <- read_csv('season-1718_csv.csv')

# --------------Question 2 -----------
home_points <- function(x) {
  valeur <- 0
  if (x == 'H') {
    valeur <- 3
  } else if (x == 'D') {
    valeur <- 1
  } else if (x == 'A') {
    valeur <- 0
  }
  valeur
}

away_points <- function(x) {
  valeur <- 0
  if (x == 'H') {
    valeur <- 0
  } else if (x == 'D') {
    valeur <- 1
  } else if (x == 'A') {
    valeur <- 3
  }
  valeur
}

df$HomeTeamPoints <- unlist(lapply(df$FTR, home_points)) #Compute the number of points won by the Home team
df$AwayTeamPoints <- unlist(lapply(df$FTR, away_points)) #Compute the number of points won by the Away team
df_home <- data.frame(aggregate(df$HomeTeamPoints, by=list(Category=df$HomeTeam), FUN=sum)) #Compute the total number of points taken at home over the season
df_away <- data.frame(aggregate(df$AwayTeamPoints, by=list(Category=df$AwayTeam), FUN=sum)) #Compute the total number of points taken away over the season
colnames(df_home) <-  c('Team','HomePoints')
colnames(df_away)<- c('Team','AwayPoints')
df_merge <-merge(x=df_home, y=df_away)
df_merge$total_points <- rowSums(df_merge[,2:3])

number_points_arsenal <- df_merge[df_merge$Team=='Arsenal',4]
df_merge <- df_merge[order(df_merge$total_points),] 
df_merge$rank<- nrow(df_merge[order(df_merge$total_points),]) + 1 - 1:nrow(df_merge[order(df_merge$total_points),])
rank_arsenal <- df_merge[df_merge$Team=='Arsenal',5]
print(c(number_points_arsenal,rank_arsenal))

ggplot(df_merge) + geom_histogram(aes(HomePoints),bins=10)


df_melt <- melt(df_merge[,1:4], id=c('Team')) # transforming the data for the ggplot visualization

ggplot(df_melt[df_melt$variable != 'total_points',], aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.4,bins = 15) # histogram with ggplot

#histograms with r functions
par(mfrow=c(1,2))
hist(df_merge$HomePoints,col = 'red') 
hist(df_merge$AwayPoints,col = 'blue')

# densities plot
ggplot(df_melt, aes(x = value, fill = variable)) + geom_density(alpha = 0.5) +
  scale_y_continuous("Distribution") + 
  labs(title="Distribution of won Points")


# ---------- Question 4 -----------

home_mean <- mean(df$FTHG)
home_var <- var(df$FTHG)
away_mean <- mean(df$FTAG)
away_var <- var(df$FTAG)

# Means of goal scored at home and away
print(c(home_mean,away_mean))
df_goal_away <- data.frame(aggregate(df$FTAG, by=list(Category=df$AwayTeam), FUN= sum))
df_goal_home <- data.frame(aggregate(df$FTHG,by=list(Category=df$AwayTeam), FUN= sum))



df_goals <- melt(df[, 5:6])
comparison <- ggplot(df_goals, aes(x = value, fill = variable)) +
  geom_bar(alpha = 0.5,bins = 15)
l <- ggplot(df) + geom_bar(aes(FTAG), color='blue')
g <- ggplot(df) + geom_bar(aes(FTHG), color='red')
grid.arrange(l, g, comparison, nrow = 1)

# It seems that both variables are following a poisson distribution
# Given the questions of the first part, we know that theta_mle = empirical mean
# so we are going to compare our empirical data to the theoritical distributions both for the home and away goals
n<-nrow(df)
par(mfrow=c(2,2))
hist(rpois(n, home_mean),col ='lightblue')
hist(df_goals[df_goals$variable=='FTHG',2], col = 'darkblue')

hist(rpois(n, away_mean),col = 'orange')
hist(df_goals[df_goals$variable=='FTAG',2], col = 'red')
# --> when we compare the theoritcal data and the expiremental data, 
# it confirms that our statistical model with two poisson distribution

# ------ Question 5 -------------
CI_Poisson <- function(vector, alpha) {
  theta_mle <- mean(vector)
  n <- length(vector)
  t_alpha <- qt(1 - alpha/2, df=n-1)
  lower_bound <- theta_mle - t_alpha*sqrt(theta_mle/n)
  upper_bound <- theta_mle + t_alpha*sqrt(theta_mle/n)
  return(c(lower_bound,upper_bound))
}
print(CI_Poisson(df$FTHG, 0.05))
print(CI_Poisson(df$FTAG, 0.05))

print('It is likely that the two distributions are different : the 95%
     CI do not overlap')


# ------- Question 6 ----------

# We first run a wilcox test on Z = df$FTHG - df$FTAG. We will se whether the distribution is symetric or not
test <- wilcox.test(df$FTHG, df$FTAG, paired = T)
test$statistic
test

test_bis <- t.test(df$FTHG, df$FTAG)
test_bis

# ------- Question 7 ----------
goals_team <- function(team){
  goals <- c(df[df$HomeTeam==team,]$FTHG,df[df$AwayTeam==team,]$FTAG )
  return(goals)
}
goals_city <- goals_team('Man City')
goals_liverpool <- goals_team('Liverpool')
df_final <- data.frame(goals_city,goals_liverpool)
l <- ggplot(df_final) + geom_bar(aes(goals_city), color='blue')
g <- ggplot(df_final) + geom_bar(aes(goals_liverpool), color='red')
grid.arrange(l, g,  nrow = 1)
final_test <- t.test(goals_city,goals_liverpool)
final_test
# the p-value is equal to 11% : we cannot conclude whether a team scores more goal than the other