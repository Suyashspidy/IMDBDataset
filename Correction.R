#library(tidyverse)
library(ggplot2)
myData <- read.csv('movie_metadata.csv')
plot(myData$imdb_score~myData$gross, main = 'Gross Revenue vs IMBD Score',
     xlab = "Gross Revenue", ylab = "IMBD Score",col='red', pch = 20, 
     xlim= c(0,800000000), ylim= c(1,10))
plot(myData$imdb_score~myData$director_facebook_likes, main = 'Director Facebook Likes vs IMBD Score',
     xlab = "Director Facebook Likes", ylab = "IMBD Score",col='red', pch = 20, 
     xlim= c(0,23000), ylim= c(1,10))
plot(myData$imdb_score~myData$actor_1_facebook_like, main = 'Lead Actor facebook Likes vs IMBD Score',
     xlab = "Lead Actor facebook Likes", ylab = "IMBD Score",col="aquamarine4", pch = 20, 
     xlim= c(0,30000), ylim= c(1,10))
ggplot(data= myData) + geom_smooth(mapping = aes(x= gross, y = imdb_score))
ggplot(data= myData) + geom_point(mapping = aes(x= gross, y = imdb_score, color = color))
