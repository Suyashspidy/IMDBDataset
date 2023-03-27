rm(list = ls())
library(ggplot2)
mydata <- read.csv("iMDB_no_outlier.csv")
head(mydata)
mydata <- subset(mydata,select=-`movie_title`)
mydata <- subset(mydata,select=-`director_name`)
mydata <- subset(mydata,select=-`actor_1_name`)
mydata$budget <- as.numeric(gsub(",","",mydata$budget))

training_Data <- sample(nrow(mydata), 
                        size = nrow(mydata)*.6, replace=F)

training <- mydata[training_Data,]
head(training)
testset <- mydata[-training_Data,]

model <- lm(imdb_score ~.,data=training)
summary(model)
res <- resid(model)

fit <- fitted(model)

plot(fit, res)
abline(0,0)
qqnorm(res)
qqline(res) 
plot(density(res))

ggplot(model, aes(x = fit, y =res)) +
  geom_point() +
  stat_smooth(method = "lm")

pred_model <- predict(model,
                      testset)
forecast::accuracy(pred_model,testset$imdb_score)

#model1
model1 <- lm(imdb_score ~ num_critic_for_reviews
             +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget,data=training)
summary(model1)$adj.r.squared
ggplot(model1, aes(x = num_critic_for_reviews
                   +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget, y =imdb_score)) +
  geom_point() +
  stat_smooth(method = "lm")

#model2
model2 <- lm(imdb_score ~ num_critic_for_reviews
             +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross,data=training)
summary(model2)$adj.r.squared

#model3
model3 <- lm(imdb_score ~ num_critic_for_reviews
             +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration
             ,data=training)
summary(model3)$adj.r.squared
ggplot(model3, aes(x = num_critic_for_reviews
                   +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration, y =imdb_score)) +
  geom_point() +
  stat_smooth(method = "lm")

#Check prediction accuracy of top two models 
#model1 and model3
pred_imdb1 <- predict(model1,
                      testset)
head(data.frame(actual=testset$imdb_score,
                predicted=pred_imdb1,
                residuals=testset$imdb_score-pred_imdb1))

pred_imdb3 <- predict(model3,
                      testset)
head(data.frame(actual=testset$imdb_score,
                predicted=pred_imdb3,
                residuals=testset$imdb_score-pred_imdb3))

forecast::accuracy(pred_imdb1,testset$imdb_score)

forecast::accuracy(pred_imdb3,testset$imdb_score)

#We choose model3 because of the lower RMSE and MAPE, which means num_critic_for_reviews+director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration have more affect on imdb score compare to other variables.

mydataa <- read.csv("iMDB_outlier.csv")
head(mydataa)
mydataa <- subset(mydataa,select=-`movie_title`)
mydataa <- subset(mydataa,select=-`director_name`)
mydataa <- subset(mydataa,select=-`actor_1_name`)
mydataa$budget <- as.numeric(gsub(",","",mydataa$budget))

training_Dataa <- sample(nrow(mydataa), 
                         size = nrow(mydataa)*.6, replace=F)

trainingset <- mydataa[training_Dataa,]

validset <- mydataa[-training_Dataa,]

modela <- lm(imdb_score ~.,data=trainingset)
summary(modela)
res1 <- resid(modela)
res1
fit1 <- fitted(modela)
fit1
plot(fit1, res1)
abline(0,0)
qqnorm(res1)
qqline(res1) 
plot(density(res1))

ggplot(modela, aes(x = fit1, y =res1)) +
  geom_point() +
  stat_smooth(method = "lm")

pred_modela <- predict(modela,
                       validset)

forecast::accuracy(pred_modela,validset$imdb_score)

#model1
model1a <- lm(imdb_score ~ num_critic_for_reviews
              +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget,data=trainingset)
summary(model1a)$adj.r.squared


#model2
model2a <- lm(imdb_score ~ num_critic_for_reviews
              +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross,data=trainingset)
summary(model2a)$adj.r.squared
ggplot(model2a, aes(x = num_critic_for_reviews
                    +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross, y =imdb_score)) +
  geom_point() +
  stat_smooth(method = "lm")

#model3
model3a <- lm(imdb_score ~ num_critic_for_reviews
              +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration
              ,data=trainingset)
summary(model3a)$adj.r.squared
ggplot(model3a, aes(x = num_critic_for_reviews
                    +director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration, y =imdb_score)) +
  geom_point() +
  stat_smooth(method = "lm")

#Check prediction accuracy of top two models 
#model2a and model3a
pred_imdb2a <- predict(model2a,
                       validset)
head(data.frame(actual=validset$imdb_score,
                predicted=pred_imdb2a,
                residuals=validset$imdb_score-pred_imdb2a))

pred_imdb3a <- predict(model3a,
                       validset)
head(data.frame(actual=validset$imdb_score,
                predicted=pred_imdb3a,
                residuals=validset$imdb_score-pred_imdb3a))

forecast::accuracy(pred_imdb2a,validset$imdb_score)

forecast::accuracy(pred_imdb3a,validset$imdb_score)

#We choose model3a because of the lower RMSE and MAPE, which means num_critic_for_reviews+director_facebook_likes+actor_facebook_likes+movie_facebook_likes+num_user_for_reviews+budget+gross+duration have more affect on imdb score compare to other variables.

