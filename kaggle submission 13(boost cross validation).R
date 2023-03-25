library(randomForest)
library(caret)
library(tidyverse)
library(dplyr)
library(rpart)
analysis=analysisData
install.packages("vtreat")
library(vtreat)
library(gbm)
set.seed(617)
split<- sample(1:nrow(analysis),size=nrow(analysis)*0.8)
train <- analysis[split, ]    
test <- analysis[-split, ]
train<- train[!is.na(train$calculated_host_listings_count),]
train<- train[!is.na(train$security_deposit),]
train<- train[!is.na(train$cleaning_fee),]
train<- train[!is.na(train$accommodates),]
train<- train[!is.na(train$host_listings_count),]
train<- train[!is.na(train$neighbourhood_cleansed),]
train<- train[!is.na(train$room_type),]
train<- train[!is.na(train$bathrooms),]
train<- train[!is.na(train$bedrooms),]
train<- train[!is.na(train$guests_included),]
train<- train[!is.na(train$cancellation_policy),]
train<- train[!is.na(train$availability_30),]
train<- train[!is.na(train$availability_90),]
train<- train[!is.na(train$number_of_reviews),]
train<- train[!is.na(train$price),]
train<- train[!is.na(train$review_scores_rating),]
train<- train[!is.na(train$review_scores_checkin),]
train<- train[!is.na(train$review_scores_communication),]
train<- train[!is.na(train$review_scores_value),]
train<- train[!is.na(train$minimum_nights),]
train<- train[!is.na(train$review_scores_location),]
trControl=trainControl(method="cv",number=10)
tuneGrid=  expand.grid(n.trees = 1000, interaction.depth = c(1,2),
                       shrinkage = (1:10)*0.01,n.minobsinnode=5)
garbage = capture.output(cvModel <- train(price~calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+cancellation_policy+availability_30+availability_90+number_of_reviews+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_value+review_scores_location,
                                          data=train,
                                          method="gbm",
                                          trControl=trControl, 
                                          tuneGrid=tuneGrid))
set.seed(617)
cvBoost = gbm(price~calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+cancellation_policy+availability_30+availability_90+number_of_reviews+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_value+review_scores_location,
              data=train,
              distribution="gaussian",
              n.trees=cvModel$bestTune$n.trees,
              interaction.depth=cvModel$bestTune$interaction.depth,
              shrinkage=cvModel$bestTune$shrinkage,
              n.minobsinnode = cvModel$bestTune$n.minobsinnode)
pred = predict(cvBoost,scoringData,n.trees=500)









