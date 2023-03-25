analysis=analysisData
scoring=scoringData
library(ggcorrplot)
library(tidyverse)
library(dplyr)
library(caret)
library(randomForest)
library(ipred)
library(gbm)
library(xgboost)
library(ggcorrplot)
summary(analysis)
ggplot(analysis,aes(x=minimum_nights,y=price,color='red'))+
  geom_point()
library(ggcorrplot)
ggcorrplot(cor(analysis[,c(1,47,42,43,54,64,68,87)]),type = 'lower',show.diag = F,colors = c('red','white','darkgreen'))
model1=lm(price~room_type+calculated_host_listings_count+host_total_listings_count+accommodates+bathrooms+bedrooms+beds+bed_type+security_deposit+guests_included+extra_people+minimum_nights+maximum_nights+number_of_reviews+property_type+
                   cleaning_fee+availability_30+review_scores_rating+review_scores_accuracy+
                   review_scores_cleanliness+review_scores_checkin+review_scores_communication+
                   review_scores_location+review_scores_value+cancellation_policy+reviews_per_month,data=analysis)

summary(model1)

model2=lm(price~extra_people+availability_60+availability_90+availability_365+host_response_time,analysis)
summary(model2)
analysis$zipcode=as.numeric(analysis$zipcode)
scoring$zipcode=as.numeric(scoring$zipcode)
train=analysis[ ,c('room_type','calculated_host_listings_count','host_total_listings_count','zipcode','accommodates',
                   'bathrooms','bedrooms','beds','security_deposit','guests_included','extra_people',
                    'minimum_nights','number_of_reviews','property_type',
                   'cleaning_fee','availability_30','review_scores_rating','review_scores_accuracy',
                   'review_scores_cleanliness','review_scores_checkin','review_scores_communication',
                   'review_scores_location','review_scores_value','cancellation_policy','reviews_per_month'
                    ,'price')]
trt=designTreatmentsZ(train,varlist=names(train)[1:25])
train1=prepare(treatmentplan = trt,
                    dframe=analysis)
test1=prepare(treatmentplan = trt,
                   dframe=scoring)
tune_nrounds=xgb.cv(data=as.matrix(train1),
                    label=train$price,
                    nrounds=250,
                    nfold=5,
                    verbose=0)
which.min(tune_nrounds$evaluation_log$test_rmse_mean)

xgboost= xgboost(data=as.matrix(train1), 
                  label = analysis$price,
                  nrounds=60,
                  verbose = 0)
xgboost$evaluation_log
pred3=predict(xgboost,newdata=as.matrix(test1))
submissionFile=data.frame(id = scoring$id, price = pred3)
write.csv(submissionFile, '~/Desktop/kaggle submission.csv',row.names = F)
















