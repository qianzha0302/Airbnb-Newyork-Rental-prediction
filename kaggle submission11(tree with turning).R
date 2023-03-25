library(caret)
train$require_guest_phone_verification <- as.factor(train$require_guest_phone_verification)
train$host_is_superhost <- as.factor(train$host_is_superhost)
train$host_identity_verified <- as.factor(train$host_identity_verified)
train$neighbourhood_group_cleansed <- as.factor(train$neighbourhood_group_cleansed)
train$room_type <- as.factor(train$room_type)
train$cancellation_policy <- as.factor(train$cancellation_policy)
train$is_business_travel_ready <- as.factor(train$is_business_travel_ready)
train$require_guest_phone_verification<- as.factor(train$require_guest_phone_verification)
summary(train)
train%>%
  drop_na(calculated_host_listings_count,extra_people,price,security_deposit,cleaning_fee,accommodates,host_listings_count,neighbourhood_group_cleansed,room_type,bathrooms,bedrooms,guests_included,minimum_nights,review_scores_rating,review_scores_checkin,review_scores_communication,review_scores_location,review_scores_value,cancellation_policy,beds,reviews_per_month,availability_30,availability_90,number_of_reviews,host_is_superhost)

train<- train[!is.na(train$host_is_superhost),]
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

library(rpart)


trControl=trainControl(method="cv",number=10)
tuneGrid = expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001))
set.seed(617)
cvModel = train(price~calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+cancellation_policy+availability_30+availability_90+number_of_reviews+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_value+review_scores_location,
                data=train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)
summary(cvModel)
cvTree = rpart(price~calculated_host_listings_count+extra_people+security_deposit+cleaning_fee+accommodates+host_listings_count+neighbourhood_group_cleansed+room_type+bathrooms+bedrooms+guests_included+minimum_nights+cancellation_policy+availability_30+availability_90+number_of_reviews+review_scores_rating+review_scores_checkin+review_scores_communication+review_scores_value+review_scores_location,data=train,cp = cvModel$bestTune$cp)
pred_cvTree = predict(cvTree,newdata=scoringData)
submissionFile_cvtree = data.frame(id = scoringData$id, price = pred_cvTree)
sum(is.na(submissionFile_cvtree$price))
write.csv(submissionFile_cvtree, '~/Desktop/kaggle submission.csv',row.names = F)






