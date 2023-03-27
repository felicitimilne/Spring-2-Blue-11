library(randomForest)
#library(datasets)
library(caret)

#make not-fraud model with blue data
#RF model use over-sampled data
#compare with AUC on training

# Load Needed Data #
# setwd("G:/My Drive/MSA/2022 Spring/AA 503/Fraud Detection/Assignments")
#ins <- read.csv(file = "aggregated_policy_label_2023B_oversampled.csv", header = TRUE)
ins <- read.csv(file = "aggregated_policy_label_2023B.csv", header = TRUE)
ins$Fraud <- as.factor(ins$Fraud)
ins[is.na(ins)] <- 0

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(ins)), size = floor(0.7*nrow(ins))) #70/30 train/test split

train <- ins[train_id,]
test <- ins[-train_id,]

table(train$Fraud)
table(test$Fraud)

# NOT Fraud Model #
train$NOTFraud <- ifelse(train$Fraud == "1", 0, 1) #make NOT fraud the "target"
train$NOTFraud <- as.factor(train$NOTFraud) #make it a factor
test$NOTFraud <- ifelse(test$Fraud == "1", 0, 1) #make NOT fraud the "target"
test$NOTFraud <- as.factor(test$NOTFraud) #make it a factor
#weight <- ifelse(train$NOTFraud == 1, 65.67, 1) #add the weight

# build Random Forest Model
#, w=train$WEIGHT
nf_RFmodel <- randomForest(NOTFraud~., data=train)
print(nf_RFmodel)

# predictions
pred_nf <- predict(nf_RFmodel, newdata = test)
confusionMatrix(pred_nf, test$NOTFraud)

