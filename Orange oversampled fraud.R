blue_over <- read.csv(file = "C:/Users/Jason Blaisdell/Documents/Grad School/503/Anomaly Detection and Fraud/aggregated_policy_label_2023B_oversampled.csv", header = TRUE)
orange_over <- read.csv(file = "C:/Users/Jason Blaisdell/Documents/Grad School/503/Anomaly Detection and Fraud/aggregated_policy_label_2023O_oversampled.csv", header = TRUE)

blue <- read.csv(file = "C:/Users/Jason Blaisdell/Documents/Grad School/503/Anomaly Detection and Fraud/aggregated_policy_label_2023B.csv", header = TRUE)
orange <- read.csv(file = "C:/Users/Jason Blaisdell/Documents/Grad School/503/Anomaly Detection and Fraud/aggregated_policy_label_2023O.csv", header = TRUE)


orange_over[is.na(orange_over)] = 0

orange_over <- orange_over[,!(names(orange_over) %in% "WEIGHT")]

tree <- randomForest(as.factor(Fraud) ~ ., data = orange_over, ntree = 500)

blue$prob <- predict(tree, newdata = blue, type = 'prob')[,2]

write.csv(blue[order(blue$prob),"Cust_ID"][1:500],"C:/Users/Jason Blaisdell/Documents/Grad School/503/Anomaly Detection and Fraud/Top 500.csv")

auc(blue$Fraud ,blue$prob)

