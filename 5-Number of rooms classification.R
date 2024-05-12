install.packages("party")
install.packages("e1071")
library(party)
library(e1071)

houses <- read.csv("melb_data_cleaned.csv")
str(houses)

#Categorical to factor
houses <- houses[, c("Rooms", "Type", "Price", "Distance","Bathroom", "Car", "Landsize", "BuildingArea", "Method","Propertycount")]
houses$Type <- as.factor(houses$Type)
houses$Rooms <- as.factor(houses$Rooms)
houses$Method <- as.factor(houses$Method)
str(houses)

#Data splitting
ind <- sample(2,nrow(houses),prob=c(0.8,0.2),replace=TRUE)
train_data <- houses[ind==1, ]
test_data <- houses[ind==2, ]

#Decision Tree
tree_model <- ctree(Rooms ~ ., data = train_data)
plot(tree_model,type="simple")
tree_pred <- predict(tree_model, newdata = test_data)
table(Predicted = tree_pred,Actual = test_data$Rooms)

#SVM
svm_model <- svm(Rooms ~ ., data = train_data, kernel="radial")
svm_pred <- predict(svm_model, newdata = test_data)
table(Predicted = svm_pred,Actual = test_data$Rooms)

#Naive bayes
nb_model <- naiveBayes(Rooms ~ ., data = train_data)
nb_pred <- predict(nb_model, newdata = test_data)
table(Predicted = nb_pred,Actual = test_data$Rooms)

tree_accuracy <- sum(tree_pred == test_data$Rooms) / length(tree_pred)
svm_accuracy <- sum(svm_pred == test_data$Rooms) / length(svm_pred)
nb_accuracy <- sum(nb_pred == test_data$Rooms) / length(nb_pred)
tree_accuracy
svm_accuracy
nb_accuracy
