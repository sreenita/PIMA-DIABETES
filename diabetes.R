getwd()
install.packages("mlbench")
library(mlbench)
diabetes<-read.csv("diabetes.csv")
View(diabetes)
##knn

library(caTools)
library(class)
all_test_accuracies_knn <- matrix(nrow=100,ncol=9)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  
  neighbors <- c(2:10)
  accuracies <- matrix(nrow=1, ncol=9)
 accuracies
  
  for (n_neighbors in neighbors){
    knn_fit <- knn(diabetes[train_ind,],diabetes[test_ind,],diabetes$Outcome[train_ind],k=n_neighbors)
    cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = knn_fit)
    View(cm)
    accuracy <- sum(diag(cm))/sum(test_ind)
    accuracy
    accuracies[n_neighbors-1] <- accuracy
  }
 accuracy
  all_test_accuracies_knn[split_number,] <- accuracies
}
##logisticregression

all_test_accuracies_logistic <- matrix(nrow=100,ncol=1)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  
  logit_fit <- glm(Outcome ~ ., data=diabetes[train_ind,], family="binomial")
  p <- predict(logit_fit,diabetes[test_ind,],family="binomial")
  probs <- exp(p)/(1+exp(p))
  test_outcomes <- probs>0.5
  cm <- table(Actual = diabetes$Outcome[test_ind],Predicted = test_outcomes)
  accuracy <- sum(diag(cm))/sum(test_ind)
  accuracy
  all_test_accuracies_logistic[split_number] <- accuracy
}
##decision tree

library(rpart)
all_dtree_importance <- matrix(nrow=8,ncol=100)
bucketsize <- 10
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  tree <- rpart(as.factor(Outcome) ~ ., data = diabetes[train_ind,],minbucket=bucketsize, model=TRUE)
  importance <- t(tree$variable.importance)
  importance <- importance/sum(importance)
  all_dtree_importance[1,split_number] <- importance[,"Glucose"]
  all_dtree_importance[2,split_number] <- importance[,"BMI"]
  all_dtree_importance[3,split_number] <- importance[,"Age"]
  all_dtree_importance[4,split_number] <- importance[,"Insulin"]
  all_dtree_importance[5,split_number] <- importance[,"DiabetesPedigreeFunction"]
  all_dtree_importance[6,split_number] <- importance[,"Pregnancies"]
  all_dtree_importance[7,split_number] <- importance[,"BloodPressure"]
  all_dtree_importance[8,split_number] <- importance[,"SkinThickness"]
}
##randomforest

library(randomForest)
for (split_number in c(1:100)){
  train_ind <- sample.split(diabetes$Pregnancies,SplitRatio = 0.8)
  test_ind <- !train_ind
  rf <- randomForest(as.factor(Outcome) ~ ., data = diabetes[train_ind,],ntree=100)
  train_accuracy <- sum(diag(rf$confusion))/sum(train_ind)
  cm <- table(predict(rf,diabetes[test_ind,]),diabetes$Outcome[test_ind])
  test_accuracy <- sum(diag(cm))/sum(test_ind)
  
  all_train_accuracies_rf[split_number] <- train_accuracy
  all_test_accuracies_rf[split_number] <- test_accuracy
  
  importance <- rf$importance/sum(rf$importance)
  all_importances_rf[split_number,1] <- importance["Glucose",]
  all_importances_rf[split_number,2] <- importance["BMI",]
  all_importances_rf[split_number,3] <- importance["Age",]
  all_importances_rf[split_number,4] <- importance["Insulin",]
  all_importances_rf[split_number,5] <- importance["DiabetesPedigreeFunction",]
  all_importances_rf[split_number,6] <- importance["Pregnancies",]
  all_importances_rf[split_number,7] <- importance["BloodPressure",]
  all_importances_rf[split_number,8] <- importance["SkinThickness",]
}
