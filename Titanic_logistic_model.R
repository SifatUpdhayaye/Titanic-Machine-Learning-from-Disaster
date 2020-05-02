#Logistic regression model 
log.model <- glm(formula=Survived ~ . , family = binomial(link='logit'),data = df.train)

#predicting the result on the testing data
fitted.probabilities <- predict(log.model,newdata=df.test,type='response')

#if the prediction is above 50% then allocate 1 for susrived and 0 for passengers who did not survived 
fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

#checking the results
fitted.results

#creating a CSV file for submission of Kaggle
predict.titanic <- data.frame(PassengerId= titanic_test$PassengerId,Survived=df$Survived) 
write.csv(predict.titanic,"D:\\MyData.csv", row.names = FALSE)