
df.test <- titanic_test

#checking the missing data in the test data det
library(Amelia)
print( missmap(df.test,main="Missing Map",col =c("yellow","black"),legend = F))

#EDA
ggplot(df.test,aes(Pclass))+geom_bar(aes(fill= factor(Pclass)))        
ggplot(df.test,aes(Sex))+geom_bar(aes(fill= factor(Sex)))        
ggplot(df.test,aes(Age))+geom_histogram(bins = 20 , alpha=0.5,fill="blue")
ggplot(df.test,aes(SibSp))+geom_bar()
ggplot(df.test,aes(Fare))+geom_histogram(bins = 20 , alpha=0.5,fill="blue")

#computing the missing ages
fixed.ages.test <- impute_age(df.test$Age,df.test$Pclass)
df.test$Age <- fixed.ages.test

#computing the missing Fare
library(ggolot2)
library(plotly)
pl <- ggplot(df.test,aes(Fare ,Pclass)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_x_continuous(breaks = seq(min(0), max(1000), by = 10))

impute_Fare <- function(fare,class){
  out <- fare
  for (i in 1:length(fare)){
    
    if (is.na(fare[i])){
      
      if (class[i] == 1){
        out[i] <- 60
        
      }else if (class[i] == 2){
        out[i] <- 15
        
      }else{
        out[i] <- 6
      }
    }else{
      out[i]<-fare[i]
    }
  }
  return(out)
}

fixed.fare.test <- impute_Fare(df.test$Fare,df.test$Pclass)
df.test$Fare <- fixed.fare.test

#checking if the data is complete and ready for prediction 
missmap(df.test, main="Titanic Testing Data - Missings Map", 
        col=c("yellow", "black"), legend=T)

#removing unwanted columns
df.test <- select(df.test,-PassengerId,-Name,-Ticket,-Cabin)

#converting into factors
df.test$Pclass <- factor(df.test$Pclass)
df.test$SibSp <- factor(df.test$SibSp)

