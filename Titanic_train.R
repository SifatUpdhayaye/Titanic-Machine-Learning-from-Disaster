df.train <- titanic_train

print(head(df.train))
print(str(df.train))

#checking for the missing or NA values in the data set
library(Amelia)
print( missmap(df.train,main="Missing Map",col =c("yellow","black"),legend = T))

# Exploratory data analysis
library(ggplot2)
ggplot(df.train,aes(Survived))+geom_bar()
ggplot(df.train,aes(Pclass))+geom_bar(aes(fill= factor(Pclass)))        
ggplot(df.train,aes(Sex))+geom_bar(aes(fill= factor(Sex)))        
ggplot(df.train,aes(Age))+geom_histogram(bins = 20 , alpha=0.5,fill="blue")
ggplot(df.train,aes(SibSp))+geom_bar()
ggplot(df.train,aes(Fare))+geom_histogram(bins = 20 , alpha=0.5,fill="blue")

#finiding the mean age for each class to gill the missing age data
pl <- ggplot(df.train,aes(Pclass,Age)) + geom_boxplot(aes(group=Pclass,fill=factor(Pclass),alpha=0.4)) 
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2))

#function to impute age
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}


fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages

#checking if the data set is complete and consistent for analysis
missmap(df.train, main="Titanic Training Data - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

#removing unwanted colums
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)

#converting the columns into factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)

