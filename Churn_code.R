library(ggplot2)
library(ggthemes)
library(psych)
library(caTools)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
df<-read.csv("Churn.csv",header = T)
str(df)
colnames(df)
#Reading the data
summary(df)
View(head(df,5))
table(df$SeniorCitizen)
df$SeniorCitizen<-as.factor(df$SeniorCitizen)
#Checking NA values
sum(is.na(df))
#All values are in TotalCharges
describe(df$TotalCharges)
#Visualizing the data.
plot(table(df[,21]), type = "h" , lwd=70, lend=2, col="darkblue")
#There is a class biasness
#Changing level of churn from NO/YES to STAYED/LEFT, to understand graph 
levels(df$Churn)[levels(df$Churn)=="Yes"]<-"LEFT"
levels(df$Churn)[levels(df$Churn)=="No"]<-"STAYED"
ggplot(df, aes(Churn, ..count..)) + geom_bar(aes(fill =gender))+
  stat_count(aes(y=..count.., label=..count..), geom="text")
#Both are almost equal, we can't say anything yet.
ggplot(df, aes(Churn, ..count..)) + geom_bar(aes(fill = SeniorCitizen))+
  stat_count(aes(y=..count.., label=..count..), geom="text")
#A non-senior citizen is more likely to stay.
ggplot(df, aes(Churn, ..count..)) + geom_bar(aes(fill = PhoneService), position = "dodge") +
  stat_count(aes(y=..count.., label=..count..), geom="text")
#We can state that the person having no 
#phone services is more likely to stay, as their % is more in stayed part is more.
ggplot(df, aes(Churn, ..count..)) + geom_bar(aes(fill = Partner), position = "dodge") +
  stat_count(aes(y=..count.., label=..count..), geom="text")
#Can't say anyhting.
ggplot(df, aes(Churn, ..count..)) + geom_bar(aes(fill = Contract), position = "dodge") +
  stat_count(aes(y=..count.., label=..count..), geom="text")
#Person with one or two year contract is highly likely to stay.
##Randomly shuffling the data
grp = runif(nrow(df))
df= df[order(grp),]
#To make our model efficient removing variables and changing levels
summary(df)
levels(df$MultipleLines)[levels(df$MultipleLines)=="No phone service"]<-"No"
levels(df$OnlineSecurity)[levels(df$OnlineSecurity)=="No internet service"]<-"No"
levels(df$OnlineBackup)[levels(df$OnlineBackup)=="No internet service"]<-"No"
levels(df$DeviceProtection)[levels(df$DeviceProtection)=="No internet service"]<-"No"
levels(df$TechSupport)[levels(df$TechSupport)=="No internet service"]<-"No"
levels(df$StreamingTV)[levels(df$StreamingTV)=="No internet service"]<-"No"
levels(df$StreamingMovies)[levels(df$StreamingMovies)=="No internet service"]<-"No"
names(df)
df.n<-na.omit(df)
df1<-df.n[,c(-1,-2,-12)]

##Distributing it into test and train
split <- sample.split(df1$Churn, SplitRatio = 7/10)
train <- subset(df1, split == TRUE)
test <- subset(df1, split == FALSE)
#To make our model efficient removing variables
##Using GLM
model<-glm(formula=Churn~.,family = "binomial",data=train)
prediction<-predict(model,newdata = test)
result<-ifelse(prediction>0.5,"STAYED","LEFT")
table(Actual=test$Churn,Predicted=result)
##SVM
Model2 = svm(formula = Churn ~ ., 
                 data = train, 
                 type = 'C-classification', 
                 kernel = 'linear')
prediction2<-predict(Model2,newdata = test)

confusionMatrix(table(Actual=test$Churn,Predicted=prediction2))
###Decision trees.
Model3 <- rpart(
  formula = Churn ~ .,
  data    = train,
  method  = "class"
)
summary(Model3)
plotcp(Model3)
rpart.plot(Model3)      
prediction3<-predict(Model3, newdata = test, type = "class")

confusionmatrix(table(Actual=test$Churn,Predicted=prediction3))

                