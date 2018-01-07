rm(list=ls(all=TRUE)) #workspace cleanup
library(ggplot2); library(ggthemes)   #for visualization
library(mice)                         #for imputation
library(dplyr)                        #for data handling
library(glmx)                         #for running GLM model (logistic regression)
library(randomForest)                 #for random forest
library(evtree)                       #for classification trees
library(e1071)                        #for Naive bayesian model

setwd("D:/Study/OneDrive - The University of Texas at Dallas/Winter break 1/titanic")
train <- read.csv('train.csv',stringsAsFactors = F,na.strings=c(""))
test <- read.csv('test.csv',stringsAsFactors = F, na.strings=c(""))
head(train)
summary(train)

#calculating family size based on the number of children/parents aboard the ship
train$famsize <- train$SibSp+train$Parch+1
test$famsize <- test$SibSp+test$Parch+1

## families of 4 individuals seem to have the higest survival rate
## generally, families sized 2-4 have a higher survival rate than others
ggplot(train, aes(x = famsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='fill') +
  scale_x_continuous(breaks=c(1:12)) +
  labs(x = 'family size', y = 'proportion')

#Females tend to have better chances at survival
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = 'fill') +
  labs(x = 'sex', y = 'proportion of survival')

#First class has better chances at survival
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(position = 'fill') +
  labs(x = 'Passenger Class', y = 'proportion of survival')

#Breaking down the gender data by passenger class
#Class and gender combined give a better picture
#First and Second class females are highly likely to have survuved
ggplot(train, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = 'fill') +
  labs(x = 'sex/class', y = 'proportion of survival') + facet_grid(~Pclass)

##Plotting survival rate with respect to age
# Younger people have better survival rates
df <- aggregate(formula=Survived~Age, 
                data=train, FUN=function(x) { mean(x) })
ggplot(df,aes(x=Age, y=Survived)) + 
  geom_smooth()

##Survival based on age and fare, broken down by gender and passenger class
#This chart indicates that classification tree could be a good techinque for
#the data set. Notice how there are few deaths in the top left and top middle
#chart. In the top right chart, we can see a cluster of deahts towards the
#higher age. In the bottom middle chart, the survivors are clustered towards 
#lower age. In the bottom right chart, the survivors are clustered close to
#the origin. The bottom left chart is less straightforward.
ggplot(train, aes(x=Age,y=Fare, color=as.factor(Survived))) +
  geom_point(size=2) + facet_grid(Sex~Pclass)

##missing data
#lots of Age data is missing
cbind(sapply(train,function(x) sum(is.na(x))))
cbind(sapply(test,function(x) sum(is.na(x))))

##Combining the training and testing dataset
full <- bind_rows(train, test)
#263 rows with Age data missing, thats roughly 20% rows
#this being a small data set, we cannot just remove 20% of the rows
cbind(sapply(full,function(x) sum(is.na(x))))

#Finding the people with missing Embarked data
subset(full, is.na(full$Embarked))

#both have a fare of 80. This fits in well with other people who Embarked
#at C and were in 1st class
ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() + geom_hline(aes(yintercept=80))

#It is a good guess to mark these two people as embarked from C
full$Embarked[c(62, 830)] <- 'C'

#Finding the individual with the missing Fare
subset(full, is.na(full$Fare))

#Its logical to think that tickets are similarly priced for a specific class
# at a specific port. So we filter the data which has the same class and 
# port as the individual with the missing fare
temp1 <- full[(full$Embarked == 'S' & full$Pclass == 3),]

#We see thatthe average Fare from port S and class 3 is 7.854.
summary(temp1)
ggplot(temp1, aes(x=Fare)) + 
  geom_density(fill='red', alpha = 0.3) +
  geom_vline(aes(xintercept=7.854))
full$Fare[1044] <- 7.854

set.seed(176)
mice <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket',
                                            'Cabin','Survived')], method='rf') 

full_imp <- complete(mice)
cbind(sapply(full_imp,function(x) sum(is.na(x))))

ggplot() +
  geom_density(data = full, aes(x = Age), color= NA , fill='darkred', alpha = 0.4 ) +
  geom_density(data = full_imp, aes(x=Age), color='darkred', fill='darkred', alpha = 0.0 ) +
  labs(x = 'age', y = 'frequency') 

summary(full$Age)
summary(full_imp$Age)

full$Age <- full_imp$Age
cbind(sapply(full,function(x) sum(is.na(x))))

train2 <- full[1:891,]
test2 <- full[892:1309,]

# temp2 <- c('Survived','Pclass','Sex','Embarked')
# train2[temp2] <- sapply(train2[temp2],function(x) as.factor(x))
# test2[temp2] <- sapply(test2[temp2],function(x) as.factor(x))
#code not working as i intended it to

head(train2)

train2$Survived <- as.factor(train2$Survived)
train2$Pclass <- as.factor(train2$Pclass)
train2$Sex <- as.factor(train2$Sex)
train2$Embarked <- as.factor(train2$Embarked)

test2$Survived <- as.factor(test2$Survived)
test2$Pclass <- as.factor(test2$Pclass)
test2$Sex <- as.factor(test2$Sex)
test2$Embarked <- as.factor(test2$Embarked)

formula <- Survived ~ Pclass + Sex + Age + SibSp + Parch +
  Fare + Embarked + famsize

##Classification tree visualization
set.seed(176)
model1 <- evtree(formula, data=train2)
model1
plot(model1)
#classification error
acc1 <- 1 - mean(predict(model1) == train2$Survived)  

##Logit regression
model2 <- glm(formula, data=train2, family=binomial(link="logit"))
summary(model2)
#classification error
acc2 <- 1 - mean(round(predict(model2, type="response")) == train2$Survived)  #classification error

##Random Forest
set.seed(176)
model3 <- randomForest(formula, data = train2)
#classification error
acc3 <- 1 - mean(predict(model3) == train2$Survived)
plot(model3, ylim=c(0,0.36))
legend('topright', colnames(model3$err.rate), col=1:3, fill=1:3)

##Naive Bayesian method
model4 <- naiveBayes(formula, data = train2)
acc4 <- 1 - mean(predict(model4, train2, type="class") == train2$Survived)  #classification error

##Comparing model training accuracy (lower is better)
acc <- data.frame(cbind(c("Classification Tree","Logistic Regression",
                          "Random Forest","Naive Bayesian"),
                        c(acc1,acc2,acc3,acc4)*100))
colnames(acc) <- c("Method","Classification error %")
acc

## Predicting survival on test data using random forest
# I am using random forest and not classification tree because
# classification trees can overfit the dataset. Random forest are less
# likely to overfit.
predict(model3, test2)

#Making a data frame from the predictions and writing as csv
solution <- data.frame(PassengerID = test$PassengerId, Survived = predict(model3, test2))
write.csv(solution, file = 'submission.csv', row.names = F)
