setwd("E://project")
getwd()

data <- read.csv("Churn.csv")
summary(data)
table(data$Churn)
set.seed(1)
index<-sample(1:nrow(data),0.8*nrow(data))
index
train_data<-data[index,-21]
str(train_data)
test_data<-data[-index,c(-8,-21)]
str(test_data)
glm_model<-glm(Churn~.,family=binomial,data=train_data)
glm_model
summary(glm_model)
pred <- predict(glm_model,test_data,type="response")
outcome <- floor(pred+0.5)
table(outcome)
ttt<- table(data$Churn[-index],outcome)
ttt

# model2 removing state#
glm_model1<-glm(Churn~. -State ,family=binomial,data=train_data)
glm_model1
summary(glm_model1)
pred1 <- predict(glm_model1,test_data,type="response")
outcome1 <- floor(pred1+0.5)
table(outcome1)
ttt1<- table(data$Churn[-index],outcome1)
ttt1
exp(cbind(Odds_and_OR=coef(glm_model1), confint(glm_model1)))


#to identify potential variables
x1 <- step(glm_model2)
summary(x1)
formula(x1)
glm_model3<- glm(Churn ~Eve.Mins + CustServ.Calls + Int.l.Plan + VMail.Plan + Day.Charge + Intl.Calls + Intl.Charge,data = train_data)
exp(glm_model3$coefficients)
pred3 <- predict(glm_model3,test_data,type="response")
plot(data$Churn[-index]~pred3)
outcome3 <- floor(pred3+0.5)
ttt3=table(data$Churn[-index],outcome3)
ttt3
exp(cbind(Odds_and_OR=coef(glm_model3), confint(glm_model3)))

# model4 removing imbalences#
data_1<-data[data$Churn==1,]
ind_1<-sample(rownames(data_1),483) 
data_0<-data[data$Churn==0,]
ind_0<-sample(rownames(data_0),483)
train_data1<-data[c(ind_1,ind_0), -21 ]
table(train_data1$Churn)
glm_model4<-glm(Churn ~Eve.Mins + CustServ.Calls + Int.l.Plan + VMail.Plan + Day.Charge + Intl.Calls + Intl.Charge ,family=binomial,data=train_data1)
glm_model4
summary(glm_model4)
pred4 <- predict(glm_model4,test_data,type="response")
outcome4 <- floor(pred4+0.5)
table(outcome4)
ttt4<- table(data$Churn[-index],outcome4)
ttt4
exp(cbind(Odds_and_OR=coef(glm_model4),confint(glm_model4)))


### accuracy measures##
acc<-(ttt4[1]+ttt4[4])/(ttt4[1]+ttt4[3]+ttt4[2]+ttt4[4])
acc         
sens=ttt4[1]/(ttt4[1]+ttt4[3])
sens
spec=ttt4[4]/(ttt4[4]+ttt4[2])
spec
install.packages("caret")
library(caret)
confusionMatrix(ttt4) 

## Performance Tradeoffs ##
install.packages("ROCR")
library(ROCR)
pred5<-predict(glm_model4,train_data1,type = "response")
pred6<-prediction(predictions = pred5,labels = train_data1$Churn )
eval<- performance(pred6,measure = "tpr", x.measure = "fpr")
plot(eval,colorize=T, main="ROC curve",col="blue",lwd=5) 
auc<-performance(pred6,measure = "auc") 
str(auc) 
as.numeric(auc@y.values)

## tableau integration##
library(Rserve)
Rserve()
