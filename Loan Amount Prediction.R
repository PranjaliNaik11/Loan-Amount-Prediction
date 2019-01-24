DataBase<-read.csv("Loan__Dataset.csv", sep=",")
hist(DataBase$loan_amnt)
str(DataBase)

family <- as.factor(DataBase[,4])
plot(DataBase$loan_amnt,DataBase$annual_inc,pch=16,col=family
    [unclass(DataBase$grade)],main="loan v/s pay",
    xlab="loan",
    ylab="pay")
abline(lm(loan_amnt~annual_inc,data=DataBase)$coefficients,col="black")

OurDat <- DataBase[,c(2,6,9,10)]
OurModel1 <- rpart::rpart(loan_amnt ~., data=OurDat, method="anova")
OurModel1

OurPredictedData <- predict(OurModel1, OurDat)
OurPredictedData

MeanAbsErr <- function (actual, predicted) { mean(abs(actual - predicted)) }
MeanAbsErr(OurDat$loan_amnt, OurPredictedData)

try<-DataBase %>%
  select(grade,loan_amnt,annual_inc,installment) %>%
  group_by(grade) %>%
  summarise(loan_amnt=mean(loan_amnt),annual_inc=mean(annual_inc),installment=mean(installment))
try

m1<-mean(DataBase$loan_amnt);std1<-sqrt(var(DataBase$loan_amnt))
hist(DataBase$loan_amnt,prob=T,main="Loan Amt")
curve(dnorm(x,mean=m1,sd=std1),col="darkblue",lwd=2,add=TRUE)

pie(table(DataBase$grade))
pie(table(DataBase$purpose))
library(ggplot2)
df.try<-data.frame(c_var=DataBase$grade,n_var=DataBase$loan_amnt)
ggplot2::ggplot(data=df.try,aes(x=c_var,y=n_var))+geom_bar(stat="identity")

x=DataBase[,c(2,3,4,5,9,11)]
set.seed(100)
trainingRowIndex<-sample(1:nrow(x),0.8*nrow(x))
trainingData<-x[trainingRowIndex,]
testData<-x[-trainingRowIndex,]
lmMod<-lm(loan_amnt~.,data=x);lmMod
distPred<-predict(lmMod,testData)
summary(lmMod)
