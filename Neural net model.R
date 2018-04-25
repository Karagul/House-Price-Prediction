library(neuralnet)
library(Metrics)
#subset selection
step(lm(SalePrice~.,data=data),direction='backward')
data=read.csv('house data.csv')
df<-data[c('MSSubClass','LotArea','MasVnrArea','BsmtFinSF1','TotalBsmtSF','X1stFlrSF',
           'X2ndFlrSF','GrLivArea','WoodDeckSF','OpenPorchSF','EnclosedPorch',
           'ScreenPorch','LotFrontage','Utilities','Condition','Condition2',
           'Yearbuild','YearRemod','neighborhood','SalePrice')]

#Feedforward neuralnet
maxValue<-apply(df,2,max)
minValue<-apply(df,2,min)
df<-as.data.frame(scale(df,center=minValue,scale=maxValue-minValue))
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
net.model <- neuralnet(SalePrice~MSSubClass+LotArea+MasVnrArea+
                               BsmtFinSF1+TotalBsmtSF+X1stFlrSF
                       +X2ndFlrSF+GrLivArea+WoodDeckSF+
                               OpenPorchSF+EnclosedPorch
                       +ScreenPorch+LotFrontage+Utilities+Condition
                       +Condition2+Yearbuild+YearRemod+neighborhood
                       ,train, hidden=c(5,5,3),stepmax = 10e6)
plot(net.model)
predictions <- compute(net.model, test[-20])
predictions_full<- predictions$net.result*(755000-minValue['SalePrice'])+minValue['SalePrice']
values<-(test$SalePrice)*(755000-minValue['SalePrice'])+minValue['SalePrice']
result4<-cbind(predictions_full,values)
result4<-as.data.frame(result4)
result4$'V1'<-log(result4$'V1')
result4$'values'<-log(result4$'values')
rmse(result4$'values',result4$'V1')
#The rmse of log price is 0.1833, which is very good