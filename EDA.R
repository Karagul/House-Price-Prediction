library(ggplot2)
library(corrplot)
library(Metrics)
data=read.csv('housing.csv')
head(data)
nrow(data)
ncol(data)
names(data)
'%!in%' <- function(x,y)!('%in%'(x,y))
unique(data$Street)
data$Id<-NULL
data$Alley
data=data[!is.na(data$"SalePrice"),]
nrow(data)
max(data$"SalePrice")
#histogram of sale price
ggplot(data, aes(x=SalePrice)) +
        geom_histogram( binwidth = 10000) +
        scale_x_continuous(breaks= seq(0, 800000, by=100000))
#finding features that are numeric
numeric_feature<-names(which(sapply(data, is.numeric)))
data_numeric<-data[numeric_feature]
#finding correlations between numeric features
correlations_everything <- cor(data_numeric, use="pairwise.complete.obs")
corrplot(correlations_everything, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
cor_with_sale<-correlations_everything['SalePrice',]
data_area<-data[c("MSSubClass","LotArea","MasVnrArea",
       "BsmtFinSF1","BsmtUnfSF","TotalBsmtSF",
       "X1stFlrSF","X2ndFlrSF","GrLivArea","WoodDeckSF",
       "OpenPorchSF","EnclosedPorch","ScreenPorch",
       "PoolArea","MiscVal","SalePrice")]
real_numeric<-c("MSSubClass","LotArea","MasVnrArea",
                "BsmtFinSF1","BsmtUnfSF","TotalBsmtSF",
                "X1stFlrSF","X2ndFlrSF","GrLivArea","WoodDeckSF",
                "OpenPorchSF","EnclosedPorch","ScreenPorch",
                "PoolArea","MiscVal","SalePrice")
cor_area<-cor(data_area,use="pairwise.complete.obs")
corrplot(cor_area, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
#Total basement area and sale price plot
ggplot(data,aes(x=TotalBsmtSF,y=SalePrice))+geom_point(col='red')+geom_smooth()
data_plot_basement=data[!is.na(data$TotalBsmtSF),]
ggplot(data_plot_basement,aes(x=TotalBsmtSF,y=SalePrice))+geom_point(col='red')+geom_smooth(method=lm)
#above ground square feet and sale price plot 
pic1<-ggplot(data,aes(x=GrLivArea,y=SalePrice))+geom_point(col='red')+geom_smooth(method=lm)
#Masonry veneer area and sale price plot
pic2<-ggplot(data,aes(x=MasVnrArea,y=SalePrice))+geom_point(col='red')+geom_smooth(method=lm)
#Check remaining variables
vector=c()
for (name in names(data_numeric)){
if (name %!in% real_numeric){
vector[name]<-name
    }
 }
rating_data<-data[vector]
rating_data$GarageYrBlt<-NULL
rating_data$X3SsnPorch<-NULL
rating_data$LotFrontage<-NULL
x<-rating_data[c("OverallQual","OverallCond")]
x$SalePrice<-data$SalePrice
#grid.arrange(a, b,c,d ,nrow = 2)
a<-ggplot(x,aes(x=OverallQual,y=SalePrice))+geom_point(col='black')+geom_smooth()

b<-ggplot(x,aes(x=factor(OverallCond),y=SalePrice))+geom_boxplot()+ labs(x='Overall Condition')
c<-ggplot(x,aes(x=factor(OverallQual),y=SalePrice))+geom_boxplot()+labs(x='Overall Quality')
d<-ggplot(data,aes(x=YearBuilt,y=SalePrice))+geom_point(col='black')+geom_smooth(col='red')
#grid.arrange(a, b,c,d ,nrow = 2)

training<-data[real_numeric]

        #Now, work with some binary categorical variables
training$Street<-ifelse(data$Street=='Pave',1,0)
training$Alley<-ifelse(!(is.na(data$Alley)),1,0)
training$MSZoning<-ifelse(data$MSZoning=='RL',1,0)
training$LotFrontage<-data$LotFrontage
training$LandContour<-ifelse(data$LandContour=='Lvl',1,0)
training$LotShape<-ifelse(data$LotShape=='Reg',1,0)
training$Utilities<-ifelse(data$Utilities=='AllPub',1,0)
training$LotConfig<-ifelse(data$LotConfig=='Inside',1,0)
training$LandSlope<-ifelse(data$LandSlope=='Gtl',1,0)
training$Condition<-ifelse(data$Condition1=='Norm',1,0)
training$Quality<-data$Quality
training$Condition2<-data$OverallCond
old_new<-function(year){
        if (year>1980){
                return (2)
        }else if ((1930<=year) & (year<=1980)){
                return (1)
        }else if (year<=1930){
                return (0)
        }
}
training$Yearbuild<-sapply(data$YearBuilt,old_new)
training$YearRemod<-ifelse(data$YearRemod>=1989,1,0)
library(dplyr)

neigh<-group_by(data,Neighborhood) %>%
        summarise((median(SalePrice,na.rm=T)))

names(neigh)<-c('neighborhood','med')
neigh<-as.data.frame(neigh)
neigh<-neigh[order(-neigh$med),]
neigh$neighborhood[2]
price<-function(x){
        if (x %in% neigh$neighborhood[1:5]){
                return (4)
        }else if (x %in% neigh$neighborhood[6:10]){
                return (3)
        }else if (x %in% neigh$neighborhood[11:15]){
                return (2)
        }else if (x %in% neigh$neighborhood[16:20]){
                return (1)
        }else return (0)
}
training$neighborhood<-sapply(data$Neighborhood,price)

style<-group_by(data,HouseStyle) %>%
        summarise((median(SalePrice,na.rm=T)))
names(style)<-c('Style','med')
style<-as.data.frame(style)
style<-style[order(-style$med),]
style_price<-function(x){
        if(x %in% style$Style[1:4]){
                return (1)
        }else return (0)
}
training$style<-sapply(data$HouseStyle,style_price)
training$SaleCond<-ifelse(data$SaleCondition=='Normal',1,0)

#imput missing data
library(mice)
md.pattern(training)
temp_data<-mice(data=training,m=5,maxit=50,meth='pmm',seed=500)
training <- complete(temp_data,1)
#Finalize dataset
data<-training[,c(1:15,17:31,16)]
data<-data[,c((1:29),31,30)]

write.csv(data,file='house data.csv')
#train test split
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#Started fiting the data to the models

library(randomForest)
model2.rf<-randomForest(SalePrice~.,data=train)
price2.rf = predict(model2.rf, test)
result2<-cbind(test,price2.rf)
result2$SalePrice<-log(result2$SalePrice)
result2$price2.rf<-log(result2$price2.rf)
rmse(result2$SalePrice,result2$price2.rf)
#The rmse of random forest is 0.159, which is excellent
#LASSO
library(glmnet)
data<-data[,c((1:29),31,30)]
train<-train[,c((1:29),31,30)]
test<-test[,c((1:29),31,30)]
cv.out <- cv.glmnet(data.matrix(train[1:30]), data.matrix(train[31]), alpha = 0)
bestlam <- cv.out$lambda.min
bestlam
lasso.mod<-glmnet(data.matrix(train[1:30]), data.matrix(train[31]), alpha = 0)
price3.lasso <- predict(lasso.mod, s = bestlam, newx = data.matrix(test[1:30]))
result3<-cbind(test,price3.lasso)
result3$SalePrice<-log(result3$SalePrice)
result3$'1'<-log(result3$'1')
rmse(result3$SalePrice,result3$'1')
#The rmse of LASSO is 0.191, which is also very good

#Subset selection for other models 
step(lm(SalePrice~.,data=data),direction='backward')
df<-data[c('MSSubClass','LotArea','MasVnrArea','BsmtFinSF1','TotalBsmtSF','X1stFlrSF',
  'X2ndFlrSF','GrLivArea','WoodDeckSF','OpenPorchSF','EnclosedPorch',
  'ScreenPorch','LotFrontage','Utilities','Condition','Condition2',
  'Yearbuild','YearRemod','neighborhood','SalePrice')]

#SVM
library(rpart)
library(e1071)
#scaling the data
maxValue<-apply(df,2,max)
minValue<-apply(df,2,min)
df<-as.data.frame(scale(df,center=minValue,scale=maxValue-minValue))
#train test split
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]
model1.svm <- svm(SalePrice ~ ., data = train,cost=3, scale = F)
price1.svm = predict(model1.svm, test)
#Unscale the data for intepretation
price1.svm<-price1.svm*(755000-minValue['SalePrice'])+minValue['SalePrice']
result1<-cbind(values,price1.svm)
result1<-as.data.frame(result1)
result1$values<-log(result1$values)
result1$price1.svm<-log(result1$price1.svm)
rmse(result1$values,result1$price1.svm)
#The rmse is 0.2446, which is the worst among all models

Models<-c('Random Forest','LASSO','SVM','Perceptron Neural Net')
Result<-c(0.159,0.191,0.2466,0.1833)
Accuracy<-cbind(Models,Result)
Accuracy<-as.data.frame(Accuracy)
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))

#Visualization scripts
#ggplot(Accuracy, aes(x=Models, 
#                     y=Result))+ geom_bar(stat='summary',
#                                                    fun.y = "median",
#                                                    fill='#56B4E9')+ggtitle(
#                                                            'Model Accuracy Summary')+
#       ylab('MSE of log prices')
#
#ggplot(test,aes(x=ID))+geom_line(aes(y=SalePrice,colour='dark blue')
#                                 )+geom_line(aes(y=predicted,colour='Predicted',
#                                                 ))+scale_color_discrete(name = "Groups", labels = c("Predicted", "Real Price"))
###
