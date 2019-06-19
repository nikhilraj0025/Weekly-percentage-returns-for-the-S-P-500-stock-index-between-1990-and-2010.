library(ISLR)
data("Weekly")
View(Weekly)
summary(Weekly)
?Weekly
dim(Weekly)
Weekly1<-Weekly
plot(Weekly[,-9])#########correlation for x#######################################
attach(Weekly1)
plot(Volume)##########Volume is increasing wrt time#################################
library(dplyr)
Weekly1<-mutate(Weekly1,Direction=ifelse(Direction=="Up",1,0))
Direction<-factor(Direction)
#########################sampling################################################
W_sam<-sample(2,nrow(Weekly1),replace=TRUE,prob=c(.75,.25))
W_Train<-Weekly1[W_sam==1,]
W_Test<-Weekly1[W_sam==2,]
#################################Model Building##################################
###########on Train and Test on Test data##############################

W_mod1<-glm(Direction~.,data=W_Train,family=binomial)
summary(W_mod1)#############pr values are very high so we cant say that there is a real association between x&y
W_pred1<-predict(W_mod1,W_Test,type="response")
W_df1<-data.frame(W_pred1,W_Test$Direction)
W_df1<-mutate(W_df1,W_pred1=ifelse(W_pred1>0.5,1,0))
colnames(W_df1)<-c("predict","actual")
t1<-table(W_df1$predict,W_df1$actual)
t1
acc1<-sum(diag(t1))/sum(t1)
acc1



###############on complete data set##############################################
W_mod2<-glm(Direction~.,data=Weekly1,family=binomial)
summary(W_mod2)#############pr values are very high so we cant say that there is a real association between x&y
W_pred2<-predict(W_mod2,Weekly1,type="response")
W_df2<-data.frame(W_pred2,Weekly1$Direction)
W_df2<-mutate(W_df2,W_pred2=ifelse(W_pred2>0.5,1,0))
colnames(W_df2)<-c("predict","actual")
t2<-table(W_df2$predict,W_df2$actual)
t2
acc2<-sum(diag(t2))/sum(t2)
acc2


###Now fit the logistic regression model using a training data period
#from 1990 to 2008, with Lag2 as the only predictor. Compute the
#confusion matrix and the overall fraction of correct predictions
#for the held out data (that is, the data from 2009 and 2010).

Train_new<-filter(Weekly1,Year<=2008)
View(Train_new)
Test_new<-filter(Weekly1,Year>=2009)
View(Test_new)


W_mod3<-glm(Direction~Lag2,data=Train_new,family=binomial)
W_pred3<-predict(W_mod3,Test_new,type="response")
W_df3<-data.frame(W_pred3,Test_new$Direction)
W_df3<-mutate(W_df3,W_pred3=ifelse(W_pred3>0.5,1,0))
colnames(W_df3)<-c("predict","actual")
t3<-table(W_df3$predict,W_df3$actual)
t3
acc3<-sum(diag(t3))/sum(t3)
acc3





#####################################################################################
data("Auto")
View(Auto)
dim(Auto)
##########################scatter plot############################
plot(Auto)
#################computation of correlation matrix using function########
Auto1<-Auto[,-9]
cor(Auto1)

##########sampling######################################
W_Au<-sample(2,nrow(Auto1),replace=TRUE,prob=c(.75,.25))
A_Train<-Auto1[W_Au==1,]
A_Test<-Auto1[W_Au==2,]
########model building on linear regression############################
aumodel_1<-lm(mpg~.,data=A_Train)
summary(aumodel_1)
A_pred1<-predict(aumodel_1,A_Test)
df_A<-data.frame(A_pred1,A_Test$mpg)
df_A1<-mutate(df_A,error=(A_pred1-A_Test$mpg)^2)
RMSE_A<-sqrt(mean(df_A1$error))
RMSE_A

#####on complete data#####################################################
aumodel_2<-lm(mpg~.,data=A_Train)
par(mfrow=c(2,2))
plot(aumodel_2)
summary(aumodel_2)
A_pred2<-predict(aumodel_1,Auto1)
df_A2<-data.frame(A_pred2,Auto1$mpg)
df_A2<-mutate(df_A2,error=(A_pred2-Auto1$mpg)^2)
RMSE_2<-sqrt(mean(df_A2$error))
RMSE_2

