library(pROC)
library("randomForest")
library(e1071) #for naiveBayes and svm

x1=rnorm(100)
x2=rnorm(100)
y=as.factor(rbinom(size=1,n=100,prob=exp(x1)/(1+exp(x1))))
data=data.frame(x1,x2,y)


####################1. logistic
model_lr=glm(y~.,data=data,binomial(link=logit))
#get the predicted probility
yhat_lr=predict(model_glm,x,type="response") 
#transform to 0-1 and compute accuracy
yhat=rep(0,length(y))
yhat[yhat_lr>0.5]=1
table(y,yhat)
sum(yhat_lr==y)/length(y)
#get AUC
auc(y,yhat_lr)
roc(y, yhat_lr, plot=TRUE, print.thres=TRUE, print.auc=TRUE)


####################2. SVM
model_svm <- svm(y ~ ., data = data,type='C-classification',probability = TRUE)
#get the predicted probility
svm_res=predict(model_svm,x,probability=TRUE)
yhat_svm=attr(svm_res,"probabilities")[,1]   #get the prob of 1
#get AUC
auc(y,yhat_svm)
#get the predicted 0-1
yhat_svm2=predict(model_svm,x)


####################3. naive Bayes
model_nb <- naiveBayes(y ~ ., data = data)
#get the predicted probility
nb_res=predict(model_nb,x,type="raw")
yhat_nb=nb_res[,2] #get the prob of 1
#get AUC
auc(y,yhat_nb)
#get the predicted 0-1
yhat_nb2=predict(model_nb,x,type="class")



####################4. randomForest
model_rf=randomForest(y ~.,data=data,importance=TRUE,ntrees=100)
#get the predicted probility
rf_res=predict(model_rf,x,type="prob")
yhat_rf=rf_res[,2] #get the prob of 1
#get AUC
auc(y,yhat_rf)
#get the predicted 0-1
yhat_rf2=predict(model_rf,x,type="response")



