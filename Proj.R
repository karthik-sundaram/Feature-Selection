
setwd("C:/Users/Karthik/Desktop/Sem 1/ISEN 613/Project")
proj=read.table("secom.data",header=F,na.strings="?")
proj_label=read.table("secom_labels.data",header=F,na.strings="?")



proj["time_stamp"]=proj_label["V2"]
proj["Result"]=proj_label["V1"]

summary(proj)
str(proj1, list.len=ncol(proj))
?barplot
barplot(colSums(is.na(proj)),ylab="number of missing values",xlab="column Index")

proj=proj[,colSums(is.na(proj))<80]
barplot(colSums(is.na(proj)),ylab="number of missing values",xlab="observation/row Index")
plot(rowSums(is.na(proj)))

proj=proj[rowSums(is.na(proj))<30,]


sum(colSums(is.na(proj)))

#for (i in 1:ncol(proj)) 
#{
#  proj[,i][is.na(proj[,i])] <- mean(proj[,i], na.rm = TRUE) 
#}  

str(proj)

#getmode <- function(v) {
#  uniqv <- unique(v)
#  uniqv[which.max(tabulate(match(v, uniqv)))]
#}
#getmode(proj[,75])



#proj <- sapply( proj, as.numeric )


  temp=proj[,c(539,540)]
{
library(missForest)
proj<-missForest(proj[,-c(539,540)])

summary(proj$ximp)


  

}
proj<-proj$ximp
proj["time_stamp"]=temp["time_stamp"]
proj["Result"]=temp["Result"]
sum(colSums(is.na(proj)))
#{
  ####
#  for (i in 1:538) 
#  {
#    if(min(proj[,i])==max(proj[,i]))
#    { proj=proj[,-i]
#    }
#  }  
#  ###
#}

proj1=read.csv("proj1.csv")
proj1=proj1[,-1]
proj1=proj1[,-c(539)] #remove date stamp

#run multiple times till 423 cols.
for(i in 1:ncol(proj1))
{
  if(min(proj1[,i])==max(proj1[,i]))
  {
    proj1=proj1[,-i]
  }
}


str(proj1,list.len=ncol(proj1))

for (i in 1:1521)
{
  if(proj1[i,423]==-1)
{
  proj1[i,423]=0
}
}
proj1$Result=as.factor(proj1$Result)

fix(proj1)

sum(colSums(is.na(proj1)))

str(proj1)
library(caret)
df = cor(proj1[,-423])
hc = findCorrelation(df, cutoff=0.8) # putt any value as a "cutoff" 
hc = sort(hc)
RData = proj1[,-c(hc)]

set.seed(1)
train=sample(1:nrow(proj1),nrow(proj1)/2)
proj1.test=RData[-train,]
result.test=RData$Result[-train]

#LOGREG [final]
log_fit=glm(Result~V60   +V406 +V22   +V120+V65   +V576 +V76  +V104 +V46   +V27   +V424 +V78  +V575 +V490 ,data=RData,subset=train,family=binomial)
summary(log_fit)
proj1.test=RData[-train,]
result.test=RData$Result[-train]
tree.pred=predict(log_fit,proj1.test,type="response")

log1.pred=rep(-1,length(tree.pred))
log1.pred[tree.pred>0.5]=1

table(log1.pred,result.test)
mean(log1.pred==result.test)

#write.csv(proj1,file="proj2.csv")

#logreg testing
log_fit=glm(Result~V60+V344+V22+V76+V104,data=proj1,subset=train,family=binomial)
summary(log_fit)
proj1.test=proj1[-train,]
result.test=proj1$Result[-train]
tree.pred=predict(log_fit,proj1.test)

log1.pred=rep(-1,length(tree.pred))
log1.pred[tree.pred>0.5]=1

table(log1.pred,result.test)
mean(log1.pred==result.test)

fix(proj1)

#logreg testing
log_fit=glm(Result~ V60 +V65    +V26    +V478  +V76     +V501+V206 +V103  +V100 +V27      ,data=proj1,subset=train,family=binomial)
summary(log_fit)
proj1.test=proj1[-train,]
result.test=proj1$Result[-train]
tree.pred=predict(log_fit,proj1.test)

log1.pred=rep(0,length(tree.pred))
log1.pred[tree.pred>0.5]=1

table(log1.pred,result.test)
mean(log1.pred==result.test)

fix(proj1)

#logreg testing
log_fit=glm(Result~ V60 +V22 +V76  +V104  ,data=RData,subset=train,family=binomial)
summary(log_fit)
#proj1.test=RData[-train,]
#result.test=rda$Result[-train]
tree.pred=predict(log_fit,proj1.test)

log1.pred=rep(-1,length(tree.pred))
log1.pred[tree.pred>0.5]=1

table(log1.pred,result.test)
mean(log1.pred==result.test)


#LDA

library(MASS)
lda.fit=lda(Result~V26+V27+V65+V60+V122+V349+V153+V578+V105+V288+V478,data=proj1,subset=train,family=binomial)
summary(lda.fit)
proj1.test=proj1[-train,]
result.test=proj1$Result[-train]

log1.prob=predict(lda.fit,proj1.test,type="response")
lda1.class=log1.prob$class
table(lda1.class,result.test)
mean(lda1.class==result.test)


#TREES

library(tree)
set.seed(1)
train=sample(1:nrow(proj1),nrow(proj1)/2)
proj1.test=RData[-train,]
result.test=RData$Result[-train]
tree.proj1=tree(Result~ .,RData,subset=train)
tree.pred=predict(tree.proj1,proj1.test,type="class")
table(tree.pred,result.test)
plot(tree.proj1)
text(tree.proj1,pretty=0)
mean(tree.pred==result.test)
summary(tree.proj1)
#86.46 % classification accuracy obtained

#pruning

set.seed(1)
tree.proj1=tree(Result~. ,RData,subset=train)
plot(tree.proj1)
text(tree.proj1,pretty=0)
cv.proj=cv.tree(tree.proj1,FUN=prune.misclass)
cv.proj$size
plot(cv.proj$size,cv.proj$dev)
tree.min=which.min(cv.proj$dev)
tree.min
points(tree.min, cv.proj$dev[tree.min],col="red", cex = 2, pch = 20)
best1=rep(0,5)
a=c(9,14,17,21,23)
for (i in 1:5) 
{
prune.proj=prune.misclass(tree.proj1,best=a[i])
plot(prune.proj)
text(prune.proj,pretty=0)

tree.proj.pred=predict(prune.proj,proj1.test,type="class")
table(tree.proj.pred,result.test)
plot(prune.proj)
text(prune.proj,pretty=0)
best1[i]=mean(tree.proj.pred==result.test)
}

best1

{plot(prune)
prune.proj=prune.misclass(tree.proj1,best=7)
plot(prune.proj)
text(prune.proj,pretty=0)

tree.proj.pred=predict(prune.proj,proj1.test,type="class")
table(tree.proj.pred,result.test)
plot(prune.proj)
text(prune.proj,pretty=0)
mean(tree.proj.pred==result.test)
summary(prune.proj)}
#fix(proj)

#pruning testing
set.seed(1)
tree.proj1=tree(Result~V60  +V406 +V120 +V332 +V65   +V22   +V46   +V104 +V26   +V66   +V490 +V349 +V501 +V125 +V100 +V435 +V27  +V291 +V342 +V157 +V511 +V353 +V105 +V101 +V432 ,proj1,subset=train)
plot(tree.proj1)
text(tree.proj1,pretty=0)
cv.proj=cv.tree(tree.proj1,FUN=prune.misclass)
cv.proj$size
plot(cv.proj$size,cv.proj$dev)
tree.min=which.min(cv.proj$dev)
tree.min
points(tree.min, cv.proj$dev[tree.min],col="red", cex = 2, pch = 20)
best1=rep(0,5)
a=c(9,14,17,21,23)
for (i in 1:5) 
{
  prune.proj=prune.misclass(tree.proj1,best=a[i])
  plot(prune.proj)
  text(prune.proj,pretty=0)
  
  tree.proj.pred=predict(prune.proj,proj1.test,type="class")
  table(tree.proj.pred,result.test)
  plot(prune.proj)
  text(prune.proj,pretty=0)
  best1[i]=mean(tree.proj.pred==result.test)
}

best1

{plot(prune)
  prune.proj=prune.misclass(tree.proj1,best=7)
  plot(prune.proj)
  text(prune.proj,pretty=0)
  
  tree.proj.pred=predict(prune.proj,proj1.test,type="class")
  table(tree.proj.pred,result.test)
  plot(prune.proj)
  text(prune.proj,pretty=0)
  mean(tree.proj.pred==result.test)
}
#fix(proj)


#bagging
install.packages("randomForest")
library(randomForest)
library(MASS)
best_bag=rep(0,422)
for(i in 1:422)
{
set.seed(1)
bag.car=randomForest(Result~V60  +V406 +V120 +V332 +V65   +V22   +V46   +V104 +V26   +V66   +V490 +V349 +V501 +V125 +V100 +V435 +V27  +V291 +V342 +V157 +V511 +V353 +V105 +V101 +V432,data=proj1,subset=train,mtry= 5,ntree=100,importance=TRUE)
bag.car
bag.car.pred=predict(bag.car,newdata=proj1[-train,])
table(bag.car.pred,result.test)
best_bag[i]=mean(bag.car.pred==result.test)

}
plot(best_bag)
best_bag[which.max(best_bag)] #best is at mtry=11
points(11,best_bag[11],col="red",cex=2,pch=20)


set.seed(1)
bag.car=randomForest(Result~.,data=proj1,subset=train,mtry= 11,ntree=100,importance=TRUE)
bag.car.pred=predict(bag.car,newdata=proj1[-train,])
table(bag.car.pred,result.test)
mean(bag.car.pred==result.test)


new_dat=proj1[,c("V26","V27","V65","V268","V120","V41","V60","V540","V22","V122","V349","V66","V148","V406","V153","V31","V578","V104","V353","V52","V240","V427","V105","V117","V437","V171","V215","V461","V288","V478","Result")]
{
  set.seed(1)
  train=sample(1:nrow(new_dat),nrow(new_dat)/2)
  proj1.test=new_dat[-train,]
  result.test=new_dat$Result[-train]
  set.seed(1)
  
  bag.car=randomForest(Result~.,data=new_dat,subset=train,mtry= 20,ntree=100,importance=TRUE)
  bag.car
  bag.car.pred=predict(bag.car,newdata=proj1.test)
   table(bag.car.pred,result.test)
  mean(bag.car.pred==result.test)
  
}
fix(proj1)
fix(new_dat)
varImpPlot(bag.car)

library(caret)
varImp(bag.car)
varImpPlot(bag.car,type=2)

plot(best_bag,xlab="ntree",ylab="Test error")

plot(varImp(bag.car), top = 10)
importance(bag.car)


#boosting

library(gbm)
set.seed(1)
boost.boston=gbm(unclass(Result)-1~V60   +V406 +V22   +V120+V65   +V576 +V76  +V104 +V46   +V27   +V424 +V78  +V575 +V490 ,data=RData[train,],distribution="bernoulli",n.trees=500,interaction.depth =4)

summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston)
plot(boost.boston)
pred.boost=predict(boost.boston,RData[-train,],n.trees=500,type="response")

log1.pred=rep(-1,length(pred.boost))
log1.pred[pred.boost>0.5]=1

table(log1.pred,result.test)
mean(log1.pred==result.test)

str(log1.pred)
contrasts(log1.pred)
#install.packages("xlsx")
#library(xlsx)
#write.xlsx(proj, file="proj1.xlsx", sheetName="processed_data", row.names=FALSE)
write.csv(proj,file="proj1.csv")





#ROC curve

#log1.fit=glm(count01~.,data=Bike_data,family=binomial,subset=train)
#log1.pred=predict(log1.fit,bike_test,type="response")
#contrasts(result.test)
roc.curve=function(s,print=FALSE)
{
  Ps=(pred.boost>s)*1
  FP=sum((Ps==1)*(result.test=="-1"))/sum(RData$Result=="-1")
  TP=sum((Ps==1)*(result.test=="1"))/sum(RData$Result=="1")
  if(print==TRUE){
    print(table(Observed=result.test,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.5
roc.curve(threshold,print=TRUE)


ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=0.01))
plot(M.ROC[1,],M.ROC[2,],col="grey",lwd=2,type="l",xlab="Falsepositive Rate",ylab="True positive rate")



AUC(result.test,pred.boost)




library("caret")



ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     savePredictions = TRUE)

mod_fit <- train(Result~ V60   +V406 +V22   +V120+V65   +V576 +V76  +V104 +V46   +V27   +V424 +V78  +V575 +V490  , data=proj1[train,], 
                 method="glm", 
                 family="binomial",
                 trControl = ctrl, 
                 tuneLength = 5)
plot(varImp(mod_fit))
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Result~ V60   +V406 +V22   +V120+V65   +V576 +V76  +V104 +V46   +V27   +V424 +V78  +V575 +V490  , data=RData, method="lvq", trControl=control)

# Check out Variable Importance
varImp(model)
plot(varImp(model))
summary(mod_fit)
pred = predict(mod_fit, newdata=proj1[-train,])
accuracy <- table(pred, result.test)
sum(diag(accuracy))/sum(accuracy)









set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(proj1[,1:422], proj1[,423], sizes=c(1:422), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)


#LDA
#lda1.fit=lda(Result~Lag2+I(Lag2^2)+I(Lag2^3),Data=Weekly,subset=train)
#summary(lda1.fit)
#lda1.prob=predict(lda1.fit,Week.200910)


#lda1.class=lda1.prob$class

#table(lda1.class,Direction.200910)
#mean(lda1.class==Direction.200910)


#PCA
#for (i in 1:25)
#{
#a=dat614[,i] - mean(dat614[,i])
#  dat_614[,i]=(dat_614[,i] - mean(dat_614[,i])/sd(dat_614[,i]))
#}
#dat614=dat614[,-1]
pr.out=prcomp(proj1[,],scale=TRUE)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve,type='b')
plot(cumsum(pve),ylim=c(0,1),type='b')




pr.out=prcomp(new_dat[,-31],scale=TRUE)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve,type='b')
plot(cumsum(pve),ylim=c(0,1),type='b')


library(leaps)
regfit.full=regsubsets(Result~V60 +V406  +V22   +V120  +V104  +V65    +V332 +V574 +V26    +V478  +V349  +V46    +V435+V66    +V334 +V342+V76    +V78   +V501+V362+V490 +V125 +V206 +V291 +V103 +V578 +V583   +V511 +V100 +V169+V52   +V575 +V134 +V300   +V27  +V356 +V299 +V430 ,proj1,nvmax=20)
summary(regfit.full)



?regsubsets




library(MASS)

lda.fit=qda(Result~V60  +V406 +V120 +V332 +V65   +V22   +V46   +V104 +V26   +V66   +V490 +V349 +V501 +V125 +V100 +V435 +V27  +V291 +V342 +V157 +V511 +V353 +V105 +V101 +V432,data=proj1,family=binomial,subset=train)
summary(lda.fit)
log1.prob=predict(lda.fit,proj1[-train,],type="response")
lda1.class=log1.prob$class
table(lda1.class,result.test)
mean(lda1.class==result.test)

#SVM
library(e1071)
tune.out=tune(svm,Result~.,data=proj1[train,],kernel="linear",ranges=list(cost=c(0.01,0.1,1,5,10,100)),scale=FALSE)
summary(tune.out)#error min=cost(1)
svm.fit=svm(count01~.,data=Bike_data,subset=train,kernel="linear",cost=10,scale=FALSE)
summary(svm.fit)
pred=predict(svm.fit,newdata=Bike_data[-train,])

table(pred,count_resp)
mean(pred==count_resp)
