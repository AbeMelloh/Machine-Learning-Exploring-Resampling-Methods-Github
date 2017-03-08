
#####################
#### QUESTION 1 ####
#####################

rm(list=ls())

set.seed(5072)
library(ISLR)
#attach(Weekly)
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family=binomial)
summary(glm.fit)
#Lag2 is the only significant predictor.
glm.probs<-predict(glm.fit,Weekly,type='response')
glm.pred<-ifelse(glm.probs>.5,'Up','Down')
mytable<-table(Weekly$Direction,glm.pred)
mytable

#The Overall fraction of the correct predictions is:
mean(glm.pred==Weekly$Direction)
print(paste('Error rate:',mean(glm.pred!=Weekly$Direction)))
#########
#Type 1 and Type 2 Errors Below Here
#########
(mytable["Down","Up"]/sum(mytable["Down",]))
(mytable["Up","Down"]/sum(mytable["Up",]))

#THE POWER OF THE MODEL BELOW HERE
print(paste('The power is',1-mytable['Up','Down']/sum(mytable['Up',])))

#The Precision of the Model goes here: 
precision<-mytable["Up","Up"]/sum(mytable[,"Up"])
precision
                                
#1e)
train<-Weekly$Year<2009
Weekly.2009<-Weekly[!train,]
Direction.2009<-Weekly$Direction[!train]

glm.fit<-glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)

#1f)
glm.probs<-predict(glm.fit,Weekly.2009,type="response")
glm.pred<-rep("Down",length(glm.probs)) 
glm.pred[glm.probs>.5]<-"Up"
mytesttable<-table(Direction.2009,glm.pred)
mytesttable
mean(glm.pred==Direction.2009)
print(paste('Error rate:',mean(glm.pred!=Direction.2009)))
(mytesttable["Down","Up"]/sum(mytesttable["Down",]))
(mytesttable["Up","Down"]/sum(mytesttable["Up",]))
print(paste('The power is',1-mytesttable['Up','Down']/sum(mytesttable['Up',])))
#The Precision of the Model goes here: 
precision<-mytesttable["Up","Up"]/sum(mytesttable[,"Up"])
precision

#1g)
library(MASS)
lda.fit<-lda(Direction~Lag2,data=Weekly,subset=train)
lda.pred<-predict(lda.fit, Weekly.2009)
mytableg<-table(Direction.2009,lda.pred$class)
mytableg
#mean(lda.pred==Direction.2009)  Delete this line.  Giving you some weird warning about not being a multiple
Correct<-(mytableg[1,1]+mytableg[2,2])/sum(mytableg)
Correct
#print(paste('Error rate:',mean(lda.pred!=Direction.2009))) Giving you some weird warning about not being a multiple
ErrorRate<-(mytableg["Up","Down"]+mytableg["Down","Up"])/sum(mytableg)
ErrorRate
(mytableg["Down","Up"]/sum(mytableg["Down",]))
(mytableg["Up","Down"]/sum(mytableg["Up",]))
print(paste('The power is',mytableg['Up','Down']/sum(mytableg['Up',])))
#The Precision of the Model goes here: 
precision<-mytableg["Up","Up"]/sum(mytableg[,"Up"])
precision

#1h)
qda.fit<-qda(Direction~Lag2,data=Weekly,subset=train)
qda.pred<-predict(qda.fit, Weekly.2009)
mytableh<-table(Direction.2009,qda.pred$class)
mytableh
Correct<-(mytableh[1,1]+mytableh[2,2])/sum(mytableh)
Correct
ErrorRate<-(mytableh["Up","Down"]+mytableh["Down","Up"])/sum(mytableh)
ErrorRate
(mytableh["Down","Up"]/sum(mytableh["Down",]))
(mytableh["Up","Down"]/sum(mytableh["Up",]))
print(paste('The power is',1-mytableh['Up','Down']/sum(mytableh['Up',])))
#The Precision of the Model goes here: 
precision<-mytableh["Up","Up"]/sum(mytableh[,"Up"])
precision

#1i)
require(class)         
train.X <- data.frame(Weekly$Lag2[train])
test.X <- data.frame(Weekly$Lag2[!train])
train.Direction.2009<-Weekly$Direction[train]
set.seed(5072)           
pred1.knn <- knn(train.X, test.X, train.Direction.2009, k = 1)
mytablei<-table(Direction.2009,pred1.knn)
mytablei
Correct<-(mytablei[1,1]+mytablei[2,2])/sum(mytablei)
Correct
ErrorRate<-(mytablei["Up","Down"]+mytablei["Down","Up"])/sum(mytablei)
ErrorRate
(mytablei["Down","Up"]/sum(mytablei["Down",]))
(mytablei["Up","Down"]/sum(mytablei["Up",]))
print(paste('The power is',1-mytablei['Up','Down']/sum(mytablei['Up',])))
#The Precision of the Model goes here: 
precision<-mytablei["Up","Up"]/sum(mytablei[,"Up"])
precision


#1j
pred2.knn <- knn(train.X, test.X, train.Direction.2009, k = 5)
mytablei<-table(Direction.2009,pred2.knn)
mytablei
Correct<-(mytablei[1,1]+mytablei[2,2])/sum(mytablei)
Correct
ErrorRate<-(mytablei["Up","Down"]+mytablei["Down","Up"])/sum(mytablei)
ErrorRate
(mytablei["Down","Up"]/sum(mytablei["Down",]))
(mytablei["Up","Down"]/sum(mytablei["Up",]))
print(paste('The power is',1-mytablei['Up','Down']/sum(mytablei['Up',])))
#The Precision of the Model goes here: 
precision<-mytablei["Up","Up"]/sum(mytablei[,"Up"])
precision
#1k

#The glm and the lda have the lowest error rate

#####################
#### QUESTION 2 ####
#####################

rm(list=ls())
require(ISLR)
set.seed(5072)
medmpg<-median(Auto$mpg)
mpg01 <- ifelse((Auto$mpg>medmpg),1,0)
Auto1 <- cbind(Auto, mpg01)
Auto1<-Auto1[,-1]

trainindices<-sample(nrow(Auto1),.8*nrow(Auto1))
testindices<-setdiff(1:nrow(Auto1),trainindices)
train.xvals<-Auto1[trainindices,]
test.xvals<-Auto1[-trainindices,]
train.yvals<-Auto1[trainindices,]
test.yvals<-Auto1[-trainindices,]

glm.fit <- glm(mpg01~cylinders+displacement+weight, data=Auto1, family=binomial, subset = trainindices)
glm.probs<-predict(glm.fit,test.yvals,type="response")
glm.pred<-rep("0",nrow(test.yvals))
glm.pred[glm.probs>.5]<-"1"
mytesttable<-table(test.yvals$mpg01,glm.pred)
mytesttable
Correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
Correct
ErrorRate<-(1-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable))
ErrorRate
(mytesttable[1,2]/sum(mytesttable[1,]))
(mytesttable[2,1]/sum(mytesttable[2,]))
print(paste('The power is',1-mytesttable[2,1]/sum(mytesttable[1,])))
#The Precision of the Model goes here: 
precision<-mytesttable[2,2]/sum(mytesttable[,2])
precision

#2f)
lda.fit <- lda(mpg01~cylinders+displacement+weight, data=Auto1, family=binomial, subset = trainindices)
lda.pred<-predict(lda.fit, test.yvals)
table<-table(Auto1$mpg01[testindices],lda.pred$class)
mytesttable
Correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
Correct
ErrorRate<-(mytesttable[1,2]+mytesttable[2,1])/sum(mytesttable)
ErrorRate
(mytesttable[1,2]/sum(mytesttable[1,]))
(mytesttable[2,1]/sum(mytesttable[2,]))
(1-(mytesttable[2,1]/sum(mytesttable[2,])))
#The Precision of the Model goes here: 
precision<-mytesttable[2,2]/sum(mytesttable[,2])
precision

#2g)
qda.fit <- qda(mpg01~cylinders+displacement+weight, data=Auto1, family=binomial, subset = trainindices)
qda.pred<-predict(qda.fit, test.yvals)
mytesttable<-table(test.yvals$mpg01,qda.pred$class)
mytesttable
Correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
Correct
ErrorRate<-(mytesttable[2,1]+mytesttable[1,2])/sum(mytesttable)
ErrorRate
(mytesttable[1,2]/sum(mytesttable[1,]))
(mytesttable[2,1]/sum(mytesttable[2,]))
print(paste('The power is',1-mytesttable[2,1]/sum(mytesttable[2,])))
#The Precision of the Model goes here: 
precision<-mytesttable[2,2]/sum(mytesttable[,2])
precision

#2hv1)
require(class)         
trainingmpg01<-Auto1$mpg01[trainindices]
standard.X<-scale(Auto1[c(1,2,4)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.pred3<-knn(train.x,test.x,trainingmpg01, k=1)
table<-table(Auto1$mpg01[testindices],knn.pred3)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct
ErrorRate<-(table["1","0"]+table["0","1"])/sum(table)
ErrorRate
(table["0","1"]/sum(table["0",]))
(table["1","0"]/sum(table["1",]))
print(paste('The power is',1-table['1','0']/sum(table['1',])))
#The Precision of the Model goes here: 
precision<-table[2,2]/sum(table[,2])
precision

#2i)
require(class)         

trainingmpg01<-Auto1$mpg01[trainindices]
standard.X<-scale(Auto1[c(1,2,4)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.pred4<-knn(train.x,test.x,trainingmpg01, k=5)
table<-table(Auto1$mpg01[testindices],knn.pred4)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct
ErrorRate<-(table["1","0"]+table["0","1"])/sum(table)
ErrorRate


require(class)         
trainingmpg01<-Auto1$mpg01[trainindices]
standard.X<-scale(Auto1[c(1,2,4)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.pred33<-knn(train.x,test.x,trainingmpg01, k=33)
table<-table(Auto1$mpg01[testindices],knn.pred33)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct
ErrorRate<-(table["1","0"]+table["0","1"])/sum(table)
ErrorRate


require(class)         
trainingmpg01<-Auto1$mpg01[trainindices]
standard.X<-scale(Auto1[c(1,2,4)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.pred84<-knn(train.x,test.x,trainingmpg01, k=84)
table<-table(Auto1$mpg01[testindices],knn.pred84)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct
ErrorRate<-(table["1","0"]+table["0","1"])/sum(table)
ErrorRate


#KNN where k=5 appears to produce the best results for the data as it has the lowest error rate. (0.08860759.)

#####################
#### QUESTION 3 ####
#####################
rm(list=ls())
require(MASS)
set.seed(5072)
medcrime<-median(Boston$crim)
crim01 <- ifelse((Boston$crim>medcrime),1,0)
Boston1 <- cbind(Boston, crim01)
Boston1<-Boston1[,-1]

trainindices<-sample(nrow(Boston1),.8*nrow(Boston1))
testindices<-setdiff(1:nrow(Boston1),trainindices)
train.xvals<-Boston1[trainindices,]
test.xvals<-Boston1[-trainindices,]
train.yvals<-Boston1[trainindices,]
test.yvals<-Boston1[-trainindices,]

glm.fit <- glm(crim01~nox+dis+rad, data=Boston1, family=binomial, subset = trainindices)
glm.probs<-predict(glm.fit,test.yvals,type="response")
glm.pred<-rep("0",nrow(test.yvals))
glm.pred[glm.probs>.5]<-"1"
mytesttable<-table(test.yvals$crim01,glm.pred)
mytesttable
Correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
Correct
ErrorRate<-(1-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable))
ErrorRate
(mytesttable[1,2]/sum(mytesttable[1,]))
(mytesttable[2,1]/sum(mytesttable[2,]))
print(paste('The power is',1-mytesttable[2,1]/sum(mytesttable[2,])))
#The Precision of the Model goes here: 
precision<-mytesttable[2,2]/sum(mytesttable[,2])
precision


lda.fit <- lda(crim01~nox+dis+rad, data=Boston1, family=binomial, subset = trainindices)
lda.probs<-predict(lda.fit,test.yvals,type="response")
lda.pred<-rep("0",nrow(test.yvals))
lda.pred[glm.probs>.5]<-"1"
mytesttable<-table(test.yvals$crim01,lda.pred)
mytesttable
Correct<-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable)
Correct
ErrorRate<-(1-(mytesttable[1,1]+mytesttable[2,2])/sum(mytesttable))
ErrorRate
(mytesttable[1,2]/sum(mytesttable[1,]))
(mytesttable[2,1]/sum(mytesttable[2,]))
print(paste('The power is',1-mytesttable[2,1]/sum(mytesttable[2,])))
#The Precision of the Model goes here: 
precision<-mytesttable[2,2]/sum(mytesttable[,2])
precision


require(class)         
trainingcrim01<-Boston1$crim01[trainindices]
standard.X<-scale(Boston1[c(4,7,8)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.predb1<-knn(train.x,test.x,trainingcrim01, k=1)
table<-table(Boston1$crim01[testindices],knn.predb1)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct
ErrorRate<-(table["1","0"]+table["0","1"])/sum(table)
ErrorRate
(table["0","1"]/sum(table["0",]))
(table["1","0"]/sum(table["1",]))
print(paste('The power is',1-table['1','0']/sum(table['1',])))
#The Precision of the Model goes here: 
precision<-table[2,2]/sum(table[,2])
precision


require(class)         
trainingcrim01<-Boston1$crim01[trainindices]
standard.X<-scale(Boston1[c(4,7,8)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.predb33<-knn(train.x,test.x,trainingcrim01, k=33)
table<-table(Boston1$crim01[testindices],knn.predb33)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct


require(class)         
trainingcrim01<-Boston1$crim01[trainindices]
standard.X<-scale(Boston1[c(4,7,8)])
train.x<-standard.X[trainindices,]
test.x<-standard.X[testindices,]
knn.predb100<-knn(train.x,test.x,trainingcrim01, k=100)
table<-table(Boston1$crim01[testindices],knn.predb100)
table
Correct<-(table[1,1]+table[2,2])/sum(table)
Correct

#It looks like KNN =5 provides the best results. 


#####################
#### QUESTION 4 ####
#####################
rm(list=ls())
set.seed(5072)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

df=data.frame(x=x,y=y)
df
plot(df)

library(boot)
set.seed(123)

cv.error<-c()

for (i in 1:4){
  glm.fit<-glm(y~poly(x,i),data=df)
  cv.error[i]<-cv.glm(df,glm.fit)$delta[1]
}
cv.error

#####################
rm(list=ls())
set.seed(5072)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

df=data.frame(x=x,y=y)
df
plot(df)

library(boot)
set.seed(456)

cv.error<-c()

for (i in 1:4){
  glm.fit<-glm(y~poly(x,i),data=df)
  cv.error[i]<-cv.glm(df,glm.fit)$delta[1]
}
cv.error

#The resuts are the same as in d.  
#I thought a different seed would have an affect on the results, but something about LOOCV might be leaving something out.  

glm.fita<-glm(y~poly(x,1),data=df)
glm.fitb<-glm(y~poly(x,2),data=df)
glm.fitc<-glm(y~poly(x,3),data=df)
glm.fitd<-glm(y~poly(x,4),data=df)
summary(glm.fita)$coef
summary(glm.fitb)$coef
summary(glm.fitc)$coef
summary(glm.fitd)$coef

#Polynomial 2 (ie glm.fitb) has the best p value and the lowest error.   Yes these results agree.  

