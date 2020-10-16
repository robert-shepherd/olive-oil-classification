#######
#Setup#
#######

#Data manipulation and visualisation
#install.packages("skimr")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("tidyr")
#install.packages("standardize")
#install.packages("dplyr")
library(skimr)
library(ggplot2)
library(corrplot)
library(tidyr)
library(standardize)
library(dplyr)

#PCA
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

#KNN
#install.packages("class")
library(class)

#Decision tree
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("forcats")
#install.packages("caret")
library(rpart)
library(rpart.plot)
library(randomForest)
library(forcats)
library(caret)

#SVM
#install.packages("e1071")
library(e1071)


#Setting seed so results can be reproduced
set.seed(123)

#Set working directory to the directory containing the project data
#setwd("C:/Users/rshepherd/OneDrive - Merkle Inc/Documents/MSc/Data Mining and Machine Learning/Assignment")

#Load project data
load("oliveoils.RData")

#Filtering data to just show assigned columns
#2431907 690 790 1778 1978
oliveoils_selected <- as.data.frame(oliveoils[,690:790])

#Creating class from labels
oliveoils_selected$class <- oliveoillabels

#Checks
colnames(oliveoils_selected)
dim(oliveoils_selected)
class_check <- as.data.frame(oliveoils_selected$class)
row.names(class_check) <- row.names(oliveoils_selected)
#all match

#Splitting data in to training, test and validation sets
n <- nrow(oliveoils_selected)
ind1 <- sample(c(1:n),round(n/2))
ind2 <- sample(c(1:n)[-ind1],round(n/4))
ind3 <- setdiff(c(1:n),c(ind1,ind2))
train.data <- oliveoils_selected[ind1, ]
valid.data <- oliveoils_selected[ind2, ]
test.data <- oliveoils_selected[ind3, ]

#Casting class as a factor
train.data$class <- as.factor(train.data$class)
valid.data$class <- as.factor(valid.data$class)
test.data$class <- as.factor(test.data$class)

######################
#Exploratory analysis#
######################

#Checking data structure, if any missing data and variance
kable(skim(train.data))

#Exploring correlation between variables
M <- cor(train.data[,-102]);
corrplot(M,method = "color",tl.cex=0.5)

#Checking for outliers
boxplot(train.data[,-102])

#########################
#Reshaping as continuous#
#########################

#Reshaping data to treat wavelength as continuous
train.gather <- gather(train.data,"wavelength","spectroscopic_measurement",1:101)
train.gather$wavelength <- as.numeric(train.gather$wavelength)

#Checking structure
kable(skim(train.gather))

#Plotting reshaped data
d <- ggplot(train.gather,aes(wavelength,spectroscopic_measurement))
d <- d + geom_point(aes(color = class))
d

#Plot filtered for exploration
train.filtered <- train.gather %>%
  filter(wavelength >= 1875)

f <- ggplot(train.filtered,aes(wavelength,spectroscopic_measurement))
f <- f + geom_point(aes(color = class))
f

#Standardising range
train.scaled <- as.data.frame(scale(train.data[-102]))
train.scaled.class <- cbind(train.scaled,class = train.data$class)
train.scaled.gather <- gather(train.scaled.class,"wavelength","spectroscopic_measurement",1:101)
train.scaled.gather$wavelength <- as.numeric(train.scaled.gather$wavelength)

#Checking structure
kable(skim(train.scaled.gather))

#Visualising scaled data
s <- ggplot(train.scaled.gather,aes(wavelength,spectroscopic_measurement))
s <- s + geom_point(aes(color = class))
s

#Boxplot scaled - checking for outliers
boxplot(train.scaled,aes(), xlab = 'wavelength', ylab="spectroscopic measurement (scaled)")

#Reshaping valid and testation data
valid.gather <- gather(valid.data,"wavelength","spectroscopic_measurement",1:101)
valid.gather$wavelength <- as.numeric(valid.gather$wavelength)

test.gather <- gather(test.data,"wavelength","spectroscopic_measurement",1:101)
test.gather$wavelength <- as.numeric(test.gather$wavelength)

##############
#Creating PCA#
##############

#Buiding PCA
train.pca <- prcomp(train.data[,-102],scale = TRUE,center = TRUE)
summary(train.pca)

#Scree plot
plot(train.pca, main = "Scree plot")

#Biplot
gb <- ggbiplot(train.pca,circle = T)
gb

#Selecting first two components and joining to class
train.pca.sel <- cbind(class = train.data$class,as.data.frame(train.pca$x[,1:2]))

#Visualising PCA
p <- ggplot(train.pca.sel,aes(PC1,PC2))
p <- p + geom_point(aes(color = class))
p <- p + ggtitle("PCA scores by class")
p

#Applying PCA to valid and test data
valid.pca <- as.data.frame(predict(train.pca,newdata=valid.data))
test.pca <- as.data.frame(predict(train.pca,newdata=test.data))

#Selecting first two components and joining to class
valid.pca.sel <- cbind(class = valid.data$class,valid.pca[1:2])
test.pca.sel <- cbind(class = test.data$class,test.pca[1:2])


################
#Model building#
################

#Data sources

#Raw
#train.data
#valid.data
#test.data

#PCA
#train.pca.sel
#valid.pca.sel
#test.pca.sel

#####
#KNN#
#####

#Raw

#Selecting k
valid.classes <- valid.data[,102]

class.rate<-numeric(10)
for(k in 1:10)
{
  pred.class<-knn(train.data[,-102], valid.data[,-102], cl=train.data$class, k=k)
  class.rate[k]<-sum((pred.class==valid.classes))/length(pred.class)
}

plot(c(1:10),class.rate,type="l",
     main="Correct Classification Rates for the valid Data for a range of k",
     xlab="k",ylab="Correct Classification Rate")
#Choosing k=3

#Performance on test
test.knn <- knn(train.data[,-102],test.data[,-102],cl=train.data$class,k=3)
table(test.knn,test.data$class)
sum(test.data[,102]==test.knn)/length(test.knn)

#PCA

#Selecting k
valid.classes <- valid.pca.sel[,1]

class.rate<-numeric(15)
for(k in 1:15)
{
  pred.class<-knn(train.pca.sel[,-1], valid.pca.sel[,-1], cl=train.pca.sel$class, k=k)
  class.rate[k]<-sum((pred.class==valid.classes))/length(pred.class)
}

plot(c(1:15),class.rate,type="l",
     main="Correct Classification Rates for the valid Data for a range of k",
     xlab="k",ylab="Correct Classification Rate")
#Choosing k=3

#Test performance
test.knn <- knn(train.pca.sel[,-1],test.pca.sel[,-1],cl=train.pca.sel$class,k=3)
table(test.knn,test.pca.sel$class)
sum(test.pca.sel[,1]==test.knn)/length(test.knn)

###############
#Decision tree#
###############

#Raw

#Building fully grown tree
tree.raw <- rpart(class~.,data = train.data, method = "class",parms = list(split = 'information'),
                  cp=-1,minsplit = 2, minbucket = 1)
rpart.plot(tree.raw,main = "Fully-grown tree using original variables")

#Checking variable importance
tree.raw$variable.importance

#Checking complexity and error values to help prune
printcp(tree.raw)

#2 split tree
tree.raw.2.split <- prune(tree.raw,cp=0.06)

#Plotting pruned tree
rpart.plot(tree.raw.2.split)

#4 split tree
tree.raw.4.split <- prune(tree.raw,cp=-1)

rpart.plot(tree.raw.4.split)

#Random forest

#Random forest doesn't like numbers as column names so relabelling with prefix 'wl' (for wavelength)
train.forest <- train.data
new.cols <- paste("wl",names(train.forest[,-102]),sep="_")
names(train.forest)[1:101] <- new.cols

#Relabelling valid and testation
valid.forest <- valid.data
test.forest <- test.data

names(valid.forest)[1:101] <- new.cols
names(test.forest)[1:101] <- new.cols

#Building forest
forest.raw <- randomForest(class~.,data=train.forest)

#Predicting trees on test data
#2.split
test.pruned.tree.2 <- predict(tree.raw.2.split,newdata = test.data[,-102],type="class")
table(test.pruned.tree.2,test.data$class)
sum(test.data[,102]==test.pruned.tree.2)/length(test.pruned.tree.2)

#4.splt
test.pruned.tree.4 <- predict(tree.raw.4.split,newdata = test.data[,-102],type="class")
table(test.pruned.tree.4,test.data$class)
sum(test.data[,102]==test.pruned.tree.4)/length(test.pruned.tree.4)

#Random forest
test.forest.pred <- predict(forest.raw,newdata = test.forest[,-102],type="class")
table(test.forest.pred,test.forest$class)
sum(test.data[,102]==test.forest.pred)/length(test.forest.pred)

#PCA

#Building fully grown tree
tree.pca <- rpart(class~.,data = train.pca.sel, method = "class",parms = list(split = 'information'),
                  cp=-1,minsplit = 2, minbucket = 1)
rpart.plot(tree.pca,main = "Fully-grown tree using PCA variables")

#Checking variable importance
tree.pca$variable.importance

#Checking complexity and error values to help prune
printcp(tree.pca)

#2 split tree
tree.pca.2.split <- prune(tree.pca,cp=0.10)

#Plotting pruned tree
rpart.plot(tree.pca.2.split)

#3 split tree
tree.pca.3.split <- prune(tree.pca,cp=0.05)

rpart.plot(tree.pca.3.split)

#Random forest

#Storing pca files as forest files to save rewriting code (columns already non-number)
train.forest.pca <- train.pca.sel
valid.forest.pca <- valid.pca.sel
test.forest.pca <- test.pca.sel

#Building forest
forest.pca <- randomForest(class~.,data=train.forest.pca)

#Predicting trees on test data
#2.split
test.pruned.tree.2 <- predict(tree.pca.2.split,newdata = test.pca.sel[,-1],type="class")
table(test.pruned.tree.2,test.pca.sel$class)
sum(test.pca.sel[,1]==test.pruned.tree.2)/length(test.pruned.tree.2)

#3.splt
test.pruned.tree.3 <- predict(tree.pca.3.split,newdata = test.pca.sel[,-1],type="class")
table(test.pruned.tree.3,test.pca.sel$class)
sum(test.pca.sel[,1]==test.pruned.tree.3)/length(test.pruned.tree.3)

#Random forest
test.forest.pca.pred <- predict(forest.pca,newdata = test.forest.pca[,-1],type="class")
table(test.forest.pca.pred,test.forest.pca$class)
sum(test.pca.sel[,1]==test.forest.pca.pred)/length(test.forest.pca.pred)

#####
#SVM#
#####

C.val<-c(0.1,0.5,1,2,5,10)

#Raw

#Linear

#Building models for different cost values to find the most predictive on the valid dataset
pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(class~.,data=train.data,type="C-classification",kernel="linear",
             cost=C.val[i])
  pred.model<-predict(model, valid.data)
  C.error[i]<-pred.error(pred.model,valid.data$class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b",main="Classification error over different cost values")
abline(v=C.sel,lty=2)
#Cost set as 0.1

#Building svm linear model
svm.raw.linear <- svm(class~.,data=train.data,type="C-classification",kernel="linear",cost=0.1)

#Testing
svm.raw.linear.pred <- predict(svm.raw.linear,newdata = test.data[,-102],type="class")
table(svm.raw.linear.pred,test.data$class)
sum(test.data[,102]==svm.raw.linear.pred)/length(svm.raw.linear.pred)

#Radial

#Building models for different cost values to find the most predictive on the valid dataset
pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(class~.,data=train.data,type="C-classification",kernel="radial",
             cost=C.val[i])
  pred.model<-predict(model, valid.data)
  C.error[i]<-pred.error(pred.model,valid.data$class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b",main="Classification error over different cost values")
abline(v=C.sel,lty=2)
#Cost set as 2

#Building svm radial model
svm.raw.radial <- svm(class~.,data=train.data,type="C-classification",kernel="radial",cost=2)

#Testing
svm.raw.radial.pred <- predict(svm.raw.radial,newdata = test.data[,-102],type="class")
table(svm.raw.radial.pred,test.data$class)
sum(test.data[,102]==svm.raw.radial.pred)/length(svm.raw.radial.pred)

#PCA

#Linear

#Building models for different cost values to find the most predictive on the valid dataset
pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(class~.,data=train.pca.sel,type="C-classification",kernel="linear",
             cost=C.val[i])
  pred.model<-predict(model, valid.pca.sel)
  C.error[i]<-pred.error(pred.model,valid.pca.sel$class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b",main="Classification error over different cost values")
abline(v=C.sel,lty=2)
#Cost set as 5

#Building svm linear model
svm.raw.linear <- svm(class~.,data=train.pca.sel,type="C-classification",kernel="linear",cost=5)

#Testing
svm.raw.linear.pred <- predict(svm.raw.linear,newdata = test.pca.sel[,-1],type="class")
table(svm.raw.linear.pred,test.pca.sel$class)
sum(test.pca.sel[,1]==svm.raw.linear.pred)/length(svm.raw.linear.pred)

#Radial

#Building models for different cost values to find the most predictive on the valid dataset
pred.error<-function(pred,truth)
{
  1-sum(diag(table(pred,truth)))/length(truth)
}
C.val<-c(0.1,0.5,1,2,5,10)
C.error<-numeric(length(C.val))
for(i in 1:length(C.val))
{
  model<-svm(class~.,data=train.pca.sel,type="C-classification",kernel="radial",
             cost=C.val[i])
  pred.model<-predict(model, valid.pca.sel)
  C.error[i]<-pred.error(pred.model,valid.pca.sel$class)
}
C.sel<-C.val[min(which.min(C.error))]
C.sel

plot(C.val,C.error,type="b",main="Classification error over different cost values")
abline(v=C.sel,lty=2)
#Cost set as 0.5

#Building svm radial model
svm.raw.radial <- svm(class~.,data=train.pca.sel,type="C-classification",kernel="radial",cost=0.5)

#Testing
svm.raw.radial.pred <- predict(svm.raw.radial,newdata = test.pca.sel[,-1],type="class")
table(svm.raw.radial.pred,test.pca.sel$class)
sum(test.pca.sel[,1]==svm.raw.radial.pred)/length(svm.raw.radial.pred)

#Other

#validing a continuous random forest model model
#train.gather
#valid.gather
#test.gather

#Building model
train.cont <- randomForest(class~.,data=train.gather)

#Predict valid
valid.cont.pred <- predict(train.cont,newdata = valid.gather[,-1],type="class")
sum(valid.gather[,1]==valid.cont.pred)/length(valid.cont.pred)
#0.593477

#Predict test
test.cont.pred <- predict(train.cont,newdata = test.gather[,-1],type="class")
sum(test.gather[,1]==test.cont.pred)/length(test.cont.pred)
#0.4826733

#Poor performance so not including in report
