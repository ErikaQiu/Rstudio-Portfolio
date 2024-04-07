#data exploration
library(readr)
library(ggplot2)
library(gridExtra)
#loading data
train.c <- read_csv("Train.csv")
test.c <- read_csv("Test.csv")
train <- na.omit(train.c)
test <- na.omit(test.c)
str(train)
summary(train[, c(4,7,9)])
layout(mat = matrix(c(1,2,3,4,5,6), ncol = 3))
for(j in 1:ncol(train)){
if(j %in% c(4,7,9)) { #if a numerical variable
hist(train[[j]], xlab = colnames(train)[j], main = "")
boxplot(train[[j]], xlab = colnames(train)[j], main = "")
}
}
panel.cor <- function(x, y) {
par(usr = c(0, 1, 0, 1))
r <- round(cor(x, y, use = "complete.obs"), 2)
txt <- paste0("R = ", r)
cex.cor <- 0.8/strwidth(txt)
text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(train[c("Age", "Work_Experience", "Family_Size")], lower.panel=panel.cor)
par(mfrow=c(2,4))
n <- nrow(train)
for(i in 1:ncol(train)){
if(!i %in% c(1,4,7,9)) { #if not a numerical variable
lbls <- unique(train[,i])[[1]]
pct <- round(100*table(train[,i])/n)
lab <- paste(lbls, pct)
lab <- paste(lab,’%’,sep=’’)
pie(table(train[,i]), main = colnames(train)[i], labels=lab)
}
}
interaction_box <- function(quant_var) {
name <- deparse(substitute(quant_var))
name <- substring(name, 7)
plots <- list(
ggplot(train, aes(x=quant_var, color=Spending_Score))+geom_boxplot()+labs(x=name),
ggplot(train, aes(x=quant_var, color=Ever_Married))+geom_boxplot()+labs(x=name),
ggplot(train, aes(x=quant_var, color=Graduated))+geom_boxplot()+labs(x=name),
ggplot(train, aes(x=quant_var, color=Gender))+geom_boxplot()+labs(x=name),
ggplot(train, aes(x=quant_var, color=Profession))+geom_boxplot()+labs(x=name),
ggplot(train, aes(x=quant_var, color=Var_1))+geom_boxplot()+labs(x=name))
grid.arrange(grobs = plots, nrow = 2)
}
interaction_box(train$Age)
interaction_box(train$Work_Experience)
bp1 <- ggplot(train, aes(x=Age))+geom_bar(aes(fill=Segmentation)) + theme(legend.position="none")
bp2 <- ggplot(train, aes(x=Work_Experience))+geom_bar(aes(fill=Segmentation)) + theme(legend.position="none")
bp3 <- ggplot(train, aes(x=Family_Size))+geom_bar(aes(fill=Segmentation))
grid.arrange(bp1,bp2,bp3, nrow = 1)

#split train and teset data
trainA <- train[which(train$Segmentation=='A'),]
trainB <- train[which(train$Segmentation=='B'),]
trainC <- train[which(train$Segmentation=='C'),]
trainD <- train[which(train$Segmentation=='D'),]
testA <- test[which(test$Segmentation=='A'),]
testB <- test[which(test$Segmentation=='B'),]
testC <- test[which(test$Segmentation=='C'),]
testD <- test[which(test$Segmentation=='D'),]

#Model A&B
trainAB <- rbind(trainA,trainB)
trainAB$Segmentation <- ifelse(trainAB$Segmentation=="A",1,0)
glmAB <- glm(Segmentation~., data=trainAB, family='binomial')
stepAIC(glmAB, scope=list(upper=glmAB, lower=~1), direction = "backward",
k = 2, trace = FALSE)

glmAB_ <- glm(formula = Segmentation ~ Gender + Age + Graduated + Profession +
Work_Experience + Spending_Score + Family_Size + Var_1, family = "binomial",
data = trainAB)
predictAB_ <- ifelse(glmAB_$fitted.values>0.5,'A','B')
confusionAB_ <- table(predictAB_, ifelse(trainAB$Segmentation==1,'A','B'), dnn=c("Predicted","True"))
confusionAB_
sum(diag(confusionAB_)) / sum(confusionAB_)
anova(glmAB_)
#Model A&C
trainAC <- rbind(trainA,trainC)
trainAC$Segmentation <- ifelse(trainAC$Segmentation=="A",1,0)
glmAC <- glm(Segmentation~., data=trainAC, family='binomial')
stepAIC(glmAC, scope=list(upper=glmAC, lower=~1), direction = "backward",
k = 2, trace = FALSE)
glmAC_ <- glm(formula = Segmentation ~ Gender + Age + Graduated + Profession +
Work_Experience + Spending_Score + Family_Size + Var_1, family = "binomial",
data = trainAC)
predictAC_ <- ifelse(glmAC_$fitted.values>0.5,'A','C')
confusionAC_ <- table(predictAC_, ifelse(trainAC$Segmentation==1,'A','C'), dnn=c("Predicted","True"))
confusionAC_
sum(diag(confusionAC_)) / sum(confusionAC_)
anova(glmAC_)
#Model A&D
trainAD <- rbind(trainA,trainD)
trainAD$Segmentation <- ifelse(trainAD$Segmentation=="A",1,0)
glmAD <- glm(Segmentation~., data=trainAD, family='binomial')
stepAIC(glmAD, scope=list(upper=glmAD, lower=~1), direction = "backward",
k = 2, trace = FALSE)
sum(diag(confusionAD_)) / sum(confusionAD_)
anova(glmAD_)
#Model B&C
trainBC <- rbind(trainB,trainC)
trainBC$Segmentation <- ifelse(trainBC$Segmentation=="B",1,0)
glmBC <- glm(Segmentation~., data=trainBC, family='binomial')
stepAIC(glmBC, scope=list(upper=glmBC, lower=~1), direction = "backward",
k = 2, trace = FALSE)
glmBC_ <- glm(formula = Segmentation ~ Gender + Age + Graduated + Profession +
Spending_Score + Family_Size + Var_1, family = "binomial",
data = trainBC)

predictBC_ <- ifelse(glmBC_$fitted.values>0.5,'B','C')
confusionBC_ <- table(predictBC_, ifelse(trainBC$Segmentation==1,'B','C'), dnn=c("Predicted","True"))
confusionBC_
sum(diag(confusionBC_)) / sum(confusionBC_)
anova(glmBC_)

#Model B&D
trainBD <- rbind(trainB,trainD)
trainBD$Segmentation <- ifelse(trainBD$Segmentation=="B",1,0)
glmBD <- glm(Segmentation~., data=trainBD, family='binomial')
stepAIC(glmBD, scope=list(upper=glmBD, lower=~1), direction = "backward",
k = 2, trace = FALSE)

glmBD_ <- glm(formula = Segmentation ~ Gender + Ever_Married + Age + Graduated +
Profession + Work_Experience + Spending_Score + Var_1, family = "binomial",
data = trainBD)
predictBD_ <- ifelse(glmBD_$fitted.values>0.5,'B','D')
confusionBD_ <- table(predictBD_, ifelse(trainBD$Segmentation==1,'B','D'), dnn=c("Predicted","True"))
confusionBD_
sum(diag(confusionBD_)) / sum(confusionBD_)
anova(glmBD_)

#Model C&D
trainCD <- rbind(trainC,trainD)
trainCD$Segmentation <- ifelse(trainCD$Segmentation=="C",1,0)
glmCD <- glm(Segmentation~., data=trainCD, family='binomial')
stepAIC(glmCD, scope=list(upper=glmCD, lower=~1), direction = "backward",
k = 2, trace = FALSE)

glmCD_ <- glm(formula = Segmentation ~ Gender + Age + Graduated + Profession +
Work_Experience + Spending_Score + Family_Size + Var_1, family = "binomial",
data = trainCD)
predictCD_ <- ifelse(glmCD_$fitted.values>0.5,'C','D')
confusionCD_ <- table(predictCD_, ifelse(trainCD$Segmentation==1,'C','D'), dnn=c("Predicted","True"))
confusionCD_
sum(diag(confusionCD_)) / sum(confusionCD_)
anova(glmCD_)
train <- na.omit(read_csv("Train.csv"))
test <- na.omit(read_csv("Test.csv"))
str(train)

#KNN
train.set <- train[,-1]
set.seed(123)
control <- trainControl(method = ’cv’,number = 10)
model <- train(Segmentation~.,train.set[,c(3,6,8,10)],
method = ’knn’,preProcess="scale",
trControl = control,tuneLength=65)
model
plot(model)

error1 <- numeric()
for(i in 5:140){
pred <- class:::knn(as.data.frame(scale(train.set[c(3,6,8)])),
as.data.frame(scale(test[c(4,7,9)])),
as.matrix(train.set[,10]), k = i)
tab<- table(pred, as.matrix(test[,10]))
error1[i] <- (1- sum(diag(tab))/sum(tab))
}
plot(5:140,error1[5:140])

nrow(train)
train.set <- train[,-1]
test.set <- test[,-1]
#Convert Categorical varibale into dummy variable
train.setnew <- dummy_cols(train.set, select_columns = c("Gender","Ever_Married","Graduated","Profession","train.setnew[,c("Age","Family_Size")]<-scale(train.setnew[,c("Age","Family_Size")])
test.setnew <-dummy_cols(test.set, select_columns = c("Gender","Ever_Married","Graduated","Profession","test.setnew[,c("Age","Family_Size")]<-scale(test.setnew[,c("Age","Family_Size")])
                                                                                                                  
error <- numeric()
for(i in 10:200){
pred1 <- neighbr:::knn(train_set=train.setnew[1:1000,-c(1:3)],test_set=test.setnew[1:300,-c(1:4)],k=i,categorical_pred1result <- pred1[["test_set_scores"]][["categorical_target"]]
tab<- table(pred1result, as.matrix(test.setnew[1:300,4]))
error[i] <- (1- sum(diag(tab))/sum(tab))
}
plot(10:200,error[10:200])  
                       
#Convert categorical columns to numeric:
temp <- train
must_convert<-sapply(temp,is.character)
temp2<-sapply(temp[,must_convert],function(x){unclass(as.factor(x))})
train.num<-cbind(temp[,!must_convert],temp2)
temp <- test
must_convert<-sapply(temp,is.character)
temp2<-sapply(temp[,must_convert],function(x){unclass(as.factor(x))})
test.num<-cbind(temp[,!must_convert],temp2)
                                                         
control <- trainControl(method = ’cv’,number = 10,)
model2 <- train(Segmentation~.,train.num[,c(2:11)],
method = ’knn’,
trControl = control,tuneLength=70)
model2
plot(model2,)

error2 <- numeric()
for(i in 5:120){
pred <- class:::knn(as.data.frame(train.num[c(2:10)]),
as.data.frame(test.num[c(2:10)]),
as.matrix(train.num[,11]), k = i)
tab<- table(pred, as.matrix(test.num[,11]))
error2[i] <- (1- sum(diag(tab))/sum(tab))
}
plot(5:120, error2[5:120])

#LDA
library(MASS)
lda_fit <- lda(Segmentation ~ ., data=train.set)
lda_fit          
lda_conf_mat <- table(predict(lda_fit, newdata = test)$class, test$Segmentation)
lda_conf_mat
#misclassification error rate
1-sum(diag(lda_conf_mat)) / sum(lda_conf_mat)                       
