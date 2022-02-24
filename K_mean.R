sns=read.csv("D:/Imarticus/Unsupervised Learning/K Means Clustering/snsdata.csv")
head(sns)

dim(sns)
colSums(is.na(sns))

table(sns$gender) # Value counts

sns$gender[is.na(sns$gender)] = 'F'
sns$age[is.na(sns$age)]=mean(sns$age,na.rm = TRUE)
colnames(sns)[grepl('factor|logical|character',sapply(sns,class))]

sns$gender=as.numeric(as.factor(sns$gender))

# No samplling needed

model_sns_kmeans=kmeans(sns,5,nstart = 1,iter.max = 10)
model_sns_kmeans

model_sns_kmeans$centers
length(model_sns_kmeans$cluster[model_sns_kmeans$cluster==1])
length(model_sns_kmeans$cluster[model_sns_kmeans$cluster==2])
length(model_sns_kmeans$cluster[model_sns_kmeans$cluster==3])
length(model_sns_kmeans$cluster[model_sns_kmeans$cluster==4])
length(model_sns_kmeans$cluster[model_sns_kmeans$cluster==5])

table(model_sns_kmeans$cluster)

model_sns_kmeans$withinss

1547135.9 + 1475398.3 + 1285646.0 + 1157314.9 + 889561.3

emp_list = list()
num = seq(1:10)
for (i in num) {
  model_sns_kmeans=kmeans(sns,i,nstart = 1,iter.max = 10)
  emp_list=append(emp_list,model_sns_kmeans$tot.withinss)
}
plot(num ,unlist(emp_list),col='red',
     xlab = "Number of Clusters",
     ylab = "Sum squares Distance",
     pch="*",
     type="b",
     main="Elbow Plot")

#----------------------------------------------------------

plot(hclust(dist(mtcars)))

#----------------------------------------------------------
# Feature Selection
# BORUTA ALGORITHM
install.packages("Boruta")
library(Boruta)

install.packages("mlbench")
library(mlbench)

data(Sonar) # load the data in current memory(this sonr data set come from mlbench )
dim(Sonar)

mdl_bor=Boruta(Class~.,data = Sonar)
mdl_bor

mdl_bor$finalDecision

# if we want to get only confirmed 
mdl_bor$finalDecision[mdl_bor$finalDecision=='Confirmed']

mdl_bor$ImpHistory

##--------------------------------------------------------------------------

## Build the boruta on credit risk data and tell me which all are important.

df=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/CreditRisk.csv")
head(df)

colSums(is.na(df))

colnames(df)[ colSums(is.na(df))  > 0 ] 

summary(df)


#_------------------------------------------

install.packages("ISLR")
install.packages("leaps")

library(ISLR)
library(leaps)

ls("package:leaps") # using leaps package we get the function regsubsets which woukd useed for feature selection

data(Hitters)
View(Hitters)

mdl_hit = regsubsets(Salary ~.,Hitters) # By default it will give 12 important features 

?regsubsets
summary(mdl_hit)
# To get 12 important featuers 
mdl_hit = regsubsets(Salary ~.,Hitters,nvmax=12)
summary(mdl_hit)

#---------------------------------------------
#Rdge & Lasso
install.packages("glmnet")
library(glmnet)
bodyfat=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/bodyfat.csv")

install.packages('DEoptimR') 
install.packages("caret",dep = TRUE)
install.packages("ggplot2")
install.packages("lattice")
install.packages("lava")
install.packages("purrr")
install.packages("recipes", dependencies = TRUE)
install.packages("")
install.packages("recipes", dependencies = c("fastICA")) 
library(caret)
library(recipes)
install.packages("recipes")
# to see correlation in the dataset
View(cor(bodyfat))
View(cor(bodyfat[,c(2:10)]))

lambda_seq=seq(0,3,length=100)

model_lasso= train(Bodyfat~.,data= bf,method ="glmnet",
                  tuneGrid=expand.grid(alpha=1,lambda =lambda_seq)
                  )  
# Note:- If alpha is 1 then it is lasso or if it is zero then it is ridge


coef(model_lasso$finalModel,model_lasso$Tune$lambda) # tHis lines gives you final beta values

pred=predict((model_lasso))

pred_actu= data.frame(pred,bodyfat$Bodyfat)
#------------------------------------------------------------

# Naive bayes 
install.packages("e1071")
library(e1071)

ctg=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/CTG.csv")
ctg_NB=naiveBayes(NSP~.,ctg_train)
pred= predict(ctg_NB,ctg_test)

modelnb=train(
  BodyFat
)

# Confusion matrix
tab1= table(pred_actual1$predicted,pred_actual1$Actual)
tab1
#Accuracy
acc=sum(diag(tab1))*100/sum(tab1)
acc

#----------------------------------------------------
# SVM
ctg_svm=svm(NSP~.,ctg_train)

?svm
install.packages('knitr')

#--------------------
library(caret)
s=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/svmfile.csv")
install.packages("tidymodels")


install.packages("tinytex")

library(tinytex)
