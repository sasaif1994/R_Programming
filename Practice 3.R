
pa=read.csv("D:/Imarticus/R_Studio/Property_Price_Train.csv")
pa
#View(pa)
head(pa)
tail(pa)
dim(pa)
colnames(pa)
nrow(pa)
ncol(pa)
colnames(pa)[2] <-'Builclass' # To change the name of the column
# Check nulls
colSums(is.na(pa))
# Check percentage % nulls
colSums(is.na(pa))*100/dim(pa)[1]
# Will give the name of the column with more than 40% nulls 
colnames(pa)[colSums(is.na(pa))*100/dim(pa)[1]>40]

# Let's Remove the columns which have huge nulls
col_to_be_dropped <-  c("Id","Lane_Type" , "Fireplace_Quality" , "Pool_Quality"  , 
                        "Fence_Quality"  , "Miscellaneous_Feature")
names(pa)
pa  <-  pa[            ,      !(names(pa)      %in%     col_to_be_dropped)]
dim(pa)

# Nulls Treatment
table(pa$BsmtFinType2)
pa$BsmtFinType2[is.na(pa$BsmtFinType2)]<-'Unf'

pa$Lot_Extent[is.na(pa$Lot_Extent)]=mean(pa$Lot_Extent,na.rm=TRUE)

mean(pa$Lot_Extent,na.rm=TRUE) # To get the mean ignoring or removing the null from  the columns.

pa$Brick_Veneer_Type[is.na(pa$Brick_Veneer_Type)]       <- 'None'
pa$Brick_Veneer_Area[is.na(pa$Brick_Veneer_Area)]              <- 103
pa$Basement_Height[is.na(pa$Basement_Height)]                  <- 'TA'   
pa$Basement_Condition[is.na(pa$Basement_Condition)]            <- 'TA'
pa$Exposure_Level[is.na(pa$Exposure_Level)]                    <- 'No'
pa$BsmtFinType1[is.na(pa$BsmtFinType1)]                        <- 'Unf'
pa$BsmtFinType2[is.na(pa$BsmtFinType2)]                        <- 'Unf'
pa$Electrical_System[is.na(pa$Electrical_System)]              <- 'SBrkr'
pa$Garage[is.na(pa$Garage)]                                    <-  'Attchd'
pa$Garage_Built_Year[is.na(pa$Garage_Built_Year)]              <-  1979
pa$Garage_Finish_Year[is.na(pa$Garage_Finish_Year)]            <- 'Unf' 
pa$Garage_Quality[is.na(pa$Garage_Quality)]                    <- 'TA'
pa$Garage_Condition[is.na(pa$Garage_Condition)]                <- 'TA'
pa$BsmtFinType2[   is.na(pa$BsmtFinType2)    ]  <- 'Unf'
pa$Brick_Veneer_Type[is.na(pa$Brick_Veneer_Type)]       <- 'None'
pa$Brick_Veneer_Area[is.na(pa$Brick_Veneer_Area)]              <-  mean(pa$Brick_Veneer_Area, na.rm = TRUE)
pa$Basement_Height[is.na(pa$Basement_Height)]                  <- 'TA'   
pa$Basement_Condition[is.na(pa$Basement_Condition)]            <- 'TA'
pa$Exposure_Level[is.na(pa$Exposure_Level)]                    <- 'No'
pa$BsmtFinType1[is.na(pa$BsmtFinType1)]                        <- 'Unf'
pa$BsmtFinType2[is.na(pa$BsmtFinType2)]                        <- 'Unf'
pa$Electrical_System[is.na(pa$Electrical_System)]              <- 'SBrkr'
pa$Garage[is.na(pa$Garage)]                                    <-  'Attchd'
pa$Garage_Built_Year[is.na(pa$Garage_Built_Year)]              <-  mean(pa$Garage_Built_Year, na.rm = TRUE)
pa$Garage_Finish_Year[is.na(pa$Garage_Finish_Year)]            <- 'Unf' 
pa$Garage_Quality[is.na(pa$Garage_Quality)]                    <- 'TA'
pa$Garage_Condition[is.na(pa$Garage_Condition)]                <- 'TA'

# To find if any columns has null 
colnames(pa)[ colSums(is.na(pa))  > 0 ] 

# convert no-numeric(labels) to numbers
table(pa$Road_Type)

# Replace gravel :0 ,Paved :1
library(dplyr)
pa<-mutate(pa,Road_Type=ifelse(Road_Type=='Gravel',0,1)) # just run it once or it will replace all 0 too
# Once new columns created then delete the old column

table(pa$Road_Type)
table(pa$Road_Type1)

table(pa$Property_Slope)

# Replace  gs:0 ,Ms:1,ss:2
pa <- mutate( pa , Property_Slope = ifelse(Property_Slope == 'GS' , 0 , 
                                           ifelse(Property_Slope == 'MS' ,1,2)))
table(pa$Property_Slope)
table(pa$Property_Slope1)

# Label Encoding random assigning variable

pa$Zoning_Class<-as.numeric(as.factor(pa$Zoning_Class))

table(pa$Zoning_Class)
class(pa$Zoning_Class)
class(pa$Road_Type)
summary(pa)


pa$Lot_Extent<-as.numeric(as.factor(pa$Lot_Extent))
pa$Property_Shape<-as.numeric(as.factor(pa$Property_Shape))
pa$Land_Outline<-as.numeric(as.factor(pa$Land_Outline))
pa$Utility_Type<-as.numeric(as.factor(pa$Utility_Type))
pa$Lot_Configuration<-as.numeric(as.factor(pa$Lot_Configuration))
pa$Neighborhood<-as.numeric(as.factor(pa$Neighborhood))
pa$Condition1<-as.numeric(as.factor(pa$Condition1))
pa$Condition2<-as.numeric(as.factor(pa$Condition2))
pa$House_Type<-as.numeric(as.factor(pa$House_Type))
pa$House_Design<-as.numeric(as.factor(pa$House_Design))
pa$Roof_Design<-as.numeric(as.factor(pa$Roof_Design))
pa$Roof_Quality<-as.numeric(as.factor(pa$Roof_Quality))
pa$Exterior1st<-as.numeric(as.factor(pa$Exterior1st))
pa$Exterior2nd<-as.numeric(as.factor(pa$Exterior2nd))
pa$Brick_Veneer_Type<-as.numeric(as.factor(pa$Brick_Veneer_Type))
pa$Exterior_Material<-as.numeric(as.factor(pa$Exterior_Material))
pa$Exterior_Condition<-as.numeric(as.factor(pa$Exterior_Condition))
pa$Foundation_Type<-as.numeric(as.factor(pa$Foundation_Type))
pa$Basement_Height<-as.numeric(as.factor(pa$Basement_Height))
pa$Basement_Condition<-as.numeric(as.factor(pa$Basement_Condition))
pa$Exposure_Level<-as.numeric(as.factor(pa$Exposure_Level))
pa$BsmtFinType1<-as.numeric(as.factor(pa$BsmtFinType1))
pa$BsmtFinType2<-as.numeric(as.factor(pa$BsmtFinType2))
pa$Heating_Type<-as.numeric(as.factor(pa$Heating_Type))
pa$Heating_Quality<-as.numeric(as.factor(pa$Heating_Quality))
pa$Air_Conditioning<-as.numeric(as.factor(pa$Air_Conditioning))
pa$Electrical_System<-as.numeric(as.factor(pa$Electrical_System))
pa$Kitchen_Quality<-as.numeric(as.factor(pa$Kitchen_Quality))
pa$Functional_Rate<-as.numeric(as.factor(pa$Functional_Rate))
pa$Garage<-as.numeric(as.factor(pa$Garage))
pa$Garage_Finish_Year<-as.numeric(as.factor(pa$Garage_Finish_Year))
pa$Garage_Quality<-as.numeric(as.factor(pa$Garage_Quality))
pa$Garage_Condition<-as.numeric(as.factor(pa$Garage_Condition))
pa$Pavedd_Drive<-as.numeric(as.factor(pa$Pavedd_Drive))
pa$Sale_Type<-as.numeric(as.factor(pa$Sale_Type))
pa$Sale_Condition<-as.numeric(as.factor(pa$Sale_Condition))

# The code will tell us if there is any non-numeric columns left
colnames(pa)[grepl('factor|logical|charcter',sapply(pa,class))]

pa$Utility_Type = NULL
pa$Total_Basement_Area = NULL
pa$Grade_Living_Area = NULL

summary(pa)

pa_sample <- sample(2,nrow(pa),replace=TRUE,prob = c(.8,.2)) # It will divide data approximately 80 and 20 percentage 
pa_train=pa[pa_sample==1,]
pa_test=pa[pa_sample==2,]

dim(pa_train)
dim(pa_test)

lm_pa=lm(Sale_Price ~ . , data = pa_train)

summary(lm_pa)

mean(lm_pa$residuals)
hist(lm_pa$residuals)
boxplot(lm_pa$residuals)
plot(lm_pa$residuals)


pred_test = predict(lm_pa,pa_test)

err_test =pa_test$Sale_Price-pred_test
mape=mean(abs(err_test*100/pa_test$Sale_Price))
mape

#install.packages("moments")
library(moments)
skewness(lm_pa$residuals)
kurtosis(lm_pa$residuals) # for normal distribution it should be around 3 

#-------------------------------------

aa=boxplot(pa$Sale_Price)
aa
dim(pa)
pa=pa[pa$Sale_Price<=307000,]
dim(pa)

hist(lm_pa$residuals)

##-----------------------------------------------------------------
##----------------------------------------------------------------

ch=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/churn.csv")
ch

dim(ch)
colSums(is.na(ch))
ch$TotalCharges[is.na(ch$TotalCharges)]=mean(ch$TotalCharges,na.rm=TRUE)

# dropna

ch=mutate(ch,Churn=ifelse(Churn=='No',0,1))

table(ch$Churn)

summary(ch)

ch$gender  <- as.numeric(as.factor(ch$gender))
ch$Partner  <- as.numeric(as.factor(ch$Partner))
ch$Dependents  <- as.numeric(as.factor(ch$Dependents))
ch$PhoneService  <- as.numeric(as.factor(ch$PhoneService))
ch$MultipleLines  <- as.numeric(as.factor(ch$MultipleLines))
ch$InternetService  <- as.numeric(as.factor(ch$InternetService))
ch$OnlineSecurity  <- as.numeric(as.factor(ch$OnlineSecurity))
ch$DeviceProtection  <- as.numeric(as.factor(ch$DeviceProtection))
ch$TechSupport  <- as.numeric(as.factor(ch$TechSupport))
ch$StreamingTV  <- as.numeric(as.factor(ch$StreamingTV))
ch$Contract  <- as.numeric(as.factor(ch$Contract))
ch$PaperlessBilling  <- as.numeric(as.factor(ch$PaperlessBilling))
ch$PaymentMethod  <- as.numeric(as.factor(ch$PaymentMethod))
ch$StreamingMovies  <- as.numeric(as.factor(ch$StreamingMovies))
ch$OnlineBackup  <- as.numeric(as.factor(ch$OnlineBackup))

# The code will tell us if there is any non-numeric columns left
colnames(ch)[grepl('factor|logical|charcter',sapply(ch,class))]

ch$customerID=NULL

samch=sample(2,nrow(ch),replace = TRUE,prob = c(.8,.2))

ch_train = ch[samch==1,]
ch_test=ch[samch==2,]

# Re_Run
model_ch =glm(Churn ~ ., data=ch_train,family='binomial')

summary(model_ch)
pred_test=predict(model_ch,ch_test ,type = 'response')

#length(pred_test)
#nrow(ch_test)
pred_actual1 = data.frame(pred_test,ch_test$Churn)
head(pred_actual1)

colnames(pred_actual1)[2]='Actual'

pred_actual1 =mutate(pred_actual1,predicted = ifelse(pred_test>=.5,1,0))
head(pred_actual1)

# Confusion matrix
tab1= table(pred_actual1$predicted,pred_actual1$Actual)
tab1
#Accuracy
acc=sum(diag(tab1))*100/sum(tab1)
acc

# Oversampling and re run the above data
df1=ch_train[ch_train$Churn==1,]
ch_train=rbind(ch_train,df1)

dim(ch_train)
dim(df1)

# Now from Re_Run
# After oversampling we may able to increase the precision but accuracy will be reduced 


# auroc Plot

#install.packages("ROSE")
library(ROSE)

View(pred_actual1)

roc.curve(pred_actual1$Actual,pred_actual1$pred_test)
#----------------------------------
# DECISION TREE

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

# If we want a better confusion matrix then comment the oversampling part above
# or resample here again
samch=sample(2,nrow(ch),replace = TRUE,prob = c(.8,.2))

ch_train = ch[samch==1,]
ch_test=ch[samch==2,]

df1=ch_train[ch_train$Churn==1,]
ch_train=rbind(ch_train,df1)

ch_train$Churn=as.factor(ch_train$Churn)

model_dt=rpart(Churn ~ . , data = ch_train)


pred_dt=predict(model_dt,ch_test,type = 'class') # we will get output in classes
pred_dt_prob=predict(model_dt,ch_test) # Prediction in probability

tab_dt=table(pred_dt,ch_test$Churn)
tab_dt

model_dt$variable.importance*100 / sum(model_dt$variable.importance)

rpart.plot(model_dt)

#-------------------------------------
# Build Decision tree on ctg file

ctg=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/CTG.csv") 

View(ctg)

ctg_sample <- sample(2,nrow(ctg), replace = TRUE ,prob= c(.8 ,.2))
ctg_train <-  ctg[ctg_sample == 1,]
ctg_test <-   ctg[ctg_sample == 2,]

ctg$NSP <- factor(ctg$NSP)
model_dt <- rpart(NSP  ~ LB + AC + FM , data =ctg_train)
rpart.plot(model_dt ,cex = .5)

             
#--------------
#install.packages("party")

library(party)

ctg_dt=ctree(NSP ~.,data = ctg_train,controls = ctree_control(minsplit = 500))
plot(ctg_dt)


#------------------
# Random Forest

install.packages("randomForest")
library(randomForest)
ctg=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/CTG.csv") 

#View(ctg)

ctg_sample <- sample(2,nrow(ctg), replace = TRUE ,prob= c(.8 ,.2))
ctg_train <-  ctg[ctg_sample == 1,]
ctg_test <-   ctg[ctg_sample == 2,]

ctg_rf=randomForest(NSP ~.,data = ctg_train,ntree=700)
pred_rf=predict(ctg_rf,ctg_test)
table_rf=table(pred_rf,ctg_test$NSP)
table_rf
#-----------------------------------

# KNN
library(class)

ctg=read.csv("D:/Imarticus/R_Studio/Dataset shared by sir/CTG.csv") 

ctg$NSP <- factor(ctg$NSP)
ctg_sample <- sample(2,nrow(ctg), replace = TRUE ,prob= c(.8 ,.2))
ctg_train <-  ctg[ctg_sample == 1,]
ctg_test <-   ctg[ctg_sample == 2,]

ls("package:class")
model_knn=knn(ctg_train,ctg_test,cl=ctg_train$NSP,k=5)

# we do not use predict function over here
table(model_knn,ctg_test$NSP)

nrow(ctg_test)


l1=list()
num=seq(1:50)
for (i in num) {
  model_knn=knn(ctg_train,ctg_test,cl=ctg_train$NSP,k=i)
  tab=table(model_knn,ctg_test$NSP)
  acc=sum(diag(tab))/sum(tab)
  l1=append(l1,acc)
}

class(l1)
plot(num,l1)

plot(unlist(l1))
