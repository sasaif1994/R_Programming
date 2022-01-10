10*8
pwd
getwd()
10^2 
setwd("C:/Users/sasai/OneDrive/Desktop/R_Studio") # Change working directory
var1 <- c(10,20,30) # C stands for combine.
var1
var2 <- c(11,21,31)
var2
var2+var1
10%% 3 # find the reminder
10%/%3 # find the quotient
length(var2)
salary =100
name="saif"
print(c("salary is",salary))
print(c("his name is",name,"and his salary is",salary))
mean(var1)
sum(var1)
sd(var1) # Standard Deviation
median(var1)
## 1 Vector,Matrix,Array,list,data frame

id<- c(1,2,3,4,5,6,7,8,9,10)
sal<- c(100,101,102,103,200,400,600,700,123,344)
we<-c(2,3,4,5,7,10,11,12,20,15)
df1<-data.frame(id,sal,we)
df1
class(df1)
head(df1)
tail(df1)
View(df1)
dim(df1) # Shape of the dataframe
dim(df1)[1] # No of rows
dim(df1)[2] # No of columns

nrow(df1) # No of rows
ncol(df1) # No of columns

df1$sal # Use $ to select your columns
mean(df1$sal)
median(df1$sal)
sum(df1$sal)

# Add a column & row  to existing dataframe
newsal<- c(102,104,105,108,400,500,100,800,124,300)
df1<- cbind(df1,newsal)
df1
new_emp <-c(11,0,9,400)
df1 <-rbind(df1,new_emp)
df1
## Slicing and dicing of data frame
# Select the rows and cols based on user choice

View(iris)
colnames(iris) # To get the Name the 
names(iris)
##R1,we want to select all cols and first 20rows
iris1=iris[c(1:20),]
## R2 ,we want to select all cols and rows as 1,4,5 then from 20 to 60 and then 80 to 100
iris2=iris[c(1,4,5,20:60,80:100),]
View(iris2)

## Select all rows and only 1st 3 cols
iris3=iris[,c(1:3)]
View(iris3)
table(iris$Species)
## Data manipulation in data frame
# another in bulid dataset
View(mtcars)
# Correlation matrix
round(cor(mtcars),3)
?mtcars  ## ? is used for help function

table(mtcars$gear)
# Colmns in mtcars which has only 3 gears
mtcars[mtcars$gear==3,]
table(mtcars$gear,mtcars$am)

table(mtcars$am, mtcars$gear)

# Sir can we count the no of records having 3 gear
dim(mtcars[mtcars$gear==3,])[1]
nrow(mtcars[mtcars$gear==3,])

# COnsider we are going to use a dataframe in our analysis for next some time
attach(mtcars)

table(gear)

detach(mtcars)

table(gear)

# To Know the mean of all columns where 2 is used for cols and 1 for row

apply(mtcars,2,mean)

install.packages("dplyr")

library(dplyr)

# Filter funtion is used to select record bases on the condition
filter_exm <-filter(mtcars,disp==160)
filter_exm
# Select the records which are 4 gear and mpg >15
filter_exm1 <-filter(mtcars,gear==4 & mpg >20)
filter_exm1

or

mtcars[mtcars$gear==4 & mtcars$mpg>20,]

#----------------------------------------

select_exm<- select(mtcars,mpg,cyl) # Select the columns based in col names
select_exm1<- select(mtcars,1,2,9)

# If we want to select cols,mpg,hp,wt
select(mtcars,mpg,wt,hp)
select(mtcars,1,4,6)
arrange(mtcars,disp) ## Arranging the rows base on ascending order based on disp cols
arrange(mtcars,desc(disp))

# Sampling 
sam_mtcars<-sample(2,nrow(mtcars),replace = TRUE,prob = c(.8,.2))
sam_mtcars
train_mtcars<-mtcars[sam_mtcars==1,]
test_mtcars<-mtcars[sam_mtcars==2,]

dim(train_mtcars)
dim(test_mtcars)

## To fix position or oriantation of the sample we use set.seed() 
set.seed(123)
sam_mtcars<-sample(2,nrow(mtcars),replace = TRUE,prob = c(.8,.2))
sam_mtcars

