a <- c(1:10)

a[a > 8]

b<-  a*2

c <- a +b

m1 <- matrix(1:6, 2,3)
dim(m1)


df1 <-data.frame()

set.seed(23)
## create a random sample 

s1 <- sample(1:6, 10, F)

#repeat a  value 
rep(0:1, 20)

sample(rep(0:1, 20), 10)

library(help="datasets")

####################################  DATA SETS ######################


attach(mtcars)

data_cars <- mtcars


colnames(data_cars)
rownames(data_cars)
str(data_cars)
summary(data_cars)

### subsetting methods

mileage <- data_cars[1]   #by default R understands as columns in two dimensional ,  in this ex. mileage is created as data.frame
mileage <- data_cars[,1]
mileage <- data_cars$mpg 
mileage <- data_cars["mpg"]
mileage <- data_cars[[1]]


data_cars$cyl <- as.factor(data_cars$cyl)
data_cars$gear <- as.factor(data_cars$gear)
data_cars$vs  <- as.factor(data_cars$vs)
data_cars$am  <- as.factor(data_cars$am)

summary(data_cars)

max(data_cars$mpg)
min(data_cars$mpg)


data_cars$condition <- ifelse(data_cars$mpg > 25, "Good", ifelse( data_cars$mpg> 20, "OK" ,"Bad"))

 data_cars$condition <- NA

 for(i in 1:nrow(data_cars)){
   
   if(data_cars$mpg[i] >25){
     data_cars$condition[i] = "Good"
     
   }
  else if(data_cars$mpg[i] >20){
    data_cars$condition[i] = "OK"
  }
   else {
     data_cars$condition[i] = "Bad"
   }
   
 } 
 
 data_cars[data_cars$mpg == max(data_cars$mpg),] 
which.max(data_cars$mpg)   

data_cars[which.max(data_cars$mpg)] ## if multiple records has max value it returns only the first record with max value

a = c(1,7,8,NA, 19)

max(a, na.rm= T)

subset(data_cars, subset = data_cars$mpg> 25, select = mpg:qsec)


##### Create a vector with elements 23, 44, 88,100,22  subset all elements except 3rd using the positional index method

c = c(23, 44, 88,100,22)
a <- c[-3]

#  subset all elements exclding 3rd and 5th 

a <-  c[-c(3,5)]
a


###create a vector with values M, F, M, M, M, F convert it to vector

sex <- c("M", "F", "M", "M", "M", "F")

sex <- as.factor(sex)
sex
str(sex)



#### 

divisbleBy <- function(x, divisor =2){
  x[x %% divisor ==0]
  
}

s = c(932, 422, 34, 951, 111, 92, 68, 512, 133,791)

a <- divisbleBy(s, 7)
a


#########WAF that takes dataframe as input  abd returns the dataframe with only the odd cokumn


extractdfByCol <- function(x){
  x[, seq(1, ncol(x), 2)]
}

s = data_CatAtr
ncol(data_CatAtr)
a <- etractdfByCol(s)
 
new_factor <- c("M", "F", "M","O")

new_factor <- as.factor(new_factor)

new_factor[2] <- "O"
new_factor
colnames(s[ , s %% 3 == 0])
colnames(s)


###########Q
sd1 <-function( x, y){
  x %/% y
  
  
}

sd1(3,2)


multi <- function( x, y=22){
  x*y
  
}

multi(123,444)

ex_df <- data.frame(1:10, 11:20, 21:30)

apply(X = ex_df , 2, mean)
ex_df


ex_df <- data.frame(200:300, 400:500, 600:700)
apply(ex_df, 2, sum)



v1 = c(25, 33, NA, 43,NA,  22, NA, 99)
v2 = c(33, 11,10, 11, 22, 9, NA, 100)
v_df <- data.frame(v1, v2)
apply(v_df , 2, function(x){sum(is.na(x))})




#########################
###EX 1 convert all the columns in the DAta_CatAttr dataframe into a factor usin the  apply function




vekta <- seq(1, 10000,4)

table(discretize(vekta, disc = "equalfreq" , 3))


#Q for each income bin compute theaverage income

tapply(data_NumAtr$Income, IncomeBin, mean)


vector1 <- c(100, 122, 124, 444, 888)

subtract_min <- vector1 - min(vector1)

subtract_min 
div_by_max <- subtract_min/(max(vector1) - min(vector1))

div_by_max

data_a <- decostand(vector1, "range") 
data_a
