rm(list = ls(all.names = T))

dir <- getwd()
setwd(dir)

data <- read.csv(file = "data/Data.csv", header = T)



str(data)
summary(data)
head(data)
tail(data)
sum(is.na(data))  # to clheck null values in data
names(data)

data_NumAtr <- subset(data, select = c(Age, Experience, Income, CCAvg, Mortgage))

data_CatAtr <- subset(data, select = -c(Age, Experience, Income, CCAvg, Mortgage))

summary(data_CatAtr)
table(data_CatAtr$ID)

# data_CatAtr$ID <- as.factor(data_CatAtr$ID)
# data_CatAtr$ZIP.Code <- as.factor(data_CatAtr$ZIP.Code)
# data_CatAtr$Family <- as.factor(data_CatAtr$Family)
# data_CatAtr$Education <- as.factor(data_CatAtr$Education)
# data_CatAtr$Personal.Loan <- as.factor(data_CatAtr$Personal.Loan)
# data_CatAtr$Securities.Account <- as.factor(data_CatAtr$Securities.Account)
# data_CatAtr$CD.Account <- as.factor(data_CatAtr$CD.Account)
# data_CatAtr$Online  <- as.factor(data_CatAtr$Online)
# data_CatAtr$CreditCard <- as.factor(data_CatAtr$CreditCard)
# 
# a <- colnames(data_CatAtr)

data_CatAtr <- data.frame (apply(data_CatAtr, 2, as.factor))
str(data_CatAtr$Onlinema)

summary(data_CatAtr)


# data$ID <- as.factor(data$ID)
# data$ZIP.Code <- as.factor(data$ZIP.Code)
# data$Family <- as.factor(data$Family)
# data$Education <- as.factor(data$Education)
# data$Personal.Loan <- as.factor(data$Personal.Loan)
# data$Securities.Account <- as.factor(data$Securities.Account)
# data$CD.Account <- as.factor(data$CD.Account)
# data$Online  <- as.factor(data$Online)
# data$CreditCard <- as.factor(data$CreditCard)

library(infotheo)

my_func <- function(x){
  
  ls1 <- c( "mean"=mean(x))
  ls1 <- c(ls1, "minimum" =min(x))
  ls1 <- c(ls1, "maximum" =max(x))
  ls1 <- c(ls1, "sum" =sum(x))
  
  ls2 <- matrix(ls1, nrow = 4, ncol(lenght))
  
}
  
length(IncomeBin)
IncomeBin <- discretize(data_NumAtr$Income, disc="equalfreq", nbins = 4 )



table(IncomeBin)
tapply(data$Income, IncomeBin, min)

tapply(data$Income, IncomeBin, max)

tapply(data_NumAtr$Income, IncomeBin, my_func)


income <- data.frame("Income" = data_NumAtr$Income,  "X" = IncomeBin)


data$AgeNew <- 0
for(i in 1:nrow(data)){
  if(data$Age[i] >= 45){
      data$AgeNew[i] = 2
  }else{
      data$AgeNew[i] = 1
  }
}


data$AgeNew  <- ifelse(test = data$Age >= 45, 2,1)


library(dummies)

eduDummiesVars <- dummy(data$Education)
head(eduDummiesVars)

data <- data.frame(subset(data, select= -c(Education)),eduDummiesVars)  #Removes Education and adds dummies of Education
head(data)


library(vegan)

data_NumAtr2 <- decostand(data_NumAtr, "range")        ####using range method
data_NumAtr3 <- decostand(data_NumAtr, "standardize")  ####using 




##################################### CASE STUDY 2######################################


transaction <- read.csv(file = "data/Transactions.csv", header = T, na.strings = c(NA,"?"))
demographics <- read.csv(file = "data/Customer_Demographics_MV_DOB.csv", header = T, na.strings = c(NA,"?"))
cust_Bank_details <- read.csv(file = "data/Customer_Bank Details_MV.csv", header = T, na.strings = c(NA,"?"))

sum(is.na(transaction))
sum(is.na(demographics))
sum(is.na(cust_Bank_details))

head(transaction)
head(demographics)
head(cust_Bank_details)

cust_level <- aggregate(Amount ~ ID, data = transaction, FUN = sum)   ##Group By ID
str(cust_level)

cust_Bank_details[cust_Bank_details == "?"] <- NA
demographics[demographics == "?"] <- NA

summary(transaction)
summary(demographics)
 
merged_data <- merge(demographics, cust_Bank_details, by.x = "Customer.ID", by.y = "ID") ##if id field in both the frame is same then use 'by' else use by.x and by.y
 
merged_data_mv2 <- na.omit(merged_data)
dim(merged_data_mv2)

library(DMwR)
#knnImputation()
merged_data_imputed <- centralImputation(merged_data)
