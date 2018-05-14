rm(list = ls(all.names = T))

dir <- getwd()
setwd(dir)

library(infotheo)
library(vegan)
library(DMwR)
library(lubridate)
library(dummies)
library(ggplot2)
#data <- read.csv(file = "data/Data.csv", header = T)
transaction <- read.csv(file = "data/Transactions.csv", header = T, na.strings = c(NA,"?"))
demographics <- read.csv(file = "data/Customer_Demographics_MV_DOB.csv", header = T, na.strings = c(NA,"?"))
cust_Bank_details <- read.csv(file = "data/Customer_Bank Details_MV.csv", header = T, na.strings = c(NA,"?"))


summary(transaction)
summary(demographics)
summary(cust_Bank_details)

sum(is.na(transaction))
sum(is.na(demographics))
sum(is.na(cust_Bank_details))

transaction$Date <- as.Date(transaction$Date, format ="%d-%m-%Y")
demographics$DOB <- as.Date(demographics$DOB, format = "%m/%d/%Y")

which(is.na(demographics$Experience))



#################converts DOB into AGE ###############
age <- function(dob, age.day = today(), units = "years", floor = T) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

#####################################3
demographics$age = age(demographics$DOB) 

#######remove negative vallues in experience as absolute values

demographics$Experience <- abs(demographics$Experience)


###################### Removes NA Values ##################
# ' this function takes three inputs @param 1.  the data.table @param 2. the name of the column for which the NA values are to removed
#                                @param   3. and the name of the column from whoes values  the data is to be averaged
# 'for example if experience column data is missing and we know that it is related to ahge,  we will pass "experience" in 
# 'argument 2 and age in argument 3
# 'from argument 3 we will get the age of the person whose age is missing and then we will calculate the average experience 
# 'for that age group and replace it with the NA.

fill_na <- function(dataTable, field_to_be_removed, field_from_to_be_averaged){
  
  if(field_to_be_removed ==  field_from_to_be_averaged){
    field_from_to_be_averaged <- na.omit( field_from_to_be_averaged)
  }
  sumNa <- sum(is.na((dataTable[field_to_be_removed])))  
  if(!(sumNa)){
    return(dataTable)
  }
  NA_index <- which(is.na(dataTable[field_to_be_removed]))
  
  for(i in 1:sumNa){
    val_in_na <- dataTable[NA_index[i], field_from_to_be_averaged]
    f2_index = which(dataTable[field_from_to_be_averaged]== val_in_na)
    
    
    avg <- mean(dataTable[f2_index, field_to_be_removed] , na.rm = T)
    
 
    dataTable[NA_index[i], field_to_be_removed] = as.integer(floor(avg))
   
    
    
  }
  
  
  return(dataTable)
  
}

########################################



demographics <- fill_na(demographics,"Experience", "age")   # removes NAs in Experience by averag
demographics <- fill_na(demographics, "Income", "Experience") 
demographics <- fill_na(demographics, "Family", "age" ) 
demographics <- fill_na(demographics, "Education", "age")
summary(demographics)
#which(is.na(transaction$Date))


sum(is.na(cust_Bank_details))

summary(cust_Bank_details)




### Merge demographics and cust_bank_details

data <- merge(demographics, cust_Bank_details, by.x = "Customer.ID", by.y = "ID") ##if id field in both the frame is same then use 'by' else use by.x and by.y

###########Create Dummy variables for data$Education
eduDummiesVars <- dummy(demographics$Education)
head(eduDummiesVars)


data <- data.frame(subset(data ,select= -c(Education)),eduDummiesVars)  #Removes Education and adds dummies of Education
head(demographics)

######remove NAs in data 
summary(data)

plot(data$Income, data$CCAvg ,type="p")
plot(data$Family, data$CCAvg ,type="p")
plot(data$age, data$CCAvg ,type="p")
data <- fill_na(data,"CCAvg", "Income")   # removes NAs in CCAVG by Income

plot(data$Income, data$Mortgage ,type="p")
plot(data$Family, data$Mortgage ,type="p")
plot(data$age, data$Mortgage ,type="p")

data <- fill_na(data,"Mortgage", "Income")

summary(data)

plot(data$Income, data$Personal.Loan ,type="p")
plot(data$Family, data$Personal.Loan ,type="p")
plot(data$age, data$Personal.Loan ,type="p")



# since Personal.Loan is not much related to any of the factors like age, Income, Family so we take the central value 
data <- fill_na(data,"Personal.Loan", "Personal.Loan")

summary(data)

###############convert demographics to categorical values and merge demographics and cust_Bank_Details

data <- data.frame (apply( -c(data$age, data$Experience, data$Income, data$CCAvg, data$Mortgage), 2, as.factor))
str(data)
trimws(data ,"both") ### trim both leading and trailing spaces if any
summary(data)

is.null(data$age)


str(data$Experience)

