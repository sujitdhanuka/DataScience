344422 / 239

my_variable <- 344422

monthly_salary <- 75000

annual_salary <- as.integer(monthly_salary * 12)

hike <- annual_salary *2.25

hike

sqrt(x = 25)
log(x = 100, base = 10)

class(my_variable)

integer <- 1000L

class(integer)

string <- "Data Scientist"
class(string)


logical_object <- TRUE

class(logical_object)



complex_object <- 8 +2i

class(complex_object)



vector_object <- (c(1:9))
class(vector_object)

vector_character <- c("sfsdaf", "fsdfsad", "ere")

vector_character[1:3]

mixed_vector <- c("myName", 22)


my_vector = c(98,82,102,99,100) 
my_vector[1]
my_vector[c(1,5)]

sd <- 24

sd  %% 8  == 0 && sd %% 13  == 0 

sd  %% 8 == 0  || sd %% 13  == 0 


com <- 444 * 2i

sd1 <- c(10+4i, 22, "ddd")

class(sd1)

class(sd1[1])

class(sd1[2])


class(c(TRUE, 2))


vector_ex = c(25:600)
vector_ex


vector_ex1 = seq(from=40, to=1000, by=10)

length(vector_ex1)  ## No. of elements in a vector


sss <- seq(1, 2500, 5)   ##from, to, by, not needed if used in same sequence as defined in the function
sss[1:50]     #  C(1:50)  == 1:50   returns TRUE and hence we need not mention the c() function
length(sss)
length(sss[c(1:50)])


sss2  <- sss %% 2

even_indexes  <-  sss2  ==0


sss[even_indexes]

sss[sss %% 2 ==0]



new_vector <- seq (30,400, 9)

new_vector[ new_vector %% 2 == 0 || new_vector %% 3 == 0]


######################### MATRIX ######################

matrix_1 = matrix(c(1:20), nrow =5, ncol=4, byrow = T)

matrix_1

class(matrix_1)


matrix_1[1,]



matrix2 <- matrix(1:1000, nrow = 100)

matrix2[seq(1, 100, 2), 1:5]


matrix2[1:100 %% 2 !=0, 1:5]



vector_x <-  c(3,4,5)
vector_y <- c(6,9,12,15,16,21)

vector_x + vector_y

vector_x <-  c(3,4,5,7)
vector_y <- c(6,9,12,15,16,21)

vector_x + vector_y


c(34, 43, 22, 43, 99) *  c(13, 17)

v_sub1 <- c(1:10)
v_sub2  <- c(11:20)

m1 <-cbind(v_sub1, v_sub2)

m2  <- rbind(v_sub1, v_sub2)

matrix_23  <- matrix(21:40, nrow = 10)

matrix_23 <- cbind(matrix_23, m1)

matrix_23

matrix_23_r  <- matrix(21:40, nrow = 2 )

matrix_23_r <- rbind(matrix_23_r, m2)

matrix_23_r


m5 <-  matrix(20:399, nrow = 20, ncol = 19)

m5 %% 5 ==0 

m5[ m5 %% 5 == 0]

m5[c(1,3,5,7), c(2,5,8)]



####################### LIST #######################3


fl <-  list("a"= c(1:4), "b" = TRUE , C = 20)

fl$a

fl[[1]]

list1 <-  list("first_element"= F,
                 
                 "second_element" = matrix(1:6, nrow= 2, ncol = 3),
                 
                 "third_element" = 20:200,
                 "fourth_element" = "DDLJ")
list1
list1[1]
class(list1[[1]])

list1$first_element
class(list1$first_element)

list1["second_element"]


############################### DATA FRAMES ###########################


df1 <- c(1:10)
df2  <- c(21:30)
df3 <- c(41:50)
df4 <- c(61:70)

df_example <- data.frame("a" = df1, 
                         "b" = df2,
                         "c" = df3,
                         "d" = df4)

df_example

colnames(df_example)

colnames(df_example) <- c("e", "f", "g", "h")

df_example


df_ex2 <- data.frame("first" = c("Sujit ", "Arjun", "Kaushik"),
                     "second" = c(40,  30, 20)
                     )

colnames(df_ex2 ) <- c("Name", "Age")
df_ex2


#Q

df_ex3 <- data.frame("x" = c(1:50),
                     "y" = 51:100)

colnames(df_ex3) <- c("column1", "column2")

colnames(df_ex3)

df_ex3

names(df_ex3)
