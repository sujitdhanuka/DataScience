model_call_regression<- function(train_data, valid_data, test_data, col_no= col_no, formula1 = formula1, step_aic, seed, ...){

set.seed(seed)
  #print(col_no)
  pandoc.header("GLM Model Regression")
  #######################################GLM ###############################


  formula1<- paste0(target_variable, "~.")

  glm_model <- glm(as.formula(formula1), data = train_data, family = gaussian(link = "identity"))
  tr_pr <- predict(glm_model, train_data )
  val_pr <- predict(glm_model, valid_data)
  test_pr <- predict(glm_model, test_data)

  train_result[, paste0('GLM_Model', col_no)] <<- tr_pr
  valid_result[, paste0('GLM_Model', col_no)] <<- val_pr
  test_result[, paste0('GLM_Model', col_no)] <<- test_pr

  if(step_aic){
    library(MASS)


    stepaic <- stepAIC(glm_model, direction = "forward")


    rm(glm_model, tr_pr, val_pr, test_pr)


    #######################  GLM with StepAIC ###############

    glm_model <- glm(stepaic$formula, data = train_data, family = gaussian(link = "identity"))
    tr_pr <- predict(glm_model, train_data )
    val_pr <- predict(glm_model, valid_data)
    test_pr <- predict(glm_model, test_data)

    train_result[, paste0('GLM_Model_aic', col_no)] <<- tr_pr
    valid_result[, paste0('GLM_Model_aic', col_no)] <<- val_pr
    test_result[, paste0('GLM_Model_aic', col_no)] <<- test_pr

    rm(glm_model, tr_pr, val_pr, test_pr)

  }

  pandoc.header("Random Forest")
  ################### RANDOM FOREST ####################
  model_rf <- randomForest(as.formula(formula1), data = train_data)
  tr_pr <- predict(model_rf, train_data )
  val_pr <- predict(model_rf, valid_data)
  test_pr <- predict(model_rf, test_data)

  train_result[, paste0('RF_Model', col_no)] <<- tr_pr
  valid_result[, paste0('RF_Model', col_no)]  <<- val_pr
  test_result[, paste0('RF_Model', col_no)]  <<- test_pr
  ##############
  rf_Imp_Attr = data.frame(model_rf$importance)
  rf_Imp_Attr = data.frame(row.names(rf_Imp_Attr),rf_Imp_Attr[,1])
  colnames(rf_Imp_Attr) = c('Attributes', 'Importance')
  rf_Imp_Attr = rf_Imp_Attr[order(rf_Imp_Attr$Importance, decreasing = TRUE),]
  # plot (directly prints the important attributes)
  varImpPlot(model_rf)


  rm(model_rf, tr_pr, val_pr, test_pr)


  if(step_aic){
    ################### RANDOM FOREST with STEP AIC ####################
    model_rf <- randomForest(stepaic$formula, data = train_data)
    tr_pr <- predict(model_rf, train_data )
    val_pr <- predict(model_rf, valid_data)
    test_pr <- predict(model_rf, test_data)

    train_result[, paste0('RF_Model_AIC', col_no)]  <<- tr_pr
    valid_result[, paste0('RF_Model_AIC', col_no)]  <<- val_pr
    test_result[, paste0('RF_Model_AIC', col_no)]  <<- test_pr


    rm(model_rf, tr_pr, val_pr, test_pr)


  }


################################# Random Forest with Important variable########################

sd <- floor(ncol(train_data)/2 ) # store the integer 50% of the importa t variables
top_Imp_Attr = as.character(rf_Imp_Attr$Attributes[1:sd])

# Build the classification model using randomForest
model = randomForest(as.formula(formula1),
                     data=train_data[,c(top_Imp_Attr,target_variable)],
                     keep.forest=TRUE,ntree=50)

pred_Train_rd = predict(model,
                        train_data[,setdiff(names(train_data), target_variable)],
                        type="response",
                        norm.votes=TRUE)

pred_valid_rd <- predict(model,
                         valid_data[,setdiff(names(train_data), target_variable)],
                         type="response",
                         norm.votes=TRUE)


pred_test_rd <- predict(model,
                        test_data,
                        type="response",
                        norm.votes=TRUE)

train_result[, paste0('RF_Tuned', col_no)]  <<- pred_Train_rd
valid_result[, paste0('RF_Tuned', col_no)]  <<- pred_valid_rd
test_result[, paste0('RF_Tuned', col_no)]  <<-  pred_test_rd

################################# Random Forest with Tune Parameter ##################
x <- train_data[,!(names(train_data) %in% c(target_variable))]
y <- train_data[,(names(train_data) %in% c(target_variable))]

model <-tuneRF(x,y,ntreeTry = 50, trace=TRUE, plot=TRUE, doBest = TRUE)
pred_Train_rd = predict(model,
                        train_data[,setdiff(names(train_data), target_variable)],
                        type="response",
                        norm.votes=TRUE)

pred_valid_rd <- predict(model,
                         valid_data[,setdiff(names(train_data), target_variable)],
                         type="response",
                         norm.votes=TRUE)


pred_test_rd <- predict(model,
                        test_data,
                        type="response",
                        norm.votes=TRUE)
train_result[, paste0('RF_Tuned', col_no)]  <<- pred_Train_rd
valid_result[, paste0('RF_Tuned', col_no)]  <<- pred_valid_rd
test_result[, paste0('RF_Tuned', col_no)]  <<-  pred_test_rd

tr_pr <- predict(model, train_data )
val_pr <- predict(model, valid_data)
test_pr <- predict(model, test_data)



############################## KNN #####################

library(class)
# N = 1/3/5/7
Neigh <-7

num_var=setdiff(names(train_data),target_variable)
val_pr=   knn(train_data[,num_var], valid_data[,num_var], train_data[, target_variable], k = Neigh)
test_pr = knn(train_data[,num_var], test_data[, num_var], train_data[, target_variable], k = Neigh)
tr_pr =   knn(train_data[,num_var], train_data[,num_var], train_data[, target_variable], k = Neigh)

train_result[, paste0('knn', col_no)]  <<- as.numeric(tr_pr)
valid_result[, paste0('knn', col_no)]  <<- as.numeric(val_pr)
test_result[, paste0('knn', col_no)]  <<- as.numeric(test_pr)

###########################mysvm model ##################
# print("# SVM Model")
 mysvm_reg(train_data, valid_data, test_data,  col_no , formula1 = formula1)


############################XGBoost #####################

set.seed(108108)
library(xgboost)
#library(tidyverse)
num_var=setdiff(names(train_data),target_variable)
train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, !(names(train_data) %in% c(target_variable))]),
                            label = as.matrix(train_data[, names(train_data) %in% target_variable]))

val_matrix <- xgb.DMatrix(data = as.matrix(valid_data[, !(names(valid_data) %in% c(target_variable))]),
                          label = as.matrix(valid_data[, names(valid_data) %in% target_variable]))

test_matrix <- xgb.DMatrix(data = as.matrix(test_data))

model <- xgboost(data = train_matrix, max.depth = 6, eta = 1, nthread = 3, nround = 800, objective = "reg:linear", verbose = 1, early_stopping_rounds = 10)

tr_pr <- predict(model, train_matrix )
val_pr <- predict(model, val_matrix)
test_pr <- predict(model, test_matrix)

train_result[, paste0('XGBOOST', col_no)]  <<- tr_pr
valid_result[, paste0('XGBOOST', col_no)]  <<- val_pr
test_result[, paste0('XGBOOST', col_no)]  <<- test_pr

rm(model, tr_pr, val_pr, test_pr )
}


################################ mySVM #########################
mysvm_reg <- function(train_data, valid_data, test_data,  col_no , formula1, ...){

  my_seed = 108
  preds_svm <- data.frame(row.names = rownames(valid_data))
  preds_svm_test <- data.frame(row.names = rownames(test_data))
  preds_svm_train <- data.frame(row.names = rownames(train_data))


  i =1
  for(i in seq(1:200)){
    set.seed(my_seed)
    trainIndex <- createDataPartition(train_data[, target_variable], p = .01, list = F)

    train_data1 <- train_data[trainIndex, ]

    model_svm <- svm(as.formula(formula1), train_data1, kernel = "linear", type = "eps-regression")


    preds_svm_train <-  cbind( preds_svm_train,  predict(model_svm, train_data))
    preds_svm  <-  cbind( preds_svm,  predict(model_svm, valid_data))
    preds_svm_test <- cbind( preds_svm_test,  predict(model_svm, test_data))

    colnames(preds_svm)[i] <- paste0("preds_svm",i)
    colnames(preds_svm_test)[i] <- paste0("preds_svm",i)


    my_seed = my_seed +70806

  }

  tr_pr <- rowMeans(preds_svm_train)

  val_pr <- rowMeans(preds_svm)

  test_pr<- rowMeans(preds_svm_test)



  train_result[, paste0('SVM', col_no)]  <<- tr_pr
  valid_result[, paste0('SVM', col_no)]  <<- val_pr
  test_result[, paste0('SVM', col_no)]  <<- test_pr

}

  getmode <- function(v) {
    v <- as.matrix(v)
    row.names(v) <- NULL
    b <- matrix(NA, nrow = nrow(v), ncol=1)
    for(row in 1:nrow(v)){
      uniqv <- unique(v[row,])
      c <- uniqv[which.max(tabulate(match(v[row,], uniqv)))]
      b[row] <- c

    }
    return(b)



    }










