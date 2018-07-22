# SDAutoML

# This is to automate Machine learning algorithms

# Accepts the Data as dataframe, pre_test_data accepts  the test dataframe , m_type - whether as the problem is a classification
# or regression type, target_variable accepts the name of the target variable, preprocess accepts a matirx of all the preprocess to
# to be performed using caret package




sdAutoML <- function(Data, pre_test_data = NULL , m_type, target_variable, preprocess, step_aic= FALSE , seed = 108108, ...){
  ## Train-Validation split

   set.seed(seed)
  require(reshape2) #For plots
  require(knitr) # For tables
  require(Hmisc)# Description of the data
  require(rpart)
  require(caret)
  require(rpart.plot)
  require(DMwR)
  require(DataExplorer)
  require(e1071)
  require(kernlab)
  require(randomForest)
  require(ipred)
  require(gbm)
  require(ROSE)
  require(pROC)
  require(pander)
  # require(tidyverse)


  if(m_type  == "Classification"){

    trainIndex <- createDataPartition(Data[, target_variable], p = .85, list = F)
    pre_train_data <- Data[trainIndex, ]
    pre_valid_data <- Data[-trainIndex, ]
  }else if(m_type == "Regression"){

    train_index <- sample(1:nrow(Data), 0.8 * nrow(Data))
    valid_index <- setdiff(1:nrow(Data), train_index)
    pre_train_data <- Data[train_index, ]
    pre_valid_data <- Data[valid_index, ]

  } else if(m_type == "Multi_class"){
    trainIndex <- createDataPartition(Data[, target_variable], p = .85, list = F)
    pre_train_data <- Data[trainIndex, ]
    pre_valid_data <- Data[-trainIndex, ]

  }
  ##########################################
  train_result <<- data.frame(target = pre_train_data[, target_variable], row.names = row.names(pre_train_data))
  valid_result <<- data.frame(target = pre_valid_data[, target_variable], row.names = row.names(pre_valid_data))
  ## PreProcess
  formula1<- paste0(target_variable, "~.")
  col_no = 1
  for(pp in preProcess){
    std_method <- preProcess(pre_train_data[, !names(pre_train_data) %in% target_variable ], method = pp)
    train_data <- predict(std_method, pre_train_data)
    valid_data <- predict(std_method, pre_valid_data)
    if(!is.null(pre_test_data) ){
      test_data <- predict(std_method, pre_test_data)
    }else{
      test_data <- NULL

    }


    # print(sum(is.na(train_data)))
    ## Build model with each Preprocess step

    if(m_type == "Classification"){
      model_call_class(train_data, valid_data, test_data, col_no= col_no, formula1 = formula1, step_aic = step_aic, seed = seed)


    }else if(m_type == "Regression") {

      model_call_regression(train_data, valid_data, test_data, col_no= col_no, formula1 = formula1, step_aic = step_aic, seed = seed)
    }else if(m_type ==  "Multi_class"){

      model_call_multi_class(train_data, valid_data, test_data, col_no= col_no, formula1 = formula1, step_aic = step_aic, seed = seed)
    }




    col_no <- col_no +1
  }


}

model_call_class <- function( train_data, valid_data, test_data,  col_no , formula1, step_aic, seed, ...){
  set.seed(seed)
  #print(col_no)
  #pandoc.header("GLM Model")
  #######################################GLM ###############################


  # formula1<- paste0(target_variable, "~.")
  #
  # glm_model <- glm(as.formula(formula1), data = train_data, family = "binomial")
  # tr_pr <- as.factor(as.character(ifelse(predict(glm_model, train_data )>= .5, "Yes", "No")))
  # val_pr <- as.factor(as.character(ifelse(predict(glm_model, valid_data)>= .5, "Yes", "No")))
  # test_pr <- as.factor(as.character(ifelse(predict(glm_model, test_data) >= .5, "Yes", "No")))
  #
  # train_result[, paste0('GLM_Model', col_no)] <<- tr_pr
  # valid_result[, paste0('GLM_Model', col_no)] <<- val_pr
  # test_result[, paste0('GLM_Model', col_no)] <<- test_pr
  #
  # if(step_aic){
  #   require(MASS)
  #
  #
  #   stepaic <- stepAIC(glm_model, direction = "forward")
  #
  #
  #   rm(glm_model, tr_pr, val_pr, test_pr)
  #
  #
  #   #######################  GLM with StepAIC ###############
  #
  #   glm_model_aic <- glm(stepaic$formula, data = train_data, family = "binomial")
  #   tr_pr <- as.factor(as.character(ifelse(predict(glm_model_aic, train_data )>= .5, "Yes", "No")))
  #   val_pr <- as.factor(as.character(ifelse(predict(glm_model_aic, valid_data)>= .5, "Yes", "No")))
  #   test_pr <- as.factor(as.character(ifelse(predict(glm_model_aic, test_data) >= .5, "Yes", "No")))
  #
  #   train_result[, paste0('GLM_Model_aic', col_no)] <<- tr_pr
  #   valid_result[, paste0('GLM_Model_aic', col_no)] <<- val_pr
  #   test_result[, paste0('GLM_Model_aic', col_no)] <<- test_pr
  #
  #   rm(glm_model_aic, tr_pr, val_pr, test_pr)
  #
  # }



  ################### RANDOM FOREST ####################
  model_rf <- randomForest(as.formula(formula1), data = train_data)
  tr_pr <- predict(model_rf, train_data )
  val_pr <- predict(model_rf, valid_data)
  test_pr <- predict(model_rf, test_data)

  train_result[, paste0('RF_Model', col_no)] <<- tr_pr
  valid_result[, paste0('RF_Model', col_no)]  <<- val_pr
  test_result[, paste0('RF_Model', col_no)]  <<- test_pr
  ##############
  imp_var <- importance(model_rf)

  print(imp_var)
  varImpPlot(model_rf)

  rm(model_rf, tr_pr, val_pr, test_pr)

  # if(step_aic){
  #   ################### RANDOM FOREST with STEP AIC ####################
  #   model_rf <- randomForest(stepaic$formula, data = train_data)
  #   tr_pr <- predict(model_rf, train_data )
  #   val_pr <- predict(model_rf, valid_data)
  #   test_pr <- predict(model_rf, test_data)
  #
  #   train_result[, paste0('RF_Model_AIC', col_no)]  <<- tr_pr
  #   valid_result[, paste0('RF_Model_AIC', col_no)]  <<- val_pr
  #   test_result[, paste0('RF_Model_AIC', col_no)]  <<- test_pr
  #
  #
  #   rm(model_rf, tr_pr, val_pr, test_pr)
  #
  # }





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


  ########################## Decision Tree using C5.0 ####################

  require(C50)
  c5_tree <- C5.0(as.formula(formula1) , data= train_data)

  tr_pr <- predict(c5_tree, train_data )
  val_pr <- predict(c5_tree, valid_data)
  test_pr <- predict(c5_tree, test_data)

  train_result[, paste0('C5.0_tree', col_no)]  <<- tr_pr
  valid_result[, paste0('C5.0_tree', col_no)]  <<- val_pr
  test_result[, paste0('C5.0_tree', col_no)]  <<- test_pr

  rm(c5_tree, tr_pr, val_pr, test_pr)



  ################################ Decision using Rpart ######################
  require(rpart)
  reg_tree <- rpart(as.formula(formula1), train_data, method = "class")

  printcp(reg_tree)
  tr_pr <- predict(reg_tree, train_data , type="class")
  val_pr <- predict(reg_tree, valid_data , type="class")
  test_pr <- predict(reg_tree, test_data , type="class")

  train_result[, paste0('Rpart_tree', col_no)]  <<- tr_pr
  valid_result[, paste0('Rpart_tree', col_no)]  <<- val_pr
  test_result[, paste0('Rpart_tree', col_no)]  <<- test_pr

  rm(reg_tree, tr_pr, val_pr, test_pr)


  ############################## KNN #####################

  # require(class)
  # # N = 1/3/5/7
  # Neigh <-5
  #
  # num_var=setdiff(names(train_data),target_variable)
  # val_pr=   knn(train_data[,num_var], valid_data[,num_var], train_data[, target_variable], k = Neigh)
  # test_pr = knn(train_data[,num_var], test_data[, num_var], train_data[, target_variable], k = Neigh)
  # tr_pr =   knn(train_data[,num_var], train_data[,num_var], train_data[, target_variable], k = Neigh)
  #
  # train_result[, paste0('knn', col_no)]  <<- tr_pr
  # valid_result[, paste0('knn', col_no)]  <<- val_pr
  # test_result[, paste0('knn', col_no)]  <<- test_pr

  ###########################mysvm model ##################
  print("# SVM Model")
  mysvm(train_data, valid_data, test_data,  col_no , formula1 = formula1)


  ############################XGBoost #####################

#   set.seed(108108)
#   require(xgboost)
#   #require(tidyverse)
#   num_var=setdiff(names(train_data),target_variable)
#   train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, !(names(train_data) %in% c(target_variable))]),
#                               label = as.matrix(train_data[, names(train_data) %in% target_variable]))
#
#   val_matrix <- xgb.DMatrix(data = as.matrix(valid_data[, !(names(valid_data) %in% c(target_variable))]),
#                             label = as.matrix(valid_data[, names(valid_data) %in% target_variable]))
#
#   test_matrix <- xgb.DMatrix(data = as.matrix(test_data))
#
#   model <- xgboost(data = train_matrix, max.depth = 6, eta = 0.3, nthread = 3, nround = 800, objective = "binary:logistic", verbose = 1, early_stopping_rounds = 10)
#
#   tr_pr <- as.factor(as.character(ifelse(predict(model, train_matrix )>= .5, "yes","No")))
#   val_pr <- as.factor(as.character(ifelse(predict(model, val_matrix)>= .5, "yes","No")))
#   test_pr <- as.factor(as.character(ifelse(predict(model, test_matrix) >= .5, "yes","No")))
#
#   train_result[, paste0('XGBOOST', col_no)]  <<- tr_pr
#   valid_result[, paste0('XGBOOST', col_no)]  <<- val_pr
#   test_result[, paste0('XGBOOST', col_no)]  <<- test_pr
#
#   rm(model, tr_pr, val_pr, test_pr )
 }


################################ mySVM #########################
mysvm <- function(train_data, valid_data, test_data,  col_no , formula1, ...){

  my_seed = 108
  preds_svm <- data.frame(row.names = rownames(valid_data))
  preds_svm_test <- data.frame(row.names = rownames(test_data))
  preds_svm_train <- data.frame(row.names = rownames(train_data))


  i =1
  for(i in seq(1:20)){
    set.seed(my_seed)
    trainIndex <- createDataPartition(train_data[, target_variable], p = .01, list = F)

    train_data1 <- train_data[trainIndex, ]

    model_svm <- svm(as.formula(formula1), train_data1, kernel = "linear", type = "C")


    preds_svm_train <-  cbind( preds_svm_train,  as.factor(as.character(predict(model_svm, train_data))))
    preds_svm  <-  cbind( preds_svm,  as.factor(as.character(predict(model_svm, valid_data))))
    preds_svm_test <- cbind( preds_svm_test,  as.factor(as.character(predict(model_svm, test_data))))

    colnames(preds_svm)[i] <- paste0("preds_svm",i)
    colnames(preds_svm_test)[i] <- paste0("preds_svm",i)


    my_seed = my_seed +70806

  }

  tr_pr <- as.factor(getmode(preds_svm_train))

  val_pr <- as.factor(getmode(preds_svm))

  test_pr<- as.factor(getmode(preds_svm_test))



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
