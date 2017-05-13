# Perform brute force manual grid search for all 4 train tables.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(xgboost)
  library(dplyr)
  library(ROCR)
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(lubridate)
  library(foreach)
  library(doParallel)
  registerDoParallel(cores=16)
})

#####################################################################################
############################# BRUTE FORCE GRID SEARCH ###############################
#####################################################################################
for (feat_type in c('ccs1', 'ccs2', 'ccs3', 'unsup')) {
    #############################  LOAD DATA ############################# 
      prep_data <- function(analysis.table, return='data') {
        data  <- suppressWarnings(as(as.matrix(analysis.table %>% select(-mrn_mask, -enc_mask, -days.till.first.return, -num.revists, -has.revisit, -yr)), "dgCMatrix"))
        label <- suppressWarnings(data.matrix(analysis.table %>% select(has.revisit)))
        if (return=='data') {return(data)} 
        if (return=='label' ) {return(as.vector(label))} 
      }
    
    ############################# PREPARE DATA ############################# 
      train <- readRDS(paste('../../cleandata/train.table.onehot.',feat_type,'.RDS', sep="")) 
    
    ############################# DESIGN TEMPORAL SPLITTING SCHEME ############################# 
      ed_visit <- readRDS('../../cleandata/ed_Visit.RDS') %>% 
                      mutate(ed_beg = update(ymd(as.Date(ed_beg)), day=1)) %>% 
                      select(enc_mask, ed_beg)
      
      # train in moving 6 month windows
      all_months <- tail(unique(ed_visit[ed_visit$ed_beg < "2015-01-01",]$ed_beg) %>% sort(), -6)
      
    ############################# SET UP GRID SEARCH ############################# 
      xgbGrid <- expand.grid(nrounds   = c(10, 50, 100, 200),
                             max_depth = c(3, 6, 9, 12),
                             eta       = c(0.01, 0.03, 0.1))
      xgbGrid$temporal_auc <- NA
      writeLines(paste(nrow(xgbGrid), " Parameter Combinations will be searched", sep=""))
      
    ############################# TEMPORAL TEST GIVEN HYPERPARAMETERS ############################# 
      temporal_test <- function(nrounds, max_depth, eta, training_data, months_list=all_months, ed_visit=ed_visit) {
        auc_list = vector() # store aucs for each time
        for (mnth in months_list) { # loop through months in lag
            mnth_test = as.Date(mnth, origin = "1970-01-01")
            mnth_beg  = mnth_test-months(6)
            
            # subset training data temporally
            # isolate train/test encounters
            train_enc <- ed_visit[(ed_visit$ed_beg >= mnth_beg & ed_visit$ed_beg < mnth_test), ]$enc_mask
            test_enc  <- ed_visit[(ed_visit$ed_beg == mnth_test), ]$enc_mask
            # build train and test folds
            x_train   <- prep_data(training_data %>% filter(enc_mask %in% train_enc))
            y_train   <- prep_data(training_data %>% filter(enc_mask %in% train_enc), return='label')
            x_test    <- prep_data(training_data %>% filter(enc_mask %in% test_enc))
            y_test    <- prep_data(training_data %>% filter(enc_mask %in% test_enc),  return='label')
            # build a model based on the training data
            bst <- xgboost(data      = xgb.DMatrix(x_train, label = y_train),
                           nrounds   = nrounds,  
                           nthread   = 8,  
                           metrics   = list("rmse","auc"), 
                           max_depth = max_depth, 
                           verbose   = FALSE,
                           eta       = eta, 
                           objective = "binary:logistic")
            # attain AUC for this round
            yhatXgb      <- predict(bst, newdata = x_test)
            pred.fpr.tpr <- prediction(yhatXgb, as.factor(y_test))
            # store AUC for this fold
            auc_list     <- rbind(auc_list, performance(pred.fpr.tpr, measure="auc")@y.values[[1]])
          }
        # return AUC across all temporal folds
        return(mean(auc_list))
      }
    
      # test on one hyperparam search
      # temporal_test(nrounds=50, max_depth=10, eta=.1, training_data=train, months_list=all_months, ed_visit=ed_visit)
      
    ################################################################################
    #######################  RUN THE GRID SEARCH IN PARALLEL #######################
    ################################################################################
      grid_solution <- foreach(i = 1:nrow(xgbGrid), .combine  = rbind, 
                               .export   = c("prep_data", "temporal_test", "ed_visit", "train", "all_months", "xgbGrid"),
                               .packages = c('xgboost', 'dplyr', 'ROCR', 'lubridate'))  %dopar% { 
                                  row <- xgbGrid[i, ]
                                  temporal_auc <- temporal_test(nrounds       = row$nrounds, 
                                                                max_depth     = row$max_depth, 
                                                                eta           = row$eta, 
                                                                training_data = train, 
                                                                months_list   = all_months, 
                                                                ed_visit      = ed_visit)
                                  row$temporal_auc <- temporal_auc
                                  row 
                                }
      grid_solution %>% write.csv(paste('../2_output/xgboost.',feat_type,'.gridresults', sep=""))
      
    ################################################################################
    ############################# RUN ON FINAL HOLD OUT ############################
    ################################################################################
      best_params <- grid_solution %>% arrange(desc(temporal_auc)) %>% top_n(1)
      
      ############# load back in the data
      train <- readRDS(paste('../../cleandata/train.table.onehot.',feat_type,'.RDS', sep=""))
      test  <- readRDS(paste('../../cleandata/test.table.onehot.',feat_type,'.RDS', sep="")) 
      tuned.bst <- xgboost(data      = xgb.DMatrix(prep_data(train), label = prep_data(train, return='label')),
                           nrounds   = best_params$nrounds,  
                           nthread   = 8,  
                           metrics   = list("rmse","auc"), 
                           max_depth = best_params$max_depth, 
                           verbose   = FALSE,
                           eta       = best_params$eta, 
                           objective = "binary:logistic")
      xgb.save(tuned.bst, paste('../2_output/xgboost.',feat_type,'.model', sep=""))
}
      