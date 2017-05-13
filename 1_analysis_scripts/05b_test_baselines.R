# performs RF and Logistic baselines on all datasets
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(xgboost)
  library(pROC)
  library(caTools)
  library(ROCR)
  library(boot)
  library(PRROC)
  library(precrec)
  library(ggplot2)
  library(ranger)
  library(caret)
  library(IDPmisc)
})

# function to calculate PR AUC 
auc_pr <- function(obs, pred) {
  xx.df = prediction(pred, obs)
  perf  = performance(xx.df, "prec", "rec")
  xy    = data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  xy    = subset(xy, !is.nan(xy$precision))
  xy    = rbind(c(0, 0), xy)
  res   = trapz(xy$recall, xy$precision)
  res # https://github.com/andybega/auc-pr/blob/master/auc-pr.r
}


for (tbl in c('CCS1', 'CCS2', 'CCS3', 'UNSUP')) {
    ## Note: Baseline models include 1. Random Forest and 2. Logistic Regression
    #########################################################################################################
    ## LOAD DATA
    #########################################################################################################
    ## Update pathnames for the test and training set you would like to perform modeling on
    train <- readRDS(paste('../../cleandata/train.table.', tbl, '.RDS', sep=""))
    test  <- readRDS(paste('../../cleandata/test.table.',  tbl, '.RDS', sep=""))
    
    train_nohist <- train %>% 
      select(-days.till.first.return, -num.revists, -enc_mask, -yr, -mrn_mask) %>%
      mutate(has.revisit = factor(has.revisit))
    train_nohist <- NaRV.omit(train_nohist[complete.cases(train_nohist),])

    test_nohist <- test %>% 
      select(-days.till.first.return, -num.revists, -enc_mask, -yr, -mrn_mask) %>%
      mutate(has.revisit = factor(has.revisit))
    test_nohist <- NaRV.omit(test_nohist[complete.cases(test_nohist),])

    #########################################################################################################
    ##  RANDOM FOREST BASELINE
    #########################################################################################################    
    rf.fit <- ranger(has.revisit ~., data = train_nohist, probability = TRUE)
    
    ## PREDICTIONS
    test.forest = predict(rf.fit, type = "response", data = test_nohist)
    test.forest$predictions
    forestpred  = prediction(test.forest$predictions[,2], test_nohist$has.revisit)
    forestperf  = performance(forestpred, "tpr", "fpr")
    auc.perf    = performance(forestpred, measure="auc")
    pr.auc      = auc_pr(test_nohist$has.revisit, test.forest$predictions[,2])
    roc.vals    = data.frame(cbind(forestperf@x.values[[1]], forestperf@y.values[[1]]))
    colnames(roc.vals) <- c("fp", "tp")
    print(paste('Random Forest ROC AUC for ', tbl, ': ', auc.perf@y.values[[1]], sep=""))
    print(paste('Random Forest PR  AUC for ', tbl, ': ', pr.auc, sep=""))

    # plot ROC curve
    ggplot(roc.vals, aes(x=fp, y=tp, colour="#2980b9")) + 
      labs(x=forestperf@x.name, y=forestperf@y.name) +
      scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
      geom_abline(aes(intercept=0, slope=1, colour="#34495e")) +
      guides(colour = FALSE) +
      ggtitle(bquote(atop(.("ROC Curve"), atop(italic(.(paste("AUC: ", round(auc.perf@y.values[[1]],digits=6)))), "")))) + geom_line(size=1.2)
    
    ggsave(paste('../2_output/rf.auc.', tbl, '.pdf', sep=""), width=11, height=8.5, units="in")
  
    #########################################################################################################
    ## LOGISTIC REGRESSION MODEL WITH ALL VARIABLES
    #########################################################################################################
    ## LOGISTIC REGRESSION MODEL ON ALL VARIABLES
    full.model <- glm(has.revisit ~ ., family=binomial(link='logit'), data=train_nohist)
    summary(full.model)
    # anova(model, test="Chisq") # takes a long time to run (many fitted probabilities numerically 0 or 1 occurs)
    
    ## PREDICTIONS
    full.predict  <- ifelse(predict(full.model, newdata = test_nohist, type="response") >.5,1,0)
    
    # ROC AUC AND PRECISION-RECALL AUC (PR-AUC better for class imbalances where there are ore negatives than positives)
    full.curves <- evalmod(scores = full.predict, labels = test_nohist$has.revisit)
    pr  <- auc(full.curves)[2,4]
    roc <- auc(full.curves)[1,4]
    autoplot(full.curves)

    pred.fpr.tpr = prediction(full.predict, test_nohist$has.revisit)
    auc          = performance(pred.fpr.tpr, measure="auc")@y.values[[1]]
    pr.auc       = auc_pr(test_nohist$has.revisit, full.predict)
    print(paste('Logistic Regression ROC AUC for ', tbl, ': ', auc, sep=""))
    print(paste('Logistic Regression PR  AUC for ', tbl, ': ', pr.auc, sep=""))
}
