# Imports best model from grid search and performs PR-AUC & ROC-AUC tests
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(xgboost)
  library(dplyr)
  library(ROCR)
  library(ggplot2)
  library(ggthemes)
  library(scales)
  library(precrec)
  library(lubridate)
  library(caTools)
  source('capstone_ggtheme.R')
})

#############################  LOAD DATA ############################# 
prep_data <- function(analysis.table, return='data') {
  data  <- suppressWarnings(as(as.matrix(analysis.table %>% 
                                           select(-mrn_mask, -enc_mask, -days.till.first.return, 
                                                  -num.revists, -has.revisit, -yr)), "dgCMatrix"))
  label <- suppressWarnings(data.matrix(analysis.table %>% select(has.revisit)))
  if (return=='data') {return(data)} 
  if (return=='label' ) {return(as.vector(label))} 
}

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
  test    = readRDS(paste('../../cleandata/test.table.onehot.',  tbl, '.RDS', sep=""))
  x_test  = prep_data(test)
  y_test  = prep_data(test,  return='label')
  train   = readRDS(paste('../../cleandata/train.table.onehot.', tbl, '.RDS', sep=""))
  xgb.tuned.model = xgb.load(paste('../2_output/xgboost.', tbl, '.model', sep=""))
  
  x_train            = prep_data(train)
  y_train            = prep_data(train, return='label')
  tuned.bst.pred     = predict(xgb.tuned.model, newdata = x_test)
  pred.fpr.tpr       = prediction(tuned.bst.pred, y_test)
  compare.perf       = performance(pred.fpr.tpr, "tpr", "fpr")
  auc.perf           = performance(pred.fpr.tpr, measure="auc")@y.values[[1]]
  roc.vals           = data.frame(cbind(compare.perf@x.values[[1]], compare.perf@y.values[[1]]))
  colnames(roc.vals) = c("fp", "tp")

  # plot ROC AUC curve
  ggplot(roc.vals, aes(x=fp, y=tp)) + 
    labs(x=compare.perf@x.name, y=compare.perf@y.name) +
    scale_x_continuous(labels = percent, limits = c(0,1)) + 
    scale_y_continuous(labels = percent, limits = c(0,1)) + 
    geom_abline(aes(intercept=0, slope=1)) + coord_equal() +
    ggtitle(paste("ROC Curve AUC: ", round(auc.perf,digits=6), sep="")) +
    geom_line(size=1.2, colour="#3498db") +
    cap4theme()
  ggsave(paste('../2_output/roc_auc',tbl,'.pdf', sep=""), width=11, height=8.5, units="in")
  
  # precision and recall AUC
  compare.perf.pr    = performance(pred.fpr.tpr, "prec", "rec")
  prec_rec_auc       = auc_pr(y_test, tuned.bst.pred)
  prec_rec_auc.vals  = data.frame(cbind(compare.perf.pr@x.values[[1]], compare.perf.pr@y.values[[1]]))
  colnames(prec_rec_auc.vals) = c("recall", "precision")
  
  # plot PR AUC curve
  ggplot(prec_rec_auc.vals, aes(x=recall, y=precision)) + 
    labs(x=compare.perf.pr@x.name, y=compare.perf.pr@y.name) +
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    ggtitle(paste("PR Curve AUC: ", round(prec_rec_auc,digits=6), sep="")) +
    geom_line(size=1.2, colour="#3498db") +
    cap4theme()
  ggsave(paste('../2_output/pr_auc',tbl,'.pdf', sep=""), width=11, height=8.5, units="in")
  
  # PRINT AUC
  print(paste('Tuned XGBoost ROC AUC for ', tbl, ': ', auc.perf, sep=""))
  print(paste('Tuned XGBoost PR  AUC for ', tbl, ': ', prec_rec_auc, sep=""))
  
  # feature Importance table
  xgMatrix   = xgb.DMatrix(x_train, label = y_train)
  importance = xgb.importance(feature_names = colnames(xgMatrix), model = xgb.tuned.model)
  write.csv(head(importance), file = paste('../2_output/featureImportance_',tbl,'.csv',sep=""))
} 
