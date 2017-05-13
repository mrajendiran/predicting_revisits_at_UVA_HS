setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(xgboost)
library(dplyr)
library(ggplot2)
library(ggthemes)
source('capstone_ggtheme.R')

prep_data <- function(analysis.table, return='data') {
  data  <- suppressWarnings(as(as.matrix(analysis.table %>% select(-mrn_mask, -enc_mask, -days.till.first.return, -num.revists, -has.revisit, -yr)), "dgCMatrix"))
  label <- suppressWarnings(data.matrix(analysis.table %>% select(has.revisit)))
  if (return=='data') {return(data)} 
  if (return=='label' ) {return(as.vector(label))} 
}

importances <- data.frame()
for (tbl in c('CCS1', 'CCS2', 'CCS3', 'UNSUP.25')) {
  train    = readRDS(paste('../../cleandata/train.table.', ifelse(tbl=="UNSUP.25", "UNSUP", tbl), '.RDS', sep=""))
  x_train  = prep_data(train)
  y_train  = prep_data(train, return='label')
  xgMatrix = xgb.DMatrix(x_train, label = y_train)

  xgb.tuned.model = xgb.load(paste('../2_output/xgboost.', tbl, '.model', sep=""))
  
  importance = data.frame(tbl = tbl, xgb.importance(feature_names = colnames(xgMatrix), model = xgb.tuned.model))
  importances <- rbind(importances, importance)
}  
colnames(importances) <- tolower(colnames(importances))

importances %>% 
  group_by(tbl) %>% 
  summarise(cnt=n())
## why doesnt every variable have importance? Seems like there are a couple dozen missing per dataset?

## CHECK ON CCS2 FEATS
train    = readRDS('../../cleandata/train.table.CCS2.RDS')
x_train  = prep_data(train)
y_train  = prep_data(train, return='label')
xgMatrix = xgb.DMatrix(x_train, label = y_train)

colnames(xgMatrix)[!(colnames(xgMatrix) %in% importances[importances$tbl=="CCS2",]$feature)]

train %>% select(cystic_fibrosis_56) %>% distinct()

