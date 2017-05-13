# Iteratively retrains model and performs by-day MAP testins
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(Metrics)
  library(dplyr)
  library(lubridate)
  library(xgboost)
  library(ggplot2)
  library(reshape2)
  library(ggthemes)
  source('capstone_ggtheme.R')
})

# 1. Identify encounters per day
# 2. For each day index to get list of encounters
# 3. Append prev_enc to train and train
# 4. Test on day_enc
# 5. Rank Encs by Descending Probabilities
# 6. Store MAP
frame_to_mat_lab <- function(analysis.table, return='matrix') {
  xg.data  <- data.matrix(analysis.table %>% select(-mrn_mask, -enc_mask, -days.till.first.return, -num.revists, -has.revisit, -yr))
  xg.label <- data.matrix(analysis.table %>% select(has.revisit))
  d        <- xgb.DMatrix(xg.data, label = xg.label)
  if (return=='matrix') {return(d)} 
  if (return=='data'  ) {return(xg.data)} 
  if (return=='label' ) {return(as.vector(xg.label))} 
}

# IMPORT TRAIN/TEST
enc_date  <- readRDS('../../cleandata/ed_visit.RDS') %>% 
                  mutate(ed_beg = as_date(ed_beg), ed_end = as_date(ed_end)) %>% 
                  select(enc_mask, ed_beg, ed_end) %>% ungroup()
test  <- left_join(readRDS('../../cleandata/test.table.ccs1.RDS') %>% ungroup(), 
                   enc_date, 
                   by="enc_mask")
train <- readRDS('../../cleandata/train.table.ccs1.RDS')  %>% ungroup()
test_days <- unique(test$ed_end) %>% sort()
xgb.tuned.model = xgb.load(paste('../2_output/xgboost.', 'ccs1', '.model', sep=""))

# point above which a patient is predicted to return
threshold_value = 0.5

#### TEST FOR A TEST DAY
map_days <- data.frame()

for (test_day in test_days) {
  cat('Processing day', which(test_days==test_day), 'of', length(test_days),'\n')
  dftrain  <- rbind(train, test[test$ed_end < test_day, ] %>% select(-ed_beg, -ed_end))
  dftest   <- test[test$ed_end==test_day, ] %>% select(-ed_beg, -ed_end)
  xgtrain  <- frame_to_mat_lab(dftrain)
  xgtest   <- frame_to_mat_lab(dftest)

  # pass new data through pre-tuned model
  bst  <- xgboost(data = xgtrain, nrounds   = 1,  xgb_model = xgb.tuned.model, verbose=FALSE)
  pred <- data.frame('predicted' = ifelse(predict(bst, xgtest) > threshold_value, 1, 0), 
                     'actual'    = test[test$ed_end==test_day, ]$has.revisit, 
                     'random'    = ifelse(runif(nrow(xgtest), min=0, max=1)>threshold_value, 1, 0)) %>% 
              arrange(desc(actual), desc(predicted))
  map_days <- rbind(map_days, 
                    data.frame('date'      = test_day,
                               'pred_map'  = mapk(length(pred$predicted), pred$actual, pred$predicted), 
                               'pred_rand' = mapk(length(pred$predicted), pred$actual, pred$random)))
}
mean(map_days$pred_map)
mean(map_days$pred_rand)
map_days$date2 <- as.Date(map_days$date, origin='1970-01-01')
days_melt <- melt(map_days %>% select(-date), id.vars="date2")
days_melt$Method <- ifelse(days_melt$variable=="pred_map", "XGBoostRank", "Random")

ggplot(days_melt, aes(x=date2, 
                      y=value, 
                      group=Method, 
                      colour=Method)) + 
  geom_line() + geom_smooth() + 
  scale_color_manual(values=c("#95a5a6", "#34495e")) + 
  ggtitle("Daily Mean Average Precision for Patient Predictions") +
  scale_y_continuous(limits=c(0, 1)) + cap4theme()
ggsave('../../cleandata/days_melt.pdf', width=11, height=8.5, units="in")


