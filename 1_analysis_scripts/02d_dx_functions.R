# functions to extract variables from DX table
# Groups to CCS 1, 2 and 3 codes
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(reshape2)
  library(data.table)
})

## function to add in # minutes in each department
add_codes <- function(analysis.df, ed_visit_dx, level=3) {
  if (level==1) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_1)==T, "None", condition_level_1))) %>% 
      select(enc_mask, mrn_mask, condition, flg)
  } else if (level==2) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_2)==T, "None", condition_level_2))) %>% 
      select(enc_mask, mrn_mask, condition, flg)
  } else if (level==3) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_3)==T, "None", condition_level_3))) %>% 
      select(enc_mask, mrn_mask, condition, flg)
  }
  analysis.w.dx <- dcast(conditions, enc_mask+mrn_mask~condition, value.var = 'flg', fun.aggregate = sum, fill=0)
  analysis.w.dx <- left_join(analysis.df, analysis.w.dx, by = c("enc_mask", "mrn_mask"))
  
  return(analysis.w.dx)
}

## function to flag whether a given dx code has previously appeared in a patient's history
add_repeat_dx <- function(analysis.df, ed_visit, ed_visit_dx, level = 3){
    if (level==1) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_1)==T, "None", condition_level_1))) %>% 
      select(enc_mask, mrn_mask, condition) %>%
      merge(ed_visit[,c('enc_mask','ed_beg','ed_end')])
  } else if (level==2) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_2)==T, "None", condition_level_2))) %>% 
      select(enc_mask, mrn_mask, condition) %>%
      merge(ed_visit[,c('enc_mask','ed_beg','ed_end')])
  } else if (level==3) {
    conditions <- ed_visit_dx %>% 
      mutate(flg = 1, 
             condition = gsub(" ", "_", ifelse(is.na(condition_level_3)==T, "None", condition_level_3))) %>% 
      select(enc_mask, mrn_mask, condition) %>%
      merge(ed_visit[,c('enc_mask','ed_beg','ed_end')])
  }
  
  patients <- left_join(conditions %>% select(enc_mask, mrn_mask, ed_beg, ed_end, condition),
                                       conditions %>% select(enc_mask, mrn_mask, ed_beg, ed_end, condition),
                                       by="mrn_mask") %>%
                mutate(timelapse = as.double(difftime(ed_end.x, ed_beg.y, units="days"))) %>% 
                filter(enc_mask.x!=enc_mask.y & timelapse > 0) %>%
                mutate(repeatCondition = (condition.x == condition.y), enc_mask = enc_mask.x) %>%
                select(mrn_mask,enc_mask,repeatCondition) %>%
                group_by(mrn_mask, enc_mask) %>%
                summarise(repeatCondition = sum(as.numeric(repeatCondition)) >= 1) %>% ungroup()
    
  analysis.df <- left_join(analysis.df, 
                              patients, 
                              by=c("enc_mask", "mrn_mask")) %>% 
    mutate(repeatCondition    = ifelse(is.na(repeatCondition   )==T, FALSE, repeatCondition   ))
  return(analysis.df)
}

# test functions
# ed_visit       <- readRDS('../../cleandata/ed_visit.RDS')
# ed_visit_dx    <- readRDS('../../cleandata/ed_visit_dx.RDS')
# ed_visit_loc   <- readRDS('../../cleandata/ed_visit_loc.RDS')
# analysis.table <- ed_visit %>% select(mrn_mask, enc_mask) %>% distinct()
# test_add_minutes_in_depts  <- add_minutes_in_depts(analysis.table,  ed_visit_loc)
# test_add_num_doctors  <- add_num_doctors(analysis.table,  ed_visit_phys)
# test_add_codes  <- add_codes(analysis.table,  ed_visit_dx, level=1)
