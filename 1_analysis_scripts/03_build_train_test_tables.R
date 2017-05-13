# create 8 train test tables (CCS1-3 & Embedding)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(data.table)
  library(caTools)
})

############## load datasets ############## 
ed_visit_dx   <- readRDS('../../cleandata/ed_visit_dx.RDS')
ed_visit_ins  <- readRDS('../../cleandata/ed_visit_ins.RDS')
ed_visit_loc  <- readRDS('../../cleandata/ed_visit_loc.RDS')
ed_visit_phys <- readRDS('../../cleandata/ed_visit_phys.RDS')
ed_visit      <- readRDS('../../cleandata/ed_visit.RDS')

############## FLAG 30-DAY REVISITS ############## 
# isolate unique patient encounters
unique_patient_encounters <- ed_visit %>% 
  mutate(yr = year(ed_beg)) %>% 
  select(enc_mask, mrn_mask, yr) %>% distinct()

## load custom feature generation functions
source('2a_ed_functions.R')
source('2b_ins_functions.R')
source('2c_loc_phys_functions.R')
source('2d_dx_functions.R')
source('2e_embedding.R')
source('capstone_ggtheme.R')

############## ANALYSIS.TABLE WITH CATEGORICAL VARAIBLES ##############
## compile all cleaning functions and stack functions
if(!file.exists('../../cleandata/analysis.table.RDS')){
  analysis.table <- unique_patient_encounters %>% 
    add_num_revisits(ed_visit, plots=F) %>%
    add_previous_visits(ed_visit) %>%
    bodyVarsCalc(ed_visit) %>%
    cleanEDvars(ed_visit) %>%
    groupArrivalDisposition(ed_visit) %>%
    insVars(ed_visit_ins) %>% ungroup() %>% # takes a few minutes to run
    add_minutes_in_depts(ed_visit_loc) %>%
    add_num_doctors(ed_visit_phys) %>% 
    duration_stats(ed_visit)  %>% 
    filter(ed_disposition!="Deceased" & raw_mort!=1) %>% 
    mutate(arriv_mode      = factor(arriv_mode),
           ed_disposition  = factor(ed_disposition),
           enc_disposition = factor(enc_disposition),
           primaryIns      = factor(primaryIns),
           secondaryIns    = factor(secondaryIns),
           tertiaryIns     = factor(tertiaryIns)) %>% 
    select(-raw_mort)
  
  analysis.table %>% saveRDS('../../cleandata/analysis.table.RDS')
} else {
  analysis.table <- readRDS('../../cleandata/analysis.table.RDS')
} 

############## ANALYSIS.TABLE WITH ONE HOT ENCODING ############## 
## one hot encoding function
hot_encode <- function(analysis.table) {
  is.fact <- sapply(analysis.table, is.factor)
  factors.df <- analysis.table[, is.fact]
  factors_encode <- model.matrix(~ . + 0, data=factors.df, contrasts.arg = lapply(factors.df, contrasts, contrasts=FALSE), na.action=na.pass)
  analysis.table %>% 
    select(-pcp, -arriv_mode, -ed_disposition, -enc_disposition, -primaryIns, -secondaryIns,
           -tertiaryIns, -medicaid, -medicare, -other, -majority_dept) -> no_factors
  data <- cbind(no_factors, factors_encode)
  return(data)
}

## compile all cleaning functions and stack functions
if(!file.exists('../../cleandata/analysis.table.onehot.RDS')){
  analysis.table.onehot <- unique_patient_encounters %>% 
    add_num_revisits(ed_visit, plots=F) %>%
    add_previous_visits(ed_visit) %>%
    bodyVarsCalc(ed_visit) %>%
    cleanEDvars(ed_visit) %>%
    groupArrivalDisposition(ed_visit) %>%
    insVars(ed_visit_ins) %>% ungroup() %>% # takes a few minutes to run
    add_minutes_in_depts(ed_visit_loc) %>%
    add_num_doctors(ed_visit_phys) %>% 
    duration_stats(ed_visit)  %>% 
    filter(ed_disposition!="Deceased" & raw_mort!=1) %>% 
    mutate(arriv_mode      = factor(arriv_mode),
           ed_disposition  = factor(ed_disposition),
           enc_disposition = factor(enc_disposition),
           primaryIns      = factor(primaryIns),
           secondaryIns    = factor(secondaryIns),
           tertiaryIns     = factor(tertiaryIns)) %>% 
    select(-raw_mort) %>% 
    hot_encode()
    
  analysis.table.onehot %>% saveRDS('../../cleandata/analysis.table.onehot.RDS')
} else {
  analysis.table.onehot <- readRDS('../../cleandata/analysis.table.onehot.RDS')
} 

############## train/test split ############## 
# Train do train test based on 2015 or not in below functions:

############## Create Multiple ICD Level Encodings Tables ############## 
############## CCS 1-3 tables ############## 
for (i in seq(1:3)) {
  temp_table <- analysis.table %>% add_codes(ed_visit_dx, level=i)
  temp_table %>% filter(yr!=2015) %>% 
        saveRDS(paste('../../cleandata/train.table.ccs', i, '.RDS', sep=''))
  temp_table %>% filter(yr==2015) %>% 
        saveRDS(paste('../../cleandata/test.table.ccs',  i, '.RDS', sep=''))
}

############## UNSUPERVISED CLUSTERING ############## 
train.table.unsup <- analysis.table %>% filter(yr!=2015)
test.table.unsup  <- analysis.table %>% filter(yr==2015)
train_pats        <- unique(analysis.table[analysis.table$yr != 2015,]$mrn_mask)
test_pats         <- unique(analysis.table[analysis.table$yr == 2015,]$mrn_mask)

clustered_tables  <- icd_clustering(train.table.unsup, 
                                    train_pats, 
                                    test.table.unsup, 
                                    test_pats)

clustered_tables$train.w.dx %>% saveRDS('../../cleandata/train.table.unsup.RDS')
clustered_tables$test.w.dx  %>% saveRDS('../../cleandata/test.table.unsup.RDS')


############## train/test split for one hot encoding ############## 
# Train do train test based on 2015 or not in below functions:

############## Create Multiple ICD Level Encodings Tables ############## 
############## CCS 1-3 tables ############## 
for (i in seq(1:3)) {
  temp_table <- analysis.table.onehot %>% add_codes(ed_visit_dx, level=i)
  temp_table %>% filter(yr!=2015) %>% 
    saveRDS(paste('../../cleandata/train.table.onehot.ccs', i, '.RDS', sep=''))
  temp_table %>% filter(yr==2015) %>% 
    saveRDS(paste('../../cleandata/test.table.onehot.ccs',  i, '.RDS', sep=''))
}

############## UNSUPERVISED CLUSTERING ############## 
train.table.unsup <- analysis.table.onehot %>% filter(yr!=2015)
test.table.unsup  <- analysis.table.onehot %>% filter(yr==2015)
train_pats        <- unique(analysis.table.onehot[analysis.table.onehot$yr != 2015,]$mrn_mask)
test_pats         <- unique(analysis.table.onehot[analysis.table.onehot$yr == 2015,]$mrn_mask)

clustered_tables  <- icd_clustering(train.table.unsup, 
                                    train_pats, 
                                    test.table.unsup, 
                                    test_pats)

clustered_tables$train.w.dx %>% saveRDS('../../cleandata/train.table.onehot.unsup.RDS')
clustered_tables$test.w.dx  %>% saveRDS('../../cleandata/test.table.onehot.unsup.RDS')

