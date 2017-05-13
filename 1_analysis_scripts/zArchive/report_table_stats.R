setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(data.table)
  library(caTools)
})

############## load datasets ############## 

source('2b_ins_functions.R')
source('1_demographics_hfu_flagging.R')
source('2a_ed_functions.R')
source('2d_dx_functions.R')
ed_visit  <- readRDS('../../cleandata/ed_visit.RDS') 
ed_visit_ins  <- readRDS('../../cleandata/ed_visit_ins.RDS')
ed_visit_dx <- readRDS('../../cleandata/ed_visit_dx.RDS')
analysis.table <- readRDS('../../cleandata/analysis.table.RDS')


#######Per-year descriptive stats#########
#unique_patient_encounters <- ed_visit %>% 
#  mutate(yr = year(ed_beg)) %>% 
#  select(enc_mask, mrn_mask, yr) %>% distinct()

descriptive_set <- analysis.table %>% left_join(ed_visit[,c('enc_mask','age','pt_sex')], by = 'enc_mask')#%>% merge(ed_visit)#unique_patient_encounters %>% insVars(ed_visit_ins) %>% 
  #add_num_revisits(ed_visit, plots=F) %>% merge(ed_visit)
descriptive_set$yr <- descriptive_set$yr - 2010
for (i in 1:5){
  print(paste('year number', i))
  edYr <- descriptive_set[descriptive_set$yr == i,]
  print(length(unique(edYr$mrn_mask))) # number of pts
  print(nrow(edYr)) # number of encounters
  print(sum(edYr$has.revisit)/nrow(edYr)) #revisit %
  print(edYr %>% 
          group_by(mrn_mask) %>% 
          summarise(n_enc = n_distinct(enc_mask)) %>% 
          summarise(mean(n_enc),
                    median(n_enc), 
                    sd(n_enc))) #encounters per patient
  print(sum(edYr$pt_sex == "F")/nrow(edYr)) #female %
  print(edYr %>% summarise(mean(age), 
                  median(age), 
                  sd(age))) #age stats
  print(sum(as.numeric(edYr$medicaid == 1))/nrow(edYr)) #medicaid %
}

############## HFU Descriptive Stats #########################
hfu_desc <- hfu_flagging(ed_visit, threshold = 10) %>%
  mutate(yr = year(ed_beg)) %>% select(enc_mask, mrn_mask, yr, hfu) %>% distinct() %>%
  add_codes(ed_visit_dx, level = 1) %>%
  insVars(ed_visit_ins)
  


#raw counts
nrow(unique(hfu_desc[hfu_desc$hfu == TRUE, 'mrn_mask']))/nrow(hfu_desc)

##Medicaid coverage
#Chi-square
hfuMed <- nrow(hfu_desc[hfu_desc$hfu == TRUE & hfu_desc$medicaid == 1,])
hfuTotal <- nrow(hfu_desc[hfu_desc$hfu == TRUE,])
nonMed <- nrow(hfu_desc[hfu_desc$hfu == FALSE & hfu_desc$medicaid == 1,])
nonTotal <- nrow(hfu_desc[hfu_desc$hfu == FALSE,])

prop.test(c(hfuMed,hfuTotal),c(nonMed,nonTotal))

#raw percent
hfuMed/hfuTotal
nonMed/nonTotal

##mental health/substance abuse
hfu_desc[is.na(hfu_desc$mental_illness), 'mental_illness'] <- 0

hfuMent <- nrow(hfu_desc[hfu_desc$hfu == TRUE & hfu_desc$mental_illness >= 1,])
hfuTotal <- nrow(hfu_desc[hfu_desc$hfu == TRUE,])
nonMent <- nrow(hfu_desc[hfu_desc$hfu == FALSE & hfu_desc$mental_illness >= 1,])
nonTotal <- nrow(hfu_desc[hfu_desc$hfu == FALSE,])

prop.test(c(hfuMent,hfuTotal),c(nonMent,nonTotal))

#raw percent
hfuMent/hfuTotal
nonMent/nonTotal



ed_visit %>% 
  summarise(n_enc = n_distinct(enc_mask), 
            n_mrn = n_distinct(mrn_mask), 
            pct_rev = mean(has.revisit))
  
ed_visit %>% 
  group_by(mrn_mask) %>% 
  summarise(n_enc = n_distinct(enc_mask)) %>% 
  summarise(mean(n_enc), 
            median(n_enc), 
            sd(n_enc))


ed_visit %>% 
  group_by(pt_sex) %>% 
  summarise(cnt = n()) %>% 
  mutate(pct = cnt/sum(cnt))

ed_visit %>% 
  summarise(mean(age), 
            median(age), 
            sd(age))

ed_visit %>% 
  group_by(clrt_ethnic) %>% 
  summarise(cnt = n()) %>% 
  mutate(pct = cnt/sum(cnt))
colnames(ed_visit)
ed_visit_dx  <- readRDS('../../cleandata/ed_visit_dx.RDS') 

x <- ed_visit %>% select(ht, wt, bmi, mrn_mask, acuity, pcp, resp, bsa, bp_syst, bp_dias, pulse, temp)

cc <- complete.cases(ed_visit %>% select(ht, wt, bmi, mrn_mask, acuity, pcp, resp, bsa, bp_syst, bp_dias, pulse, temp))
length(cc)

ed_visit_dx %>% 
  group_by(dx_icd) %>% 
  summarise(cnt=n_distinct(dx_cde_dotless))
