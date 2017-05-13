# functions to extract variables from physician table
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(reshape2)
})

## write function to add in # minutes in each department
add_minutes_in_depts <- function(analysis.table, ed_visit_loc) {
  add_time <- ed_visit_loc %>% 
    mutate(time_in_dept = as.numeric(difftime(outdate, indate, units="hours")), 
           dept         = gsub('__', '_', gsub('([[:punct:]])|\\s+','_',tolower(dept))))
  
  na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
  }
  
  add_time <- add_time %>% 
                group_by(mrn_mask, enc_mask, dept) %>% 
                summarise(time_in_dept = sum(time_in_dept, na.rm=T)) %>% 
                group_by(mrn_mask, enc_mask) %>% 
                summarise(max_time       = max(time_in_dept), 
                          majority_dept  = max(ifelse(time_in_dept==max_time, dept, 0)), 
                          total_time     = sum(time_in_dept),
                          dept_mix       = n()/total_time) %>% 
                select(mrn_mask, enc_mask, majority_dept, total_time, dept_mix)
  
  analysis.table <- left_join(analysis.table, add_time, by=c("mrn_mask", "enc_mask")) %>% 
                      mutate(majority_dept = as.factor(ifelse(is.na(majority_dept), 'NoLocations', majority_dept)), 
                             total_time    = na.zero(total_time),
                             dept_mix      = na.zero(dept_mix))
  return(analysis.table)
}

## write function to add in # minutes in each department
add_num_doctors <- function(analysis.table, ed_visit_phys) {
  num_doctors <- ed_visit_phys %>% 
                  group_by(enc_mask, mrn_mask) %>% 
                  summarise(num_docs = as.numeric(max(atn_line)))

  analysis.table <- left_join(analysis.table, num_doctors, by=c("mrn_mask", "enc_mask"))
  return(analysis.table)
}

add_discharge_admit_locs <- function(analysis.table, ed_visit_phys) {
  discharge_locs = ed_visit_phys %>%
                    filter(discharge == 1)
  
  analysis.table <- left_join(analysis.table, discharge_locs, by=c("mrn_mask", "enc_mask"), suffix = c('','_discharge'))
  admit_locs     <- ed_visit_phys %>% filter(admit == 1)
  analysis.table <- left_join(analysis.table, admit_locs, by=c("mrn_mask", "enc_mask"), suffix = c('','_admit'))
  
  return(analysis.table)
}

# test functions
# ed_visit       <- readRDS('../../cleandata/ed_visit.RDS')
# ed_visit_phys  <- readRDS('../../cleandata/ed_visit_phys.RDS')
# ed_visit_loc   <- readRDS('../../cleandata/ed_visit_loc.RDS')
# analysis.table <- ed_visit %>% select(mrn_mask, enc_mask) %>% distinct()
# 
# test_add_minutes_in_depts  <- add_minutes_in_depts(analysis.table,  ed_visit_loc)
# test_add_minutes_in_depts  %>% filter(mrn_mask==22) %>% glimpse
# ed_visit_loc %>% filter(mrn_mask==22) %>% glimpse
# test_add_num_doctors  <- add_num_doctors(analysis.table,  ed_visit_phys)