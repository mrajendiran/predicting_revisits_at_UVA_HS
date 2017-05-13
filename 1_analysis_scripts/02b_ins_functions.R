# functions to extract variables from insurance table
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(reshape2)
  library(dplyr)
  library(data.table)
  library(purrr)
})

insVars <- function(analysis.table, ed_visit_ins){
  ins_bin <- function(ins_plan) {
    ins_plan %>%
      map_chr(function(ins) {
        if (grepl("MEDICARE|VIRGINIA PREMIER CC*", ins)) {
          return("MEDICARE")
        }
        if (grepl("MEDICAID|MCAID|EDICAID|MCD", ins)) {
          return("MEDICAID")
        }
        if (grepl("ANTHEM|HUMANA|PACE", ins)) {
          return("MEDICAID/MEDICARE")
        }
        "OTHER"
      }) 
  }
  
  # create: primary, secondary, tertiary, medicare, medicaid, and other features
  analysis.table <- left_join(analysis.table, ed_visit_ins, by=c("enc_mask", "mrn_mask")) %>% 
    group_by(enc_mask) %>%
    mutate(primaryIns = ifelse(insrnc_prio == 1, ins_bin(insrnc_pln_nme), "OTHER")) %>% 
    mutate(secondaryIns = ifelse(insrnc_prio == 2, ins_bin(insrnc_pln_nme), "OTHER")) %>% 
    mutate(tertiaryIns = ifelse(insrnc_prio == 3, ins_bin(insrnc_pln_nme), "OTHER")) %>% 
    mutate(medicaid = factor(ifelse(ins_bin(insrnc_pln_nme) == "MEDICAID" | ins_bin(insrnc_pln_nme) == "MEDICAID/MEDICARE", 1, 0))) %>% 
    mutate(medicare = factor(ifelse(ins_bin(insrnc_pln_nme) == "MEDICARE" | ins_bin(insrnc_pln_nme) == "MEDICAID/MEDICARE", 1, 0))) %>% 
    mutate(other = factor(ifelse(ins_bin(insrnc_pln_nme) == "OTHER", 1, 0))) %>% 
    select(-insrnc_prio, -offsite_data_category_nme, -insrnc_pln_nme) %>% 
    distinct(enc_mask, .keep_all = TRUE)
  
  return(analysis.table)
}
