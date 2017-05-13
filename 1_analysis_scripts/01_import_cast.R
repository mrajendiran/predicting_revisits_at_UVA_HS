# load original RDATA sets, cast important variables and save as RDS

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(reshape2)
})

################# ED VISIT ######################################################## 
ed_visit <- get(load('../../rawdata/ED_Visit.RData'))
ed_visit$ED_Beg        <- mdy_hm(ed_visit$ED_Beg       )
ed_visit$ED_End        <- mdy_hm(ed_visit$ED_End       )
ed_visit$ED_Arrv_Dtm   <- mdy_hm(ed_visit$ED_Arrv_Dtm  )
ed_visit$ED_BdReq_Dtm  <- mdy_hm(ed_visit$ED_BdReq_Dtm )
ed_visit$ED_Rmd_Dtm    <- mdy_hm(ed_visit$ED_Rmd_Dtm   )
ed_visit$Admt_frED_Dtm <- mdy_hm(ed_visit$Admt_frED_Dtm)
ed_visit$ED_Dsch_Dtm   <- mdy_hm(ed_visit$ED_Dsch_Dtm  )
ed_visit$ED_Lft_Dtm    <- mdy_hm(ed_visit$ED_Lft_Dtm   )
colnames(ed_visit)     <- tolower(colnames(ed_visit))

ed_visit %>% saveRDS('../../cleandata/ed_visit.RDS')

################# ICD9 AND ICD10 LOOKUPS ######################################################## 
conn <- file("../../rawdata/CCS_ICD9.txt",open="r")
all_lines <-readLines(conn)
results = list()
# parse through file
for (i in 5:length(all_lines)) { # start at 5th line
  row = all_lines[i]
  if (nchar(row)>0) {
    if (substr(row, 0, 5)!= "     ") {
      current_condition = substr(row, 6, nchar(row))
    } else {
      current_codes = trimws(row)
      results[current_condition] = paste(results[current_condition], current_codes, sep=" ")
    }
  }
}
close(conn)
results <- as.data.frame(as.matrix(results))
colnames(results) <- c("codes")
# combine into dataframe
split_out <- data.frame(do.call('rbind', strsplit(as.character(results$codes),' ',fixed=TRUE)))
split_out$condition <- rownames(results)
# pivot long
icd9_lookup <- melt(split_out, id.vars="condition") %>% 
  filter(value!="NULL") %>% mutate(code=value) %>% 
  select(condition, code) %>% distinct()

####### LOAD THE ICD-10 Codes #######
icd10_lookup <- read_delim("../../rawdata/CCS_ICD10.csv", delim='|', progress=F)
colnames(icd10_lookup) <- c('icd10_code', 'ccs_category', 'icd10_desc', 'ccs_category_desc', 
                            'multi_ccs_lvl_1', 'multi_ccs_lvl_1_label', 'multi_ccs_lvl_2', 'multi_ccs_lvl_2_label')
# remove single quotes
icd10_lookup <- icd10_lookup %>% 
  mutate(icd10_code            = gsub("'", "", icd10_code), 
         ccs_category          = gsub("'", "", ccs_category),
         multi_ccs_lvl_1       = gsub("'", "", multi_ccs_lvl_1), 
         multi_ccs_lvl_2       = gsub("'", "", multi_ccs_lvl_2), 
         multi_ccs_lvl_2_label = gsub("'", "", multi_ccs_lvl_2_label))

####### MAP ICD-9 AND ICD-10 CODES #######
icd9_lookup <- icd9_lookup %>% 
  mutate(
    version = 9,  flg=1,
    code = tolower(code),
    condition = gsub("  ", " ",gsub("[[:punct:]]", "",tolower(condition))),
    condition = gsub("e codes ", "external cause codes ", condition) ,
    condition_level_3=ifelse(condition=="screening and history of mental health and substance abusexternal cause codes", "screening and history of mental health and substance abuse codes", 
              ifelse(condition=="other pregnancy and delivery including normal","normal pregnancy andor delivery", 
              ifelse(condition=="external cause codes cutpierceb", "external cause codes cutpierce", 
              ifelse(condition=="miscellaneous mental health disorders", "miscellaneous disorders", 
              ## these things are in 9 and not 10 so we will lump together
              ifelse(condition %in% c("external cause codes adverse effects of medical care", "external cause codes adverse effects of medical drugs", "external cause codes naturalenvironment" , "external cause codes other specified and classifiable" , "external cause codes other specified nec" , "external cause codes overexertion" ,                   "external cause codes place of occurrence" , "external cause codes poisoning" , "external cause codes struck by against" , "external cause codes suffocation" ,  "external cause codes unspecified"), 
                     "external cause codes unspecified", 
                     condition)))))) %>% 
  select(version, flg, code, condition_level_3)

icd10_lookup <- icd10_lookup %>% 
  mutate(
    version = 10, flg=1,
    code = tolower(icd10_code),
    condition_level_3 = gsub("  ", " ",gsub("[[:punct:]]", "",tolower(ccs_category_desc))),
    condition_level_1 = gsub("  ", " ",gsub("[[:punct:]]", "",tolower(multi_ccs_lvl_1_label))),
    condition_level_2 = gsub("  ", " ",gsub("[[:punct:]]", "",tolower(multi_ccs_lvl_2_label)))
    ) %>% 
  select(version, flg, code, condition_level_3, condition_level_2, condition_level_1)

# at this point the two lookups are mappable on the condition_level_3 column
# going to pull in additional levels to the ICD9 Lookup. If there is no mapping (aka the 
# external cause unknown variable then just use condition level 3)
icd9_lookup <- left_join(icd9_lookup,
                         icd10_lookup %>% select(condition_level_3, condition_level_2, condition_level_1) %>% distinct(),
                         by="condition_level_3") %>% 
                mutate(condition_level_2 = ifelse(is.na(condition_level_2)==T, condition_level_3, condition_level_2), 
                       condition_level_1 = ifelse(is.na(condition_level_1)==T, condition_level_3, condition_level_1))

# stack final lookup
all_code_lookup <- rbind(
  icd9_lookup  %>% distinct(version, code, condition_level_3, condition_level_2, condition_level_1),
  icd10_lookup %>% distinct(version, code, condition_level_3, condition_level_2, condition_level_1)) %>% 
  mutate(condition_level_2 = ifelse(trimws(condition_level_2)=="", condition_level_1, condition_level_2)) # if condition level 2 is blank then replace with group 3

################# ED DX VISIT ######################################################## 
ed_visit_dx <- get(load('../../rawdata/ED_Visit_Dx_w_Priority.RData'))
colnames(ed_visit_dx) <- tolower(colnames(ed_visit_dx))
ed_visit_dx <- ed_visit_dx %>% mutate(dx_poa_cde = ifelse(dx_poa_cde=="Y", "Y", "N"), 
                                      dx_cde_dotless = tolower(dx_cde_dotless))

# join in condition levels 1-3
ed_visit_dx <- left_join(ed_visit_dx, 
                          all_code_lookup,
                          by=c("dx_cde_dotless"="code", "dx_icd"="version")) %>% 
                mutate(condition_level_3 = ifelse(is.na(condition_level_3)==T, "other or unknown", condition_level_3),
                       condition_level_2 = ifelse(is.na(condition_level_2)==T, "other or unknown", condition_level_2),
                       condition_level_1 = ifelse(is.na(condition_level_1)==T, "other or unknown", condition_level_1))
# approx 650 ICD10's didn't have a mapping - many are due to being "place of occurance" or "external cause"
# could use these grouping categories to fix: https://www.hcup-us.ahrq.gov/db/tools/I10_Formats.TXT
# for now just map to "other"

ed_visit_dx %>% saveRDS('../../cleandata/ed_visit_dx.RDS')

################# ED VISIT INSURANCE ######################################################## 
ed_visit_ins <- get(load('../../rawdata/ED_Visit_Ins.RData'))
colnames(ed_visit_ins) <- tolower(colnames(ed_visit_ins))

## problem: some patients have multiple insurers listed for the same priority
## desired   solution: where there are multiple, choose the insurance that is most frequent
#inner_join(ed_visit_ins,
#           ed_visit_ins %>% 
#             group_by(mrn_mask, enc_mask, insrnc_prio) %>% 
#             summarise(cnt=n()) %>% filter(cnt>1) %>% select(-cnt), 
#           by=c("mrn_mask", "enc_mask", "insrnc_prio"))
# -- 80 dupes : should remove 40
# join in the encounter frequency for each insurance plan name 
# only keep the duplicate record that is the more frequent insurer (in terms of # of encounters)
ed_visit_ins <- left_join(ed_visit_ins, 
                          ed_visit_ins %>% 
                            group_by(insrnc_pln_nme) %>% 
                            summarise(cnt=n_distinct(enc_mask)), 
                          by="insrnc_pln_nme") %>% 
  group_by(enc_mask, mrn_mask, insrnc_prio) %>% 
  mutate(max_cnt=max(cnt)) %>% 
  filter(cnt==max_cnt) %>% 
  select(-cnt, -max_cnt) %>% 
  ungroup()

ed_visit_ins %>% saveRDS('../../cleandata/ed_visit_ins.RDS')

################# ED VISIT LOCATION ######################################################## 
ed_visit_loc <- get(load('../../rawdata/ED_Visit_Loc.RData'))
colnames(ed_visit_loc) <- tolower(colnames(ed_visit_loc))
ed_visit_loc$indate   <- mdy_hm(ed_visit_loc$indate       )
ed_visit_loc$outdate  <- mdy_hm(ed_visit_loc$outdate       )
ed_visit_loc %>% saveRDS('../../cleandata/ed_visit_loc.RDS')

################# ED VISIT PHYSICIAN ######################################################## 
ed_visit_phys <- get(load('../../rawdata/ED_Visit_Phys_iden.RData'))
colnames(ed_visit_phys) <- tolower(colnames(ed_visit_phys))
ed_visit_phys$atn_beg_dtm <- mdy_hm(ed_visit_phys$atn_beg_dtm      )
ed_visit_phys$atn_end_dtm <- mdy_hm(ed_visit_phys$atn_end_dtm      )
ed_visit_phys$atn_line    <- as.numeric(ed_visit_phys$atn_line     )
ed_visit_phys <- ed_visit_phys %>% 
  mutate(dept     = ifelse(dept %in% c("NULL", "Invalid"), "Unknown", dept), 
         division = ifelse(division %in% c("NULL", "Invalid"), "Unknown", division))
ed_visit_phys %>% saveRDS('../../cleandata/ed_visit_phys.RDS')