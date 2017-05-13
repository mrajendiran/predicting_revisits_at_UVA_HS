# functions to clean data and create features for ED Visit table
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(data.table)
  library(stringr)
  library(dplyr)
})

################## :: RESPONSE VARIABLE :: ##################
# add revisits flag 
add_num_revisits <- function(analysis.table, ed_visit, plots=F) {
  # identify followups within 0-30 days
  followups <- left_join(ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end),
                         ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end),
                         by="mrn_mask") %>% 
    mutate(timelapse = as.double(difftime(ed_beg.y, ed_end.x,units="days"))) %>% 
    filter(enc_mask.x!=enc_mask.y & timelapse >= 0 & timelapse <= 30) %>% 
    group_by(mrn_mask, enc_mask.x) %>%
    mutate(followup.rnk = dense_rank(ed_beg.y)) %>% ungroup()
  
  if (plots==T) {hist(followups[followups$followup.rnk==1,]$timelapse, breaks=30, main="# Days Till First Followup Visit", xlab="Days")}
  
  # roll up the to the patient encounter level
  followups.roll <- followups %>% 
    mutate(enc_mask = enc_mask.x) %>% 
    group_by(enc_mask, mrn_mask) %>% 
    summarise(days.till.first.return = max(ifelse(followup.rnk==1, timelapse, 0)), 
              num.revists = max(followup.rnk))
  
  # join followups back to the analysis table - if no followup then # revisits = 1
  analysis.table <- left_join(analysis.table, 
                              ed_visit %>% select(enc_mask, mrn_mask, raw_mort), 
                              by=c("enc_mask", "mrn_mask"))
  
  analysis.table <- left_join(analysis.table,
                              followups.roll, 
                              by=c("enc_mask", "mrn_mask")) %>% 
    mutate(num.revists = ifelse(is.na(num.revists), 0, num.revists), 
           has.revisit = ifelse(num.revists>0 | raw_mort>0, 1, 0))
  
  # what percent of patient encounters have a revisit within 30 days?
  if (plots==T) { 
    print(paste("What Percent of Patients have at least one revisit within 30 days: ", mean(analysis.table$has.revisit), sep=""))
    # for the patients who DO come back visit how many times on average?
    print(paste("For those patients who DO come back, they have: ", mean(analysis.table[analysis.table$num.revists>0,]$num.revists), " revisits on average", sep=""))
    hist(analysis.table[analysis.table$num.revists>0,]$num.revists, main = "# Revisits for Patients who DO Return < 30 Days", xlab="# Revisits")
    hist(followups[followups$followup.rnk==1,]$timelapse, breaks=30, main="# Days Till First Followup Visit", xlab="Days")
  }
  return(analysis.table)
}


cleanEDvars <- function(analysis.table, ed_visit) {
  # this function: 
  # adds variables that need no or minimal cleanup
  # PCP: presence of a primary care physician within uva system
  # acuity: acuity at admission
  # resp: respiration, 0s changed to NA
  # bsa: body surface area
  # bp_syst, bp_dias: systolic and diastolic blood pressure
  # pulse: pulse
  analysis.table <- left_join(analysis.table,
                              ed_visit %>% 
                                mutate(acuity = 
                                         ifelse(acuity=="Immediate",   1, 
                                         ifelse(acuity=="Emergent",    2, 
                                         ifelse(acuity=="Urgent",      3, 
                                         ifelse(acuity=="Less Urgent", 4, 
                                         ifelse(acuity=="Non-Urgent",  5, 6))))), 
                                  pcp = as.factor(pcp)) %>% 
                                select(enc_mask, mrn_mask, acuity, pcp), 
                              by=c("enc_mask", "mrn_mask"))
  
  #clean anything that just needs 0s changed to NA
  ed_visit[ed_visit$resp == 0, 'resp']      <- NA
  ed_visit[ed_visit$bsa == 0,'bsa']         <- NA #TODO: verify that these 0s are missing data
  ed_visit[ed_visit$bp_syst == 0,'bp_syst'] <- NA
  ed_visit[ed_visit$bp_dias == 0,'bp_dias'] <- NA
  ed_visit[ed_visit$pulse == 0,'pulse']     <- NA
  
  #chop off temp outliers
  ed_visit[ed_visit$temp < 95 | ed_visit$temp > 110, 'temp'] <- NA
  
  #recombine
  analysis.table <- merge(analysis.table, ed_visit[,c('enc_mask','resp','bsa','bp_syst','bp_dias','pulse','temp')], by = 'enc_mask')
  return(analysis.table)
}

bodyVarsCalc <- function(analysis.table, ed_visit) {
  # this function cleans height, weight, and bmi with reconstruction from existing data
  # change weight to lbs; height from BMI or 0 if wt&bmi missing
  body.stats <- ed_visit %>%
    mutate(wt = wt / 16) %>%
    mutate(ht = ifelse(ht == 'Unknown',
                       ifelse(wt == 0 | bmi == 0, 0, sqrt((wt / bmi) * 703)),
                       ht %>% as.character %>% strsplit("'|\"") %>%
                         sapply(function(x) {
                           12 * as.numeric(x[1]) + as.numeric(x[2])
                         })
    ))
  # impute height, weight, bmi from most recent encounter with data
  body.imputes <- left_join(body.stats, 
                            body.stats, by=c("mrn_mask")) %>% 
    mutate(timelapse = as.double(difftime(ed_end.x, ed_beg.y, units="days"))) %>% 
    filter(enc_mask.x!=enc_mask.y & timelapse > 0 & (ht.x == 0 | wt.x == 0 | bmi.x == 0)) %>% 
    group_by(enc_mask.x) %>% 
    filter(timelapse == min(timelapse)) %>% 
    mutate(ht.x = ht.y) %>% 
    mutate(wt.x = wt.y) %>% 
    mutate(bmi.x = bmi.y) %>% 
    ungroup() %>% 
    mutate(enc_mask = enc_mask.x) %>% 
    select(mrn_mask, enc_mask, ht.x, wt.x, bmi.x)
  
  # combine body.stats & body.imputes
  stats.imputes <- left_join(body.stats, body.imputes, by=c("enc_mask", "mrn_mask")) %>% 
    mutate(ht = ifelse(ht == 0 & !is.na(ht.x), ht.x, ht)) %>% 
    mutate(wt = ifelse(wt == 0 & !is.na(wt.x), wt.x, wt)) %>% 
    mutate(bmi = ifelse(bmi == 0 & !is.na(bmi.x), bmi.x, bmi)) %>% 
    select(mrn_mask, enc_mask, ht, wt, bmi)
  
  # return imputed values to analysis.table
  analysis.table <- left_join(analysis.table, 
                              stats.imputes, by=c("enc_mask")) %>% 
    mutate(mrn_mask = mrn_mask.x) %>% 
    select(-mrn_mask.x, -mrn_mask.y)
  return(analysis.table)
}

## Lookback Statistics
add_previous_visits <- function(analysis.table, ed_visit) {
  past_visits <- left_join(ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end),
                           ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end),
                           by="mrn_mask") %>% 
    mutate(timelapse = as.double(difftime(ed_end.x, ed_beg.y, units="days"))) %>% 
    filter(enc_mask.x!=enc_mask.y & timelapse > 0 & timelapse <= 365) %>% 
    mutate(visits_in_last_year    = ifelse(timelapse > 0 & timelapse <= 365,  1, 0), 
           enc_mask = enc_mask.x) %>% 
    select(mrn_mask, enc_mask, visits_in_last_year) %>% 
    group_by(mrn_mask, enc_mask) %>%
    summarise(visits_in_last_year    = sum(visits_in_last_year   )) %>% ungroup()
  
  # join followups back to the analysis table - if no followup then # revisits = 1
  analysis.table <- left_join(analysis.table, 
                              past_visits, 
                              by=c("enc_mask", "mrn_mask")) %>% 
    mutate(visits_in_last_year    = ifelse(is.na(visits_in_last_year   )==T, 0, visits_in_last_year   ))
  return(analysis.table)
}

# group together arrival and disposition variables
groupArrivalDisposition = function(outputTable, edTable) {
  for(i in 1:length(edTable$enc_disposition)) {
    if (grepl('Planned', edTable[i,  "enc_disposition"])) {
      edTable[i,  "enc_disposition"] = 'Planned readmit'
    } else if (grepl('Discharge', edTable[i,  "enc_disposition"])) {
      edTable[i,  "enc_disposition"] = 'Discharged to other care'
    } else if (grepl('Psyc', edTable[i,  "enc_disposition"])) {
      edTable[i,  "enc_disposition"] = 'Psych facility'
    } else if (grepl('Home', edTable[i,  "enc_disposition"])) {
      edTable[i,  "enc_disposition"] = 'Home'
    } else if (edTable[i,  "enc_disposition"] == 'Rehab Facility (not TCH)' | 
               edTable[i,  "enc_disposition"] == 'Short Term Hospital' |
               edTable[i,  "enc_disposition"] == 'Skilled Nursing Facility') {
    } 
    else {
      edTable[i,  "enc_disposition"] = 'Other'
    }
  }
  for(i in 1:length(edTable$ed_disposition)) {
    if (edTable[i,  "ed_disposition"] == 'AMA' | edTable[i,  "ed_disposition"] == 'Eloped'
        | edTable[i,  "ed_disposition"] == 'Left Without Being Discharged' | edTable[i,  "ed_disposition"] == 'Left Without Being Seen after Triage'
        | edTable[i,  "ed_disposition"] == 'Left Without Being Seen before Triage') {
      edTable[i,  "ed_disposition"] = 'Left before treatment'
    } else if (edTable[i,  "ed_disposition"] == 'Sent to Cath Lab' | edTable[i,  "ed_disposition"] == 'Sent to L&D'
               | edTable[i,  "ed_disposition"] == 'Sent to OR' | edTable[i,  "ed_disposition"] == 'Transferred to Another Facility') {
      edTable[i,  "ed_disposition"] = 'Transferred'
    } else if (edTable[i,  "ed_disposition"] == 'Admitted' | edTable[i,  "ed_disposition"] == 'Deceased'
               | edTable[i,  "ed_disposition"] == 'Shelter for Help and Emergency'
               | edTable[i,  "ed_disposition"] == 'Discharged') {
    } 
    else {
      edTable[i,  "ed_disposition"] = 'Other'
    }
  }
  for(i in 1:length(edTable$arriv_mode)) {
    if(edTable[i,  "arriv_mode"] != 'AIRCARE' & edTable[i,  "arriv_mode"] != 'WALK-IN' 
       & edTable[i,  "arriv_mode"] != 'CVILLE/ALBEMARLE'
       & edTable[i,  "arriv_mode"] != 'PEGASUS'
       & edTable[i,  "arriv_mode"] != 'ALB CO FIRE AND RESCUE') {
      edTable[i,  "arriv_mode"] = 'other area'
    }
  }
  outputTable$arriv_mode = edTable$arriv_mode
  outputTable$ed_disposition = edTable$ed_disposition
  outputTable$enc_disposition = edTable$enc_disposition
  return(outputTable)
}

# add proportionality of urgency categories in past visits
add_past_urgencies <- function(analysis.df, ed_visit){
  ed_visit$acuity <- as.factor(ed_visit$acuity)
  
  patients <- left_join(ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end, acuity),
                        ed_visit %>% select(enc_mask, mrn_mask, ed_beg, ed_end, acuity),
                        by="mrn_mask") %>%
    mutate(timelapse = as.double(difftime(ed_end.x, ed_beg.y, units="days"))) %>% 
    filter(enc_mask.x!=enc_mask.y & timelapse > 0) %>%
    mutate(priorEmergent   = (acuity.y=='Emergent'),
           priorImmediate  = (acuity.y=='Immediate'),
           priorLessUrgent = (acuity.y=='Less Urgent'),
           priorNonUrgent  = (acuity.y=='Non-Urgent'),
           priorUnknown    = (acuity.y=='Unknown'),
           priorUrgent     = (acuity.y=='Urgent'),
           rcount = 1,
           enc_mask = enc_mask.x) %>%
    select(mrn_mask, enc_mask, priorEmergent, priorImmediate, priorLessUrgent, priorNonUrgent, priorUnknown, priorUrgent, rcount) %>%
    group_by(mrn_mask, enc_mask) %>%
    summarise(priorEmergent   = sum(as.numeric(priorEmergent))   / sum(rcount),
              priorImmediate  = sum(as.numeric(priorImmediate))  / sum(rcount),
              priorLessUrgent = sum(as.numeric(priorLessUrgent)) / sum(rcount),
              priorNonUrgent  = sum(as.numeric(priorNonUrgent))  / sum(rcount),
              priorUnknown    = sum(as.numeric(priorUnknown))    / sum(rcount),
              priorUrgent     = sum(as.numeric(priorUrgent))     / sum(rcount)) %>% ungroup()

  analysis.df <- left_join(analysis.df, 
                           patients, 
                           by=c("enc_mask", "mrn_mask")) %>%
    mutate(priorEmergent    = ifelse(is.na(priorEmergent  )==T, 0, priorEmergent   ),
           priorImmediate   = ifelse(is.na(priorImmediate )==T, 0, priorImmediate  ),
           priorLessUrgent  = ifelse(is.na(priorLessUrgent)==T, 0, priorLessUrgent ),
           priorNonUrgent   = ifelse(is.na(priorNonUrgent )==T, 0, priorNonUrgent  ),
           priorUnknown     = ifelse(is.na(priorUnknown   )==T, 0, priorUnknown    ),
           priorUrgent      = ifelse(is.na(priorUrgent    )==T, 0, priorUrgent     ))
  return(analysis.df)
}


duration_stats <- function(analysis.table, ed_visit) {
    # rank each patients' visits
    lu <- ed_visit %>% #filter(mrn_mask==11) %>% #filter(mrn_mask %in% c(39189, 39194)) %>% 
      select(enc_mask, mrn_mask, ed_beg, ed_end) %>% 
      group_by(mrn_mask) %>% 
      mutate(rnk = dense_rank(ed_beg))
    
    # calculate duration between each 2 consecutive visits
    duration_lookup <- left_join(lu, lu, by="mrn_mask") %>% 
      filter(rnk.x-rnk.y==1) %>% 
      mutate(timelapse=as.integer(ed_beg.x-ed_beg.y)) %>% 
      select(enc_mask = enc_mask.x, mrn_mask, rnk=rnk.x, timelapse, ed_end = ed_end.y)
    
    # function that inputs an ordered array and returns the slope
    rate_change <- function(arr) {
      x_arr <- seq(1, length(arr))
      coef  <- coef(lm(arr ~ x_arr))[2]
      return(coef)
    }
  
    na.zero <- function (x) {
      x[is.na(x)] <- 0
      return(x)
    }
    
    # for all visits before this one, calculate the average durations
    # standard deviation of durations and rate of change
    lapse_stats <- left_join(lu, 
                           duration_lookup, 
                           by="mrn_mask") %>% 
    mutate(date_diff = as.integer(as.Date(ed_end.y) - as.Date(ed_beg))) %>% 
    filter(rnk.x>=rnk.y & date_diff >= -365) %>% 
    group_by(mrn_mask, enc_mask = enc_mask.x) %>% 
    arrange(rnk.x) %>% # sort by rank so rate of change array will be ordered right
    summarise(avg.between = na.zero(mean(timelapse, na.rm=T)), 
              sd.between  = na.zero(sd(timelapse, na.rm=T)), 
              rate        = na.zero(rate_change(timelapse))) 
    
    # join stats back to main table
    analysis.table <- left_join(analysis.table, lapse_stats, 
                                by=c("mrn_mask"="mrn_mask", "enc_mask"="enc_mask")) %>% 
                      mutate(avg.between = na.zero(avg.between), 
                             sd.between  = na.zero(sd.between), 
                             rate        = na.zero(rate)) 
    return(analysis.table)
}



#testing code
# ed_visit     <- readRDS('../../cleandata/ed_visit.RDS')
# analysis.table <- ed_visit %>% select(mrn_mask, enc_mask) %>% distinct()
# dur.test <- duration_stats(analysis.table, ed_visit)
# ed_visit %>% filter(mrn_mask==11) %>% select(mrn_mask, enc_mask, ed_beg, ed_end) %>% arrange(ed_beg)

# test_cleanEDvars  <- cleanEDvars(analysis.table,  ed_visit)
# test_rawBodyVars  <- rawBodyVars(analysis.table,  ed_visit)
# test_bodyVarsCalc <- bodyVarsCalc(analysis.table, ed_visit)
# test_add_num_revisits <- add_num_revisits(analysis.table, ed_visit)
# test_add_previous_visits <- add_previous_visits(analysis.table, ed_visit)
# test_arriv_disp = groupArrivalDisposition(ed_visit, ed_visit)