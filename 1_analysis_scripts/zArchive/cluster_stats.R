setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(reshape2)
  library(lubridate)
})

## load datasets/functions
clusters <- readRDS('../../cleandata/clusters.RDS') # on collab
ed_visit_dx   <- readRDS('../../cleandata/ed_visit_dx.RDS')
ed_visit      <- readRDS('../../cleandata/ed_visit.RDS')
source('2a_ed_functions.R')

############## cluster mapping to diagnosis codes/names for every patient encounter ############## 

# diagnosis code, cluster, diagnosis name, and condition level 3 classification
cluster_stats <- left_join(clusters, ed_visit_dx %>%
                             select(dx_cde_dotless, dx_nme, condition_level_3), 
                           by="dx_cde_dotless") %>% 
                 distinct(dx_cde_dotless, .keep_all = TRUE)

# number of dx codes in each cluster
cluster_stats %>% 
  group_by(cluster) %>% 
  summarise(freq = n_distinct(dx_cde_dotless)) -> cluster_counts

############## cluster mapping to diagnosis codes/names for patients revisiting in 30 days ############## 

# diagnosis code, cluster, diagnosis name, and condition level 3 classification
ed_visit %>% 
  mutate(yr = year(ed_beg)) %>% 
  select(enc_mask, mrn_mask, yr) %>% 
  distinct() %>% 
  add_num_revisits(ed_visit, plots=F) %>% 
  filter(has.revisit == 1) %>% 
  left_join(ed_visit_dx, by="enc_mask") %>% 
  select(dx_cde_dotless) %>% 
  left_join(clusters, by="dx_cde_dotless") %>% 
  distinct(dx_cde_dotless, .keep_all = TRUE) %>% 
  filter(cluster < 26) %>% 
  left_join(ed_visit_dx %>%
              select(dx_cde_dotless, dx_nme, condition_level_3), 
            by="dx_cde_dotless") %>% 
  distinct(dx_cde_dotless, .keep_all = TRUE) -> freq_cluster_stats

# number of dx codes in each cluster
freq_cluster_stats %>% 
  group_by(cluster) %>% 
  summarise(freq = n_distinct(dx_cde_dotless)) -> freq_cluster_counts

  
