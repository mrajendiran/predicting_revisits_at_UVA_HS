# perform unsupervised clustering of diagnosis codes
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
suppressMessages({
  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(readr)
  library(wordVectors)
  library(magrittr)
  library(Rtsne)
  library(ggthemes)
  library(caTools)
})
source('capstone_ggtheme.R')


# this function imports the raw diagnosis information related to any patient in the analysis table then pivots long to wide and exports to a text file:
# one line per encounter with space delimiting
# this file is read into the "word2vec" algorithm which calculates 200 length vectors for each one of the ICD codes based on 
# its surrounding codes. this is then inserted into the TSNE algorithm to reduce each vector to 2 dimensions and plot each ICD9 code and coloring by the CCS coding
# in the hopes that clear cross-CCS clusters might appear and indicate frequent  comorbidity conditions
icd_clustering <- function(train.table, train_pats, test.table, test_pats, plots=TRUE) {
  # limit to patients in analysis.table
  writeLines('Performing Unsupervised ICD Clustering based only on Training Data...')
  writeLines('\tLimiting to Patients in Analysis Table & Spliting Train/Test...')
  ed_visit_dx.test  <- ed_visit_dx[ed_visit_dx$mrn_mask %in% test_pats, ]
  ed_visit_dx.train <- ed_visit_dx[ed_visit_dx$mrn_mask %in% train_pats, ]
  
  writeLines('\tReranking and Pivoting...')
  # rerank priority for pivoting later
  ed_visit_dx.train2 <- ed_visit_dx.train %>% 
    select(enc_mask, dx_cde_dotless, dx_prio_nbr) %>% 
    group_by(enc_mask) %>% 
    arrange(-desc(dx_prio_nbr), dx_cde_dotless) %>% 
    mutate(dx_prio_nbr = row_number())

  # add seq ID to pivot
  ed_visit_dx.train2$seq <- with(ed_visit_dx.train2, 
                           ave(dx_cde_dotless, 
                               enc_mask, 
                               dx_prio_nbr, 
                               FUN = seq_along))
  
  writeLines('\tPivoting Diagnosis codes...')
  # pivot out so each encounter has a list of diags
  dx_wide <- dcast(enc_mask + seq ~ dx_prio_nbr, 
                   data = ed_visit_dx.train2,
                   value.var = "dx_cde_dotless") %>% select(-seq, -enc_mask)

  # write out to text file
  dx_wide %>% write_delim('../../cleandata/diags.txt', delim = " ", na="", col_names=FALSE)

  writeLines('\tTrain ICDtoVec Embeddings...')
  # import and train word to vec in parallel to get 200 length vectors
  # chose window to be only 5 diags around it 
  model = train_word2vec("../../cleandata/diags.txt",
                         "../../cleandata/diags.bin",
                         vectors = 200,
                         threads = 8,
                         window  = 5,
                         iter    = 10, 
                         force   = TRUE)
  
  # train the TSNE model to take the embedding vectors and decompose into 2 coordinate pairs
  writeLines('\tRun TSNE for Visualization...')
  rtsne_out <- Rtsne(as.matrix(model))
  
  # enrich the tsne file with names etc.
  locations <- as.data.frame(rtsne_out$Y)
  locations$dx_cde_dotless <- rownames(model)
  locations <- left_join(locations, 
                         ed_visit_dx.train %>% 
                           group_by(dx_cde_dotless, dx_nme, condition_level_3) %>% 
                           summarise(freq = n_distinct(enc_mask)),
                         by="dx_cde_dotless") %>% 
                        filter(is.na(freq)==F) %>% arrange(desc(freq))
  
  # Plot the embeddings and color based on CCS 3 grouping
  if (plots==T) {
    ggplot(locations, aes(x=V1, 
                          y=V2, 
                          colour=condition_level_3, 
                          size  = freq,
                          alpha = .95, 
                          line  = F,
                          label = dx_nme)) +
      geom_point() + guides(colour=F, size=F, alpha=F) +
      ggtitle('T-SNE Visualization of ICD9 Embedding Codes\nICD Codes Colored by CCS-3 Grouping') +
      cap4theme()
    ggsave('../../cleandata/tsne.pdf', width=11, height=8.5, units="in")
  }
  
  # RUN K-MEANS CLUSTERING ON RAW EMBEDDING VARIABLES
  # THEN JOIN INTO THE EMBEDDINGS DATAFRAME AND REPLOT
  writeLines('\tK-Means Clustering...')
  km.out=kmeans(as.data.frame(model@.Data)[-1,], 
                25, 
                nstart=25, 
                iter.max = 100)

  clusters <- data.frame(icd = rownames(model)[-1], cluster = km.out$cluster)
  locations <- left_join(locations, 
                         clusters, 
                         by=c("dx_cde_dotless"="icd")) %>% 
                mutate(cluster = paste("Clstr_", cluster, sep=""))
  
  # Same plot as above but now color by unsupervised k-means cluster
  if (plots==T) {
    ggplot(locations, aes(x=V1, 
                          y=V2, 
                          colour=cluster, 
                          size  = freq,
                          alpha = .95, 
                          line  = F,
                          label = dx_nme)) +
      geom_point() + guides(colour=F, size=F, alpha=F) +
      ggtitle('T-SNE Visualization of ICD9 Embedding Codes\n25-K-Means Cluster on Raw Embeddings') +
      cap4theme()
    ggsave('../../cleandata/tsne.w.clusters.pdf', width=11, height=8.5, units="in")
  }
  
  writeLines('\tJoining Clusters Back to TRAINING Table...')
  clusters   <- clusters %>% mutate(cluster = paste("Clstr_", cluster, sep="")) 
  conditions <- left_join(ed_visit_dx.train, 
                          clusters, 
                          by=c("dx_cde_dotless"="icd")) %>% 
                      mutate(flg=1) %>% 
                      filter(is.na(cluster)==F)
  train.w.dx <- dcast(conditions, enc_mask+mrn_mask~cluster, value.var = 'flg', fun.aggregate = sum, fill=0)
  train.w.dx <- left_join(train.table, train.w.dx, by = c("enc_mask", "mrn_mask"))
  
  
  writeLines('\tJoining Clusters Back to TESTING Table...')
  conditions.train <- left_join(ed_visit_dx.train, 
                                clusters, 
                                by=c("dx_cde_dotless"="icd")) %>% 
                      mutate(flg=1) %>% 
                      filter(is.na(cluster)==F)
  test.w.dx <- dcast(conditions.train, enc_mask+mrn_mask~cluster, value.var = 'flg', fun.aggregate = sum, fill=0)
  test.w.dx <- left_join(test.table, test.w.dx, by = c("enc_mask", "mrn_mask"))
  
  results <- list("train.w.dx"=train.w.dx, "test.w.dx"=test.w.dx) 
  return(results)
}
  
# test function
# at <- readRDS('../../cleandata/ed_visit.RDS') %>% select(mrn_mask, enc_mask) %>% distinct()
# ed_visit_dx    <- readRDS('../../cleandata/ed_visit_dx.RDS')
# 
# pat_ids    = unique(analysis.table$mrn_mask)
# split      = sample.split(pat_ids, SplitRatio = 4/5)
# train_pats = pat_ids[split]
# test_pats  = pat_ids[!split]
# train.table = analysis.table[analysis.table$mrn_mask %in% train_pats, ]
# test.table  = analysis.table[analysis.table$mrn_mask %in% test_pats,  ]

# clustered_tables <- icd_clustering(train.table, train_pats, test.table, test_pats)
# train.table <- clustered_tables$train.w.dx
# test.table  <- clustered_tables$test.w.dx
