require(stringr)
# Create correct names metadata file from fasta file. 
#all_seq <- seqinr::read.fasta('DENV4/denv3_complete_cohort_n354_formatted_aligned_curated_norecc.fasta')
typing_data <- read.csv('DENV4/d4_typingdata.csv')
metadata <- read.csv("DENV4/final_seq_metadata_denv4.csv")
typing_data$RealID <- NA
#Non study sequences
for (i in 1:198){
  realid <- strsplit(typing_data$name[i], '\\.')
  realid <- paste(realid[[1]][1],  '.1', sep = '')
  typing_data$RealID[i] <- realid
}

#Study sequences
for (i in 199:nrow(typing_data)){
  realid <- substring(typing_data$name[i], 1, 7)
  typing_data$RealID[i] <- realid
}

# Real ID for metadata. 
metadata$RealID <- NA
for (i in 1:nrow(metadata)) {
  id <- metadata$ID[i]
  id_split <- strsplit(id, '/')
  realid <- id_split[[1]][1]
  metadata$RealID[i] <- realid
}

# create metadata
seq_metadata_denv <- data.frame(matrix(nrow = nrow(typing_data), ncol = 5))
colnames(seq_metadata_denv) <- c('ID', 'Date', 'Location', 'Genotype', 'Site')

for (i in 1:nrow(typing_data)){
  
  this_seq_id <- typing_data$RealID[i]
  if (this_seq_id %in%  metadata$RealID) {
    
    id <- metadata$ID[metadata$RealID == this_seq_id]
    location <- metadata$Location[metadata$RealID == this_seq_id]
    date <- metadata$Date[metadata$RealID == this_seq_id]
    genotype <- typing_data$type[typing_data$RealID == this_seq_id]
    
    seq_metadata_denv[i, 1] <- id
    seq_metadata_denv[i, 2] <- date
    seq_metadata_denv[i, 3] <- location
    seq_metadata_denv[i, 4] <- genotype
    
  }
}

#Cheat. 
seq_metadata_denv[354, 4] <- 'DENV-3 Genotype III'

write.csv(seq_metadata_denv, file='DENV4/final_seq_metatyping_denv4.csv')
