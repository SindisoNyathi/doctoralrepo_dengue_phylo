# Prelims
require(seqinr)
require(stringr)
require(gdata)
require(lubridate)
require(ips)
require(Biostrings)

# Set working directory
setwd("~/Library/CloudStorage/Box-Box/Sindiso Nyathi's Files/Dengue Evolution/Phylodynamics/denv_add")

# Read in the entire R dataset and the reference
denv2_ref <- seqinr::read.fasta("denv2_ref.fasta", seqtype = "DNA")
all_denv2 <- seqinr::read.fasta("all_denv2.fasta", seqtype = "DNA")

denv2_ref <- readDNAStringSet("denv2_ref.fasta")
all_denv2 <- readDNAStringSet("all_denv2.fasta")


#Subset ref to envelope
denv2_ref_env <- subseq(denv2_ref, 937, 2421)

length(all_denv2)

sequence <- all_denv2[[64]]

pairwise_alignment <- pairwiseAlignment(patter = denv2_ref_env, 
                                        subject = sequence)

330#Iterate through the sequences, align and trim them to envelope only. 
for (sequence in all_denv2) {
  
}