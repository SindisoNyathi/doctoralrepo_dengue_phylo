# Introduction
This repository contains R, Python and Bash code that comprises the bioinformatic and statistical workflows used in Chapters 1 and 2 of my dissertation. These chapters have been published in [Nature Communications](https://www.nature.com/articles/s41467-024-51018-0) and [Virus Evolution](https://academic.oup.com/ve/article/11/1/veae116/7934600), and the abstracts are included below. The complete worksflows detailed below are complicated, but typical phylogenetics/phylodynamics workflows that involve accessing sequences using specific inclusion and exclusion criteria, processing them using various Bayesian and maximum likelihood tools, and summarising and reporting the results. I will only describe the tools and scripts I used in this workflow as there are other excellent resources that go over the theory. 

# Detailed Analytic Workflow

## 1. Compilation of sequencing database

The sequence database used workflows like this depends on the goal of the analysis. The goal of my analysis is to investigate genetic diversity of dengue in Kenya and Africa, and also determine how dengue is making it's way into the communities I studied using phylogeographic approaches. My sequence database included 2 kinds of sequences - newly collected sequences and secondary data sequences, i.e. existing sequences collected by other teams in the past. Study details for the collection of dengue virus sequencing materials from our study sites and laboratory processing I conducted on the sequences are included in the manuscripts linked above. FASTQ files retrieved following illumina sequencing using a custom tool provided by (basespace)[bash/basespace_download.sh] and are processed using [this](bash/denv_singlesample_analysis.sh) workflow that makes use of arrays, based on similar work by [Grubaugh lab](https://grubaughlab.com/), [Vogels Lab](https://vogelslab.com/), [Bennet Lab](https://www.calacademy.org/staff-member/shannon-bennett-phd) and other dengue research teams. Sequences are manually inspected and QC evaluated based on the outputs of commands from the SAMTOOLS, PICARD, BOWTIE and IVAR packages. 

To retrieve sequences collected from other groups, I used the (NCBIVirus)[https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/] website. NCBI has useful APIs that allow one to download larger sequence datasets that can also be used in (this)[r/genbank_sequencedownload.R] way. Using this API of the web app they have you cna select sequences based on your inclusion and exclusion criteria, with the goal of creating a comprehensive but unbiased sequencing dataset. My inclusion criteria included samples surce (human, and not vector or environemtal), dates (up to 2023) and some length requires (at least envelope gene for dengue, approx. 1400bp). These downloaded sequences were concatenated with my new sequences. One interesting step I had to include was a data reduction step. Since i am primarily interested in dengue in Africa, the thousands of sequences from places like Singapore and Brazil were very redundant. As such, I used this (script)[https://github.com/SindisoNyathi/doctoralrepo_dengue_phylo/blob/main/r/reduce_to_representative_sequences.R] to reduce country level sub-datasets to more manageable sizes, based on the original data size. Files were reduced by a factor, $r$ that depends on initial country datasize $N_c$, as follows, for each $c \in C$ were $C = \\{Brazil, Colombia, Thailand, Vietnam, . . .\\}$, $Nc$ is the number of sequences from that location included in the analysis. Define $r$ as follows:

$$
r =
\begin{cases}
1 & \text{if } N_c \leq 25, \\
5 & \text{if } 25 < N_c \leq 100, \\
10 & \text{if } 100 < N_c \leq 500.
\end{cases}
$$

And calculate the new country-specific dataset size:

$$
N_c' = \frac{N_c}{r}.
$$

For each country the initial $Nc$ sequences are hierarchically clustered into $N_c'$ clusters using a distance based measure, and a random sequence is selected from each cluster. This gives a new dataset with a more computationally manageable number of sequences that can be used. 

## 2. Alignment of sequencing and alignemnt quality control

The resulting sequence dataset was further processed using various tools to clean the sequence names and metadata (such as country and date). Sequences can be aligned using any one of several tools. Many newer tools cna handle tens of thousands of sequences. I used (this)[bash/reduce_genbank_sequences.sh] script to align the cleaned sequences in mafft, which is my tool of choice. Following the alignement by mafft teh alignemnt file is manually inspected and curated in Aliview. For this workflow this included removing any stop codons using (this])(r/find_stop_codon.R) script in order to be able to run tools such as MEME, FUBAR etc to identify signatures of selection. 

## 3. Iterative maximum likelihood tree building

As with treealignment the maximum likelihood tree building step can be conducted with multiple tools depending on ones needs. I have used (RAXmL)[https://cme.h-its.org/exelixis/web/software/raxml/] here. For timed tree building i reccomend (TreeTime)[https://treetime.readthedocs.io/en/latest/tutorials/timetree.html] which is very efficient, making it ideal for massive sequence datasets. New tools are being developed everyday though, so these favorites of mine may become obsolete quickly. This analytic step has 3 components. The first is making the ML tree and visualizing it in R or to identify any divergent sequences that might be excluded. Tree visualization is a fun exercise. You can use GUI type software such as IcyTree and FigTree or some truly phenominal packages in R usch as (ggtree)[https://bioconductor.org/packages/release/bioc/html/ggtree.html] by Guangchuang Yu  and (phytools)[http://www.phytools.org/] by Liam Revell. Both have written extensively about comparative phylogenetics methodsand are generally good names to follow on academic social media. If you need somewhere to start take a look as some of my iterations visualizing phylogenetic trees (here)[r/phylo_visualization.R], (here)[r/plot_ml_tree.R] or (here)[r/plot_bayesian_trees.R]. The goal here is to make sure the tree makes sense. For example if a single outlier tip or clade distorts the shape so other tips are not visually resolved, one would think about why the sequence is so divergent and consider excluding it. The ML tree can also be used to investigate temporal signal in (TempEst)[http://tree.bio.ed.ac.uk/software/tempest/]. Again, sequneces that seem to be outliers might be excluded. One final group of sequences that might be excluded here are potential recombinants. These are identified using Darren Martin's (RDP4)[https://academic.oup.com/ve/article/1/1/vev003/2568683]. I went through these steps iteratively mutliple times - make the tree, evaluate temporal signal, remove outliers and potential recombinants, until I had a reasonable tree and temporal signal with no evident issues. unfortunately the exact threshold were this occurs is subjective and i went back and forth with reviewers about it. 

## 4. Bayesian tree building and phylogeography. 


 
