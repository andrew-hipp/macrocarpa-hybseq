library(ape)
library(phytools)
library(magrittr)

tr <- read.tree('../DATA/trees/RAX.2018-10-15/RAxML_bipartitions.macrocarpa.hybseq.2018-10-15.tre')
tr <- root(tr, grep('rubra', tr$tip.label)) %>%
      ladderize %>%
      drop.tip(tip = grep('rubra', tr$tip.label))
