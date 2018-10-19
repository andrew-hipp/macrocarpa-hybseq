library(ape)
library(phytools)
library(magrittr)
source('../SCRIPTS/scrubOaks.R')

tr <- read.tree('../DATA/trees/RAX.2018-10-15/RAxML_bipartitions.macrocarpa.hybseq.2018-10-15.tre')
tr <- root(tr, grep('rubra', tr$tip.label)) %>%
      ladderize %>%
      drop.tip(tip = grep('rubra', tr$tip.label))
tr$tipsCleaned <- scrubOaks(tr$tip.label, 'cleaned')
tr$tip.label <- scrubOaks(tr$tip.label, 'renamed')
tr.dat <- dat.meta.ext[tr$tipsCleaned, c('lat', 'long')]
tr.dat$long <- -abs(as.numeric(tr.dat$long))
row.names(tr.dat) <- gsub(" ", "_", tr$tip.label) # necessary only for phylo.to.map
tr.dat <- tr.dat[!is.na(tr.dat$long) | is.na(tr.dat$lat), ]
tr.dat <- as.matrix(tr.dat)
tr.dat <- apply(tr.dat, 1:2, as.numeric)
tr.mac <- drop.tip(tr, grep('macrocarpa', tr$tip.label, invert = T))

pdf('../out/phylo.to.map.pdf')
x = phylo.to.map(drop.tip(tr.mac, setdiff(tr$tip.label, row.names(tr.dat))),
              tr.dat[grep('macrocarpa', row.names(tr.dat)), ],
              xlim = c(-120, -70), ylim = c(30, 55),
              fsize = 0.00000001)
dev.off()
