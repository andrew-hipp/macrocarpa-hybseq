## read data for hybSeq analyses
##  v 1, AH 2018 10 09, remix MG 2018 10 11

library(adegenet)
library(ape)
library(parallel)
library(pegas)

source('../scripts/DNAbin2str.R')

#nCores = 2
readDNA = TRUE
cleanDNAnames = TRUE

##if(readDNA) {
  #dna.snps <- lapply(dir('../data/FASTA.2018-10-11/', full = T), fasta2DNAbin, snpOnly = TRUE) #took out mc.cores=nCores and changed mclapply to lapply
  #names(dna.snps) <- dir('../data/FASTA.2018-10-11/')
#}

if(cleanDNAnames) {
  for(i in names(dna.snps)) {
    row.names(dna.snps[[i]]) <-
      strsplit(row.names(dna.snps[[i]]), "_", fixed = T) %>%
      sapply(., '[', 3)
    row.names(dna.snps[[i]])[row.names(dna.snps[[i]]) %in% c('all', 'merged', 'transcript')] <-
      paste('Q.rubra',
            row.names(dna.snps[[i]])[row.names(dna.snps[[i]]) %in% c('all', 'merged', 'transcript')],
            sep = '-')
    row.names(dna.snps[[i]]) <- gsub('MOR-1|MOR1|MIR-1', 'MOR-001',
                                      row.names(dna.snps[[i]]))
    }
}

dna.all <- do.call('cbind', c(dna.snps, fill.with.gaps = TRUE))


dat.meta.ext <- read.delim('../DATA/dna.database/OAKS_MOR_DNA_DATABASE-2017.tsv',
                            as.is = T, row.names = 1)
dat.meta.ext$sp <- sapply(strsplit(dat.meta.ext$SPECIES, " ", fixed = T), function(x) x[2])

dat.meta.spm <- read.delim('../DATA/dna.database/OAKS_America_Accessions_Database_excel_2017-05-15.tsv', as.is = T)
dat.meta.ext$lat <- dat.meta.spm[match(dat.meta.ext$SPECIMEN.CODE, dat.meta.spm$Specimen.CODE), 'latitude.orig']
dat.meta.ext$long <- dat.meta.spm[match(dat.meta.ext$SPECIMEN.CODE, dat.meta.spm$Specimen.CODE), 'longitude.orig']

dat.mapping <-  dat.meta.ext[row.names(dna.all), c('long', 'lat', 'sp')] %>%
  as.data.frame
names(dat.mapping) <- c('long', 'lat', 'Species')
for(i in c('long', 'lat')) {
  dat.mapping[[i]] <- as.numeric(dat.mapping[[i]])
}
dat.mapping$long <- -abs(dat.mapping$long) # some positives
dat.mapping$Species[grep('Quercus', dat.mapping$Species, invert = T)] <-
  paste('Quercus', dat.mapping$Species[grep('Quercus', dat.mapping$Species, invert = T)])
dat.mapping <- dat.mapping[dat.mapping$Species != "Quercus NA", ]

## write structure file
row.names(dna.all) <-
  paste(dat.meta.ext[row.names(dna.all), 'sp'],
        row.names(dna.all),
        sep = '|'
        )

inds.struct <- grep('macro|alba|stellata|bicolor|muehl', row.names(dna.all), value = T)
a = DNAbin2str(list(dna.all[grep('macro|alba|stellata|bicolor|muehl', row.names(dna.all)), ]),
                file='hybseq.v3.2018-10-10')

## making some changes that matter to me
