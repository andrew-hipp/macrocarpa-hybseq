## clean up FASTA names so that they match current dets
## AH 2019-11-07
## NOTE: this is not the proper way to do things...
##      ... an expedient for current analysis...
##      ... before full analyses, update specimen database and work from there

library(ape)

readDNA <- TRUE
writeDNA <- TRUE
dnaDir <- '../DATA/FASTA.RENAMED.2019-11-07'

dat.meta <- read.delim('../DATA/dna.database/OAKS_MOR_DNA_DATABASE-2017.tsv',
                        as.is = T, row.names = 1)
dat.meta$spNew <- sapply(strsplit(dat.meta$SPECIES, ' ', fixed = T), '[', 2)

if(readDNA & !exists('dna.fas'))
  dna.fas <- lapply(dir('../DATA/FASTA.2018-10-11', full = T), function(x) {
    message(paste('just read', x))
    temp <- read.dna(x, format = 'fasta')
    row.names(temp) <-
      sapply(strsplit(row.names(temp), '_', fixed = T), tail, 1)
    row.names(temp) <-
      gsub('MOR-1|MOR1|MIR-1', 'MOR-001', row.names(temp))
    row.names(temp)[row.names(temp) %in% c('all','merged','transcript')] <-
      paste('Qu_rubra',
            row.names(temp)[row.names(temp) %in% c('all','merged','transcript')],
            sep = '_')
    row.names(temp)[row.names(temp) %in% row.names(dat.meta)] <-
      paste(dat.meta[row.names(temp)[row.names(temp) %in% row.names(dat.meta)], 'spNew'],
            row.names(temp)[row.names(temp) %in% row.names(dat.meta)],
            sep = '_')
    return(temp)
  })

names(dna.fas) <- dir('../DATA/FASTA.2018-10-11')

if(writeDNA) {
  dir.create(dnaDir)
  for(i in names(dna.fas)) {
    write.dna(dna.fas[[i]], paste(dnaDir,'/relabelled', i, sep = ''))
  }
}
