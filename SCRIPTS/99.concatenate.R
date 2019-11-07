## a script to concatenate genes
## 2019-11-07

library(ape)
readDNA <- FALSE
writeDNA <- TRUE
dna.out.name <- 'myFavPartition.phy'

if(readDNA) {
  dna.fas.all <- lapply(dir('../DATA/FASTA.RENAMED.2019-11-07', full = T),
                        function(x) {
                          message(paste('reading', x))
                          read.dna(x)
                        })
  names(dna.fas.all) <- dir('../DATA/FASTA.RENAMED.2019-11-07')
}
## change these when you want to concatenate new genes
genesToConcatenate <- c(
  'T0766030.2',
  'T0633270.2',
  'T0564850.2'
)

## This does the dirty work:
whichOnes <- sapply(genesToConcatenate,
                    grep, x = names(dna.fas.all),
                    value = TRUE)
whichOnes <- unique(unlist(whichOnes))

dna.concat <- do.call('cbind.DNAbin',
                      c(dna.fas.all[whichOnes], fill.with.gaps = TRUE)
                    )
if(writeDNA)
  write.dna(dna.concat,
    paste('../out/temp.dna.out/', dna.out.name, sep = ''),
    nbcol = -1, colsep = '')
