## grab SNPs from a list of DNAbin objects,
##  ... write to STRUCTURE
##  args:
###   x - a list of DNAbin objects;
###       assumes there are already subsetted to SNPs and named consistently
###   grabPops - boolean; if(grabPops) get population assignment from the
###              popElement element of the row.names, delimited by popDelim
###   snpSol - boolean; if(snpSol) take one SNP per DNAbin object
###   freqThresh - maximum frequency for the most common nucleotide at a SNP,
###                as threshold for oneSNP
###   snpToPick - which SNP to grab if(snpSol); integer or 'random'
DNAbin2str <- function(x, file='structure.out', freqThresh = 0.95,
                        snpSol = FALSE, snpToPick = 1,
                        snpsToIntegers = TRUE,
                        grabPops = TRUE, shortenNames = 5,
                        popDelim = '|', popElement = 1,
                        loci.sep = '\t', verbose = TRUE,
                        ...) {
  require(ape)
  require(magrittr)
  if(class(x) %in% 'DNAbin') {
    x <- list(x)
    warning("Only one DNAbin object passed in")
  } # close if(class)
  if(snpsToIntegers) x <- lapply(x, function(y) {
    y <- as.character(y) %>% toupper
    y <- apply(y, 1:2, function(z) switch(z,  A='00',
                                              C='11',
                                              G='22',
                                              T='33',
                                              M = "01",
                                              R = "02",
                                              W = "03",
                                              S = "12",
                                              Y = "13",
                                              K = "23",
                                              'NA' = '99',
                                              'NULL' = '99'
                                            ) # close switch
                                        ) # close apply
                                        #y <- apply(y, 1:2, as.character)
                                        y
                                      } # close function
                                    ) # close lapply
  if(!snpSol) {
    if(length(x) > 1) {x <- do.call('cbind', list(x, fill.with.gaps = TRUE))
      } else x <- x[[1]]
  } else {
    print('subsetting SNPs is not implemented yet')
    # once it is, x becomes a matrix in this point after concatenated subsetted SNPs
  }
  x <- apply(x, 1:2, function(y) ifelse(y=='NULL', '99', as.character(y)))
  if(!is.na(freqThresh)) {
    maxFreqs <- apply(x, 2, function(y) {
      a <- table(paste(y, collapse = '') %>% strsplit(split = ''))
      a <- a[names(a) != '9']
      a <- a / sum(a)
      a <- max(a)
      a
      } # close function(y)
      )# close apply
    x <- x[ , maxFreqs <= freqThresh & !is.infinite(maxFreqs)]
} # close if(freqThresh)
  out <- c(N = dim(x)[1], L = dim(x)[2]) # dimensions of locus matrix
  x <- apply(x, 1, function(y) unlist(strsplit(y,''))) %>% t
  x <- apply(x, 1:2, function(y) ifelse(y == '9', '-9', y))

  if(grabPops) {
    pops <- sapply(strsplit(row.names(x), popDelim, fixed = T),
                  '[', popElement)
    pops <- as.factor(pops)
    popNum <- as.integer(pops)
    popMat <- cbind(popNum = as.integer(unique(pops)),
                    popName = unique(as.character(pops))
                  )
    popMat <- popMat[match(names(sort(table(pops))), popMat[, 2]), ]
    x <- cbind(popNum, x)
  }
  popTable <- table(as.character(pops))
  if(!is.na(shortenNames)) row.names(x) <- sapply(strsplit(row.names(x), "|", fixed = T), function(x) x[1]) %>%
                                      substr(start = 1, stop = shortenNames) %>%
                                      make.unique(sep = "_")
  write.table(x, file = paste(file, 'str', sep = '.'),
              sep = loci.sep, quote = FALSE, col.names = FALSE,
              ...)
  if(grabPops) write.table(popMat,
                           file = paste(file, 'pop_labels', sep = '.'),
                           sep = loci.sep, row.names = F,
                           quote = FALSE, col.names = FALSE,
                            ...)
  out <- c(out, POPDATA = ifelse(grabPops, 1, 0))
  structCommand <- paste('structure -i ', paste(file, 'str', sep = '.'),
                          ' -L ', out['L'],
                          ' -N ', out['N'],
                          ' -K ', dim(popMat)[1],
                          ' -o ', file, sep = '')
  writeLines(structCommand, paste('strRun', file, 'sh', sep = '.'))
  distructCommand <- paste('./distruct',
                           '-M',  dim(popMat)[1],
                           '-N', out['N'],
                           '-K', dim(popMat)[1],
                           '-b', paste(file, 'pop_labels', sep = '.'),
                           '-c quercus.perm',
                           '-p', paste(file, 'f.popq', sep = '_'),
                           '-i', paste(file, 'f.indvq', sep = '_'),
                           '-o', paste(file, 'ps', sep = '.'),
                           '-d drawparams')
  writeLines(distructCommand, paste('distruct', file, 'sh', sep = '.'))
  if(verbose) {
    message('Number of loci:', out['L'], '\n',
            'Number of individuals:', out['N'], '\n',
            'POPDATA =', out['POPDATA'])
    } # close if(verbose)
  out <- list(params = out, strctTemplate = structCommand, distTemplate = distructCommand)
  out
}
