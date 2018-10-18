scrubOaks <- function(x, out = c('renamed', 'cleaned'), replaceTable = dat.meta.ext,
                      replaceCols = c('sp','state'), outDelim = '|') {
  x <- gsub('MOR_1|MOR1|MIR_1', 'MOR_001', x)
  x <- strsplit(x, "_", fixed = T) %>%
          lapply(., '[', 3:5) %>%
          sapply(., paste, collapse = '-')
  x[x %in% c('all', 'merged', 'transcript')] <-
    paste('Q.rubra', x[x %in% c('all', 'merged', 'transcript')], sep = '-')
  x.orig <- x
  if(class(replaceTable) %in% c('matrix', 'data.frame')) {
    x <- apply(cbind(replaceTable[x, replaceCols], x), 1, paste, collapse = '|') %>%
      as.character
  }
  if(out[1] == 'renamed') out <- x
  if(out[1] == 'cleaned') out <- x.orig
  return(out)
}
