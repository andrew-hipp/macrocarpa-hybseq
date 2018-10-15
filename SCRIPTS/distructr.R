distructr <- function(structFILE,
													bottom = 'quercus.pops',              #change label file if needed
													colors = 'quercus.perm',
													drawparams = 'drawparams',               #change file if needed
													returnDistructLine = TRUE,
													distructPath = '../ZZZ.DISTRUCT/',
													distructProg = './distruct',          #!# change to your path to DISTRUCT AREA
													K = 7
												  ) {
	dat.str <- readLines(structFILE)
  newfile <- sapply(strsplit(structFILE, '/', fixed = T), function(x) tail(x, 1))
	first.popq.line1 <- which(dat.str == "Proportion of membership of each pre-defined") + 6
	first.popq.lastLine <- which(dat.str == "--------------------------------------------")[2] -1
	popNum <- first.popq.lastLine-first.popq.line1 + 1
	writeLines(dat.str[first.popq.line1:first.popq.lastLine], paste(distructPath, newfile,'.popq', sep = ''))
	first.indivq.line1 <- which(dat.str == "Inferred ancestry of individuals:") + 2
	first.indivq.lastLine <- which(dat.str == "Estimated Allele Frequencies in each cluster") -3
  indNum <- first.indivq.lastLine - first.indivq.line1 + 1
	writeLines(dat.str[first.indivq.line1:first.indivq.lastLine], paste(distructPath, newfile,'.indivq', sep = ''))
  if(is.na(K)) {
    K <- strsplit(structFILE, "_K|_")[[1]]							###############################this may need to be changed depending on how you named your files
	  K <- K[length(K) - 2]
  }
	if(returnDistructLine) out <- paste(
		distructProg,
    '-M', popNum,
    '-N', indNum,
		'-K', K,
		'-b', bottom, '-c', colors,
		'-p', paste(newfile,'.popq', sep = ''),
		'-i', paste(newfile,'.indivq', sep = ''),
		'-o', paste(newfile, '.ps', sep = ''),
		'-d', drawparams
	)

}
