## the subset of lines needed to make the table

K=8
kCols <- 6:(K+5)
dat.mac <- read.table('../ZZZ.DISTRUCT/hybseq.k8.20181016_f.indivq') # updated 2018-10-17
dat.mac <- cbind(dat.mac, dat.mapping[sapply(strsplit(inds.struct, '|', fixed = T), '[', 2), ])
for(i in c('Quercus muehlenbergii',
            'Quercus alba',
            'Quercus bicolor',
            'Quercus stellata')) {
  names(dat.mac)[
    which(names(dat.mac) == apply(dat.mac[dat.mac$Species == i, ], 2, function(x) mean(as.numeric(x))) %>% '['(kCols) %>% sort %>% tail(1) %>% names)
    ] <- i
  }
for(i in c('Quercus macrocarpa')) {
  names(dat.mac)[
    which(names(dat.mac) %in% (apply(dat.mac[dat.mac$Species == i, ], 2, function(x) mean(as.numeric(x))) %>% '['(kCols) %>% sort %>% tail(2) %>% names))
    ] <- paste(i, 1:2, sep = '-')
  }

dat.mac <- dat.mac[,c(grep('Quercus', names(dat.mac), value = T), 'long', 'lat', 'Species')]
dat.mac$Radius <- rep(0.75, dim(dat.mac)[1])
names(dat.mac)[names(dat.mac) == 'long'] <- 'Longitude'
names(dat.mac)[names(dat.mac) == 'lat'] <- 'Latitude'

dat.alba <- dat.mac[dat.mac$Species == 'Quercus alba', ]
dat.mac <- dat.mac[dat.mac$Species == 'Quercus macrocarpa', ]
