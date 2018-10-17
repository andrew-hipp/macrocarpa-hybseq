library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(scatterpie)
library(magrittr)
library(ggrepel)

K=8
kCols <- 6:(K+5)
dat.mac <- read.table('../ZZZ.DISTRUCT/hybseq.k8.20181016_f.indivq') # updated 2018-10-17
dat.mac <- cbind(dat.mac, dat.mapping[sapply(strsplit(inds.struct, '|', fixed = T), '[', 2), ])
for(i in c('Quercus bicolor',
            'Quercus stellata')) {
  names(dat.mac)[
    which(names(dat.mac) == apply(dat.mac[dat.mac$Species == i, ], 2, function(x) mean(as.numeric(x))) %>% '['(kCols) %>% sort %>% tail(1) %>% names)
    ] <- i
  }
for(i in c('Quercus muehlenbergii',
            'Quercus alba',
            'Quercus macrocarpa')) {
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

# https://guangchuangyu.github.io/2016/12/scatterpie-for-plotting-pies-on-ggplot/

states <- map_data("state")
counties <- map_data('county')
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p.mac <- ggplot(data = states)
p.mac <- p.mac + geom_polygon(data = counties,
                      aes(x = long, y = lat, group = group),
                      fill = 'gray90', color = "white",
                      lwd = 0.1)
p.mac <- p.mac + geom_polygon(aes(x = long, y = lat, group = group),
                      fill = NA, color = "black")
p.mac <- p.mac + scale_fill_manual('Species cluster', values = cbbPalette)
p.mac <- p.mac + coord_map('albers', lat0=40, lat1=38)
p.macPrint <- p.mac + geom_scatterpie(aes(x = Longitude, y = Latitude, r = Radius),
                                 data = dat.mac, cols = grep('Quercus', names(dat.mac), value = T), alpha = 1)
pdf('../out/macrocarpa.genotypes.map.HybSeq.pdf', 11, 8.5)
print(p.macPrint)
dev.off()

p.alba <- p.mac + geom_scatterpie(aes(x = Longitude, y = Latitude, r = Radius),
                                 data = dat.alba, cols = grep('Quercus', names(dat.alba), value = T), alpha = 1)

pdf('../out/alba.genotypes.map.HybSeq.pdf', 11, 8.5)
print(p.alba)
dev.off()
