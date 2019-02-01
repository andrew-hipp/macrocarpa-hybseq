library(rgeos)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(rgdal)
library(plyr)
library(proj4)



proj4string <- c("+proj=aea", #albers equal area
                 "+lon_0=-82",
                  "+ellps=clrk66",
                  "+lat_1=38",
                   "+lat_2=42",
                   "+lat_0=40",
                   "+units=m",
                   "+x_0=0",
                   "+y_0=0",
                   "+datum=WGS84",
                   "+axis=enu"
                 )
# Source data

if(!exists('mapsLittle')) {
  mapsLittle <- lapply(dir('../DATA/little-maps'), function(x) {
    temp <- readOGR(dsn=paste('../DATA/little-maps/', x, sep = ''),
                    layer=strsplit(x, '_', fixed = T)[[1]][2])
    temp@data$id = rownames(temp@data)
    temp.points = fortify(temp, region="id")
    temp.df = join(temp.points, temp@data, by="id")
    temp.df$Species <- paste('Quercus',
                        strsplit(x, '_', fixed = T)[[1]][1])
    names(temp.df)[10:11] <- c('ID1', 'ID2')
    temp.df$spGroup <- paste(strsplit(x, '_', fixed = T)[[1]][1],
                             temp.df$group, sep = '')
    xy <- as.data.frame(temp.df[, c('long', 'lat')])
    pj <- project(xy, proj4string, inverse=TRUE)
    temp.df$lat <- pj$y
    temp.df$long <- pj$x
    temp.df
  })
  names(mapsLittle) <- sapply(strsplit(dir('../DATA/little-maps'), '_', fixed = T),
                              function(x) x[1])
}
mapsLittle$all <- do.call(rbind, mapsLittle)

states <- map_data("state")
counties <- map_data('county')
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
                )
names(cbbPalette) <-
  c("Quercus alba", "Quercus bicolor", "Quercus lyrata", "Quercus macrocarpa",
    "Quercus michauxii", "Quercus montana", "Quercus muehlenbergii",
    "Quercus stellata")

p <- ggplot()
p <- p + geom_polygon(data = counties,
                      aes(x = long, y = lat, group = group),
                      fill = 'gray90', color = "white",
                      lwd = 0.1)
p <- p + geom_polygon(data = states,
                      aes(x = long, y = lat, group = group),
                      fill = NA, color = "gray70",
                      lwd=0.3)## figure out line width here
## add in a layer here of light gray for Q. macrocarpa, from GBIF, then overplot with counties, fill = NA
#tempMap <- mapsLittle$all[mapsLittle$all$Species %in% c('Quercus stellata', 'Quercus alba'), ]
p <- p + coord_fixed(1.3)
p <- p + coord_map('albers', lat0=40, lat1=38)
p <- p + scale_fill_manual(values = cbbPalette)
p <- p + scale_x_continuous('Longitude')
p <- p + scale_y_continuous('Latitude')
#p <- p + scale_fill_discrete(values = c('red', 'blue', 'yellow', 'gray'))
p <- p + theme(legend.position = c(0.12, 0.25))
p <- p + ylim(25, 53) + xlim(NA, -66) # to accommodate Q. macrocarpa range, into Canada
p.out <- list(
  macrocarpa = p + geom_polygon(data = mapsLittle$macrocarpa,
                          aes(x =long, y =lat, group = spGroup, fill = Species),
                          #fill = 'red',
                          #color = 'black',
                          alpha = 0.3
                        ),

  alba = p + geom_polygon(data = mapsLittle$alba,
                          aes(x =long, y =lat, group = spGroup, fill = Species),
                          #fill = 'red',
                          #color = 'black',
                          alpha = 0.3
                        ),

  bicolor.stellata = p + geom_polygon(data = mapsLittle$all[mapsLittle$all$Species %in% c('Quercus bicolor','Quercus stellata'), ],
                          aes(x =long, y =lat, group = spGroup, fill = Species),
                          #fill = 'red',
                          #color = 'black',
                          alpha = 0.3
                        ),

  lyrata.montana.michauxi.muehlenbergii = p + geom_polygon(data = mapsLittle$all[mapsLittle$all$Species %in%
                                                    c('Quercus lyrata',
                                                      'Quercus montana',
                                                      'Quercus michauxi',
                                                      'Quercus muehlenbergii'), ],
                          aes(x =long, y =lat, group = spGroup, fill = Species),
                          #fill = 'red',
                          #color = 'black',
                          alpha = 0.3
                        ),

  muehlenbergii = p + geom_polygon(data = mapsLittle$all[mapsLittle$all$Species %in%
                                                    c('Quercus muehlenbergii'), ],
                          aes(x =long, y =lat, group = spGroup, fill = Species),
                          #fill = 'red',
                          #color = 'black',
                          alpha = 0.3
                          )

                        )# close list

for(i in names(p.out)) {
  sppUse <- paste('Quercus', strsplit(i, '.', fixed = T)[[1]])
  p.out[[i]] <- p.out[[i]] + geom_point(data = dat.mapping[dat.mapping$Species %in% sppUse, ],
                    mapping = aes(x = jitter(long), y = jitter(lat),
                                  fill = Species),
                    shape=24,
                    colour = 'black',
                    size = 2.5
                    )
  pdf(paste('../out/map.out', i, 'pdf', sep = '.'), 11, 8.5)
  print(p.out[[i]])
  dev.off()
} # close for i

#print(p)
