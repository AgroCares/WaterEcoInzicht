
# load packages
library(readxl)
library(dplyr)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)
library(RColorBrewer)

# load data -------------------

# load ESFoordelen (database beheerregister)
# set directory were ESF database is located
dir <- './input/toestand_esf/'
# select the latest file with ESF oordelen
fname <- list.files(dir, pattern = '.xlsx$')
fname <- sort(fname[grepl('^esfKRW',fname)],decreasing = TRUE)[1]
# read ESF oordelen
d1 <- readxl::read_xlsx(paste0(dir,'/',fname))
# which colnames are character
cols <- colnames(d1)[sapply(d1, is.character)]
# trim character columns from starting and ending space
setDT(d1)
d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
# trim all white spaces in area name
d1[,OWL_SGBP3 := lapply(.SD, function(x) gsub(" ", "", x)),.SDcols = "OWL_SGBP3"]
ESFoordelen <- d1[!is.na(d1$OWL_SGBP3) & !d1$OWL_SGBP3 == "",]

## aanvullende eag data
eag_wl <- fread('./input/gebiedsinfo/EAG_Opp_kenmerken_20220811.csv')
eag_wl <- eag_wl[is.na(eag_wl$Einddatum),]

# read the latest version spatial polygons EAGs
gEAG <- st_read("./data/EAG_20220809.gpkg") %>% st_transform(4326)
gEAG$watertype <- NULL

# merge EAG en ESF oordeel ---------------------
# step 1 extract area codes (one row = one factsheet in ESFoordelen)
gebieden <- data.table::tstrsplit(ESFoordelen$OWL_SGBP3, split=c(','), fixed=TRUE)
ESFoordelen <- cbind(ESFoordelen, as.data.table(gebieden))
# step 2 transpose data format to create a format which can be merged with spatial data 
mapdata <- melt.data.table(ESFoordelen, idvars = "OWMNAAM_SGBP3", measure.vars = colnames(ESFoordelen[,42:47]))
mapdata <- mapdata[!is.na(value), ]
# step 3 is translate KRW waterlichaam codes en gaf codes 2 EAGIDENTS
# 1 KRW waterbody = 1 or more EAGIDENT
mapdata_krw <- mapdata[grepl("NL11",mapdata$value),]
mapdata_krw <- merge(mapdata_krw, eag_wl, by.x = "value", by.y = "KRW_SGBP3", all.x = TRUE)
# eag = eag
mapdata_eag <- mapdata[grepl("EAG",mapdata$value),]
mapdata_eag$GAFIDENT <- mapdata_eag$value
# 1 gaf = 1 or more EAGIDENT
mapdata_gaf <- mapdata[!grepl("EAG",mapdata$value) & !grepl("NL11",mapdata$value),]
# add code here 2 merge gaf with eag codes
# combine mapdata sets krw eag and gaf (code is not finished!!)
mapdata <- rbind(mapdata_krw[,c('OWMNAAM_SGBP3','GAFIDENT')], mapdata_eag[,c('OWMNAAM_SGBP3','GAFIDENT')])
# step 4 merge spacial info EAG with EAG code from ESFoordelen (mapdata): 1 unique area name (OWMNAAM_SGBP3) = one factsheet 
map <- merge(gEAG, mapdata, by = 'GAFIDENT', all.x = TRUE, duplicateGeoms = T)


# maps can be made with ggplot and leaflet, 2 examples:
# ggplot
ggplot() +
  geom_sf(data = map, aes(fill = OWMNAAM_SGBP3), color = 'black') + 
  scale_fill_discrete()+
   theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#e0f3f8"),  ## azure
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0), # left-align
        legend.position="none") 
  

# leaflet example show watertype per EAG
EAG <- merge(gEAG, eag_wl, by = 'GAFIDENT',  all.x = TRUE, duplicateGeoms = T)
EAG <- EAG[EAG$KRW_SGBP3 == "",]
setorder(EAG,watertype)
### Create n colors for fill
n <- length(unique(EAG$watertype))
mypal <- colorRampPalette(brewer.pal(9, "Paired"))(n-2)
mypal1 <- colorRampPalette(brewer.pal(3, "Greys"))(1)
mypal <- c(mypal,mypal1)
pal <- colorFactor(palette = mypal,  domain = EAG$watertype)

leaflet(EAG) %>%
  addPolygons(layerId = EAG$GAFIDENT, popup= paste("naam", EAG$GAFNAAM, "<br>",
                                                       "Ident:", EAG$GAFIDENT,"<br>",
                                                       "watertype:", EAG$watertype),
              stroke = T, color= ~pal(watertype) , fillColor = ~pal(watertype), opacity=0.8, weight = 1, smoothFactor = 0.8,
              fill=T, fillOpacity = 0.6) %>%
  addProviderTiles("Esri.WorldGrayCanvas")%>%
  addLegend("bottomright", pal=pal, values=EAG$watertype)
