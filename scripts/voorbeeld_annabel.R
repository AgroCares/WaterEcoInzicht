# load packages
library(readxl)
library(dplyr)
library(data.table)
library(sf)
library(leaflet)
library(ggplot2)

# read in the latest version of ESFoordelen from 'data' -------------------
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

# read the latest version of EAGs
gEAG <- st_read("./data/EAG_20220809.gpkg") %>% st_transform(4326)
gEAG$watertype <- NULL

# merge EAG en ESF oordeel
# stap 1 is extraheren van gebiedscodes per regel in ESFoordelen
gebieden <- data.table::tstrsplit(ESFoordelen$OWL_SGBP3, split=c(','), fixed=TRUE)
ESFoordelen <- cbind(ESFoordelen, as.data.table(gebieden))
# stap 2 is iedere kolom vertalen naar een regelid (koppelcode)
mapdata <- melt.data.table(ESFoordelen, idvars = "OWMNAAM_SGBP3", measure.vars = colnames(ESFoordelen[,42:47]))
mapdata <- mapdata[!is.na(value), ]
# stap 3 is kwr waterlichaam codes en gaf codes vertalen naar EAGIDENTS
# een waterlichaam bestaat uit meerdere EAGIDENT
mapdata_krw <- mapdata[grepl("NL11",mapdata$value),]
mapdata_krw <- merge(mapdata_krw, eag_wl, by.x = "value", by.y = "KRW_SGBP3", all.x = TRUE)
# eag = eag, niets aanwijzigen
mapdata_eag <- mapdata[grepl("EAG",mapdata$value),]
mapdata_eag$GAFIDENT <- mapdata_eag$value
# een gaf bestaat uit meerdere eag idents
mapdata_gaf <- mapdata[!grepl("EAG",mapdata$value) & !grepl("NL11",mapdata$value),]
# hier moet nog een regel om vegelijkbaar als bij krw waterlichamen eags te koppelen aan gafs
# voeg mapdata samen (data van gafs mist nog)
mapdata <- rbind(mapdata_krw[,c('OWMNAAM_SGBP3','GAFIDENT')], mapdata_eag[,c('OWMNAAM_SGBP3','GAFIDENT')])

# stap 4 koppel een ruimtelijk bestannd met eags met een unieke gebiedsnaam (OWMNAAM_SGBP3) die in de factsheets wordt gebruikt
map <- merge(gEAG, mapdata, by = 'GAFIDENT', all.x = TRUE, duplicateGeoms = T)


# maak kaart kan zowel met ggplot als met leaflet, beide voorbeelden hieronder zijn niet af 
ggplot() +
  geom_sf(data = map, aes(fill = OWMNAAM_SGBP3), color = 'black') + 
   theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "#e0f3f8"),  ## azure
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0), # left-align
        legend.position="right") 
  

# Hier nog een palette aan toevoegen met verschillende kleuren per gebiedsnaam (OWMNAAM_SGBP3) of unieke regel in ESFoordelen
leaflet() %>%
  addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM),
              stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
              fill=T, fillColor = map$OWMNAAM_SGBP3, fillOpacity = 0.6) %>%
  addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
