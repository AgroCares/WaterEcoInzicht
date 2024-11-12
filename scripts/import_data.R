
# import data hybi, fychem and BC------------
import_wqdata <- function(path = paste0('./input/', snapshot, '/fychem/'), location = location, parameter = parameter, pattern = ".txt"){
  
  # import data---------------------------------------------------------------
  # chemical data
  fychem <- list.files(path= path, pattern=pattern, full.names =  T)
  fychem <- lapply(fychem, fread, sep=';', colClasses = "character")
  fychem <- rbindlist(fychem, fill =T, use.names = T)
  # set classes data other than characters
  fychem[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  fychem[,meetwaarde := as.numeric(meetwaarde)]
  fychem[,c('xcoormonster','ycoormonster')  := list(as.integer(xcoormonster),as.integer(ycoormonster))]
  
  # data pre processing-------------------------------------------------------
  # couple data 2 water catchments by location code
  fychem <- fychem[,grep('locatie ', colnames(fychem)):= NULL]
  fychem <- fychem[,grep('WNA ', colnames(fychem)):= NULL]
  if('locatiecode' %in% colnames(fychem)){fychem[,locatie := locatiecode]}
  fychem <- merge(fychem, location, by.x = 'locatie', by.y = 'CODE', all.x = TRUE)
  
  if('fewsparameter' %in% colnames(fychem)){fychem[,parameterid := fewsparameter]}
  if('analysecode' %in% colnames(fychem)){fychem[,analyse := analysecode]}
  # couple data 2 additional parameterinfo
  fychem <- merge(fychem, parameter[,c("code","naam", "categorie", "H_min", "H_max")], by.x = 'parameterid', by.y = 'code')
  setnames(fychem, c("naam"),c("parameterid_naam"))
  
  return(fychem)

}

# import output Aquo-kit ------------
importKRW <- function(inputdir = inputdir, 
                      locaties = locaties, eag_wl, orderMaatlatten) {
  
  # load all results from aquo kit from input dir
  EKRlijst <- list.files(path= paste0(inputdir), pattern=".csv", full.names =  T)
  classes <- sapply(fread(EKRlijst[2L], sep=';'), class)
  EKRlijst <- lapply(EKRlijst, fread, sep=';', colClasses = unlist(classes))
  EKRlijst <- rbindlist(EKRlijst, fill =T, use.names = T)
  
  # create a sample loc code which can be mapped to deelgebied (EAG)
  EKRlijst[,locatie := sapply(strsplit(Meetobject.lokaalID, '_'), `[`, 1)]
  # merge sample loc with additional info on the area (ecologisch analysegebied = EAG) sampled 
  EKRlijst <- merge(EKRlijst, locaties[,c('CODE','EAGIDENT','XCOORD','YCOORD')], by.x = 'locatie', by.y = 'CODE', all.x = TRUE)
  # complement EAGIDENT with aggregated KRW results (which can not be mapped from locatie)
  EKRlijst[locatie %in% unique(eag_wl$EAGIDENT[eag_wl$EAGIDENT!=""]), EAGIDENT := locatie]
  
  # complement OWMIDENT from aggregated results (which can not be mapped from eag) -- annoying = the change of referring to waterbodies in de toetsresulateten differes each year
  # EKRlijst[GeoObject.code %in% unique(eag_wl$KRW_SGBP3[eag_wl$KRW_SGBP3!=""]), KRW_SGBP3 := GeoObject.code]
  eag_wl <- eag_wl[,krwlocatie :=  sapply(strsplit(KRWmonitoringslocatie_SGBP3, '_'), `[`, 2)]
  EKRlijst[locatie %in% unique(eag_wl$krwlocatie[eag_wl$KRW_SGBP3!=""]), krwlocatie := locatie]
  EKRlijst <- merge(EKRlijst, unique(eag_wl[,c('KRW_SGBP3','krwlocatie')]), by = 'krwlocatie', all.x = TRUE)

  # merge data with additional info KRW waterbody (mapped 2 eagident)
  EKRlijst <- merge(EKRlijst, eag_wl[,c('EAGIDENT','GAFIDENT','EAGNAAM','KRW_SGBP3')], by = 'EAGIDENT',all.x = TRUE)
  EKRlijst[,KRW_SGBP3 := KRW_SGBP3.x]
  EKRlijst[!is.na(EAGIDENT), KRW_SGBP3 := KRW_SGBP3.y]
  EKRlijst[,c('KRW_SGBP3.x','KRW_SGBP3.y') := NULL]
  
  # add name KRW
  EKRlijst <- merge(EKRlijst, unique(eag_wl[,c('KRW_SGBP3','SGBP3_NAAM')]), by = 'KRW_SGBP3', all.x = TRUE)
 
  # add naam van een toetsgebied (WL of EAG naam)
  EKRlijst[,waterlichaam := fifelse(!(is.na(SGBP3_NAAM)|SGBP3_NAAM == ""), SGBP3_NAAM, EAGNAAM)]
  EKRlijst[,waterlichaam_code := fifelse(!(is.na(KRW_SGBP3)|KRW_SGBP3 == ""), KRW_SGBP3, EAGIDENT)]
  
  # remove data which cannot be mapped 2 location, eag or owm (fish data)
  checkloc <-  EKRlijst[(is.na(EAGIDENT)|EAGIDENT == "") & (is.na(KRW_SGBP3)|KRW_SGBP3 ==""),]
  
  if(nrow(checkloc)>1){
    warning(paste0('locatie ', unique(checkloc$locatie), ' komt wel voor in toetsresultaten maar niet in meetpuntenbestand'))}
  EKRlijst <- EKRlijst[!is.na(EAGIDENT) | !(is.na(KRW_SGBP3)|KRW_SGBP3 == ""),]
  
  # Bewerken data / proces data ---------------------------------------------------
  #add unique ID 4 aggregated data 
  EKRlijst[, aggregated := 'nee']
  # EKRlijst[EAGIDENT == locatie | GeoObject.code == KRW_SGBP3, aggregated := 'ja']
  EKRlijst[EAGIDENT == locatie | krwlocatie == locatie, aggregated := 'ja']
  # make classes a factor
  EKRlijst$klasse <- as.factor(EKRlijst$Classificatie)
  # add year
  EKRlijst$jaar<- format(EKRlijst$Begindatum, '%Y')
  EKRlijst$jaar<- as.numeric(EKRlijst$jaar)
  # make unique parameter/ waarneming
  EKRlijst$GHPR <- paste(ifelse(is.na(EKRlijst$Grootheid.omschrijving)|EKRlijst$Grootheid.omschrijving == '', EKRlijst$Typering.Omschrijving,EKRlijst$Grootheid.omschrijving),ifelse(is.na(EKRlijst$Parameter.omschrijving)|EKRlijst$Parameter.omschrijving == '',"",EKRlijst$Parameter.omschrijving))
  EKRlijst$GHPR <- trimws(EKRlijst$GHPR)
  #couple parameter 2 leveld parameter legend
  # check <- unique(EKRlijst$GHPR[!(EKRlijst$GHPR %in% unique(orderMaatlatten$GHPR))])
  EKRlijst <- merge(EKRlijst, orderMaatlatten[, c('GHPR','level','GHPR_level')], by = 'GHPR', all.x = TRUE)
  # adapt names for readability
  EKRlijst[,facet_wrap_code := as.factor(gsub("other:Aquo-kit;Bio-toetsing;KRWmaatlat2018 - ","",Waardebepalingsmethode.code))]
  EKRlijst[facet_wrap_code == 'other:Aquo-kit;OW-toetsing;tussenresultaat;SOM',facet_wrap_code := sapply(strsplit(Parameter.code, '_'), `[`, 1)]
  
  # opslaan Rdataset --------------------------------
  return(EKRlijst)
  
}

# import KRW factsheets data files on ESFs and maatregelen -----------
# read in the lastest data from ESF oordelen
ppr_esf_oordeel <- function(dir = './input/toestand_esf/', eag_wl, ekrtrendbeschrijving, trendfychem, toestandbeschrijving_eag){
  
  # select the latest file with ESF oordelen
  fname <- list.files(dir, pattern = '.csv$')
  fname <- sort(fname[grepl('^esfKRW',fname)],decreasing = TRUE)[1]
  
  
  # read ESF oordelen
  d1 <- data.table::fread(paste0(dir,'/',fname),encoding = "Latin-1")
  
  # which colnames are character
  cols <- colnames(d1)[sapply(d1, is.character)]
  
  # trim character columns from starting and ending space
  d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  d1 <- d1[!is.na(d1$OWL_SGBP3) & !d1$OWL_SGBP3 == "",]
  
  #add status ESF as text
  #ESF1
  d1$ESF1 <- ifelse(d1$ESF1_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor productiviteit water. De hoeveelheid voedingsstoffen die dit watersysteem binnenkomt is hoger dan de draagkracht van het ecosysteem, waardoor kroos of algen kunnen gaan woekeren. Te veel kroos of algen belemmeren de groei van ondergedoken waterplanten.', d1$ESF1),
                             ifelse(d1$ESF1_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor productiviteit water. De hoeveelheid voedingsstoffen die dit watersysteem binnenkomt is mogelijk (of lokaal) hoger dan de draagkracht van het ecosysteem waardoor kroos of algen kunnen gaan woekeren. Te veel kroos of algen belemmeren de groei van ondergedoken waterplanten.', d1$ESF1),
                                    ifelse(d1$ESF1_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor productiviteit water. De hoeveelheid voedingsstoffen die dit watersysteem binnenkomt is lager dan de draagkracht van het ecosysteem. Kroos of algen zijn geen belemmering voor de groei van ondergedoken waterplanten. ', d1$ESF1),
                                           paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor productiviteit water. Het is niet bekend of de hoeveelheid voedingsstoffen die dit watersysteem binnenkomt hoger of lager is dan de draagkracht van het ecosysteem en woekerende kroos of algen een belemmering vormen voor de groei van ondergedoken waterplanten. ', d1$ESF1))))
  
  #ESF2
  d1$ESF2 <- ifelse(d1$ESF2_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor lichtklimaat. Er valt onvoldoende licht op de waterbodem voor plantengroei. ', d1$ESF2),
                    ifelse(d1$ESF2_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor lichtklimaat. Er valt mogelijk (of lokaal) onvoldoende licht op de waterbodem voor plantengroei. ', d1$ESF2),
                          ifelse(d1$ESF2_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor lichtklimaat. Er valt voldoende licht op de waterbodem voor plantengroei. ', d1$ESF2),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor lichtklimaat. Het is niet bekend of er voldoende licht op de waterbodem valt voor plantengroei. ', d1$ESF2))))
  
  #ESF3
  d1$ESF3 <- ifelse(d1$ESF3_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor productiviteit waterbodem. De hoeveelheid voedingstoffen in de waterbodem is te hoog. Een hoge productiviteit van de waterbodem gaat vaak samen met toxische condities in de wortelzone. ', d1$ESF3),
                    ifelse(d1$ESF3_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor productiviteit waterbodem. De hoeveelheid voedingstoffen in de waterbodem is mogelijk of lokaal te hoog. Een hoge productiviteit van de waterbodem gaat vaak samen met toxische condities in de wortelzone. ', d1$ESF3),
                           ifelse(d1$ESF3_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor productiviteit waterbodem. De hoeveelheid voedingstoffen en de toxines sulfide en ammonium in de waterbodem zijn voldoende laag voor de ontwikkeling van een soortenrijke onderwatervegetatie. ', d1$ESF3),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor productiviteit waterbodem. Het is niet bekend of de hoeveelheid voedingstoffen in de waterbodem te hoog of laag is. ', d1$ESF3))))
  #ESF4
  d1$ESF4 <- ifelse(d1$ESF4_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor habitatgeschiktheid. Het water voldoet dus niet aan de belangrijkste eisen die planten en dieren aan hun leefomgeving stellen. Dit wordt vooroorzaakt door een ongewenste habitatstructuur (zoals waterdiepte, slibsamenstelling, oevertalud, peilfluctuatie) en/of een ongewenste variatie in de chemische samenstelling (zoals chloride, macro-ionen) van het water. Niet alleen fysieke systeemkenmerken, maar ook de vegetatiedichtheid en -structuur bieden habitatstructuur; macrofauna en visgemeenschappen worden dus ook beïnvloed door de aanwezige vegetatie.', d1$ESF4),
                    ifelse(d1$ESF4_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor habitatgeschiktheid. Het water voldoet mogelijk (of lokaal) niet aan de belangrijkste eisen die planten en dieren aan hun leefomgeving stellen. Dit wordt mogelijk vooroorzaakt door een ongewenste habitatstructuur (zoals waterdiepte, slibsamenstelling, oevertalud, peilfluctuatie) en/of een ongewenste variatie in de chemische samenstelling (zoals chloride, macro-ionen) van het water. Niet alleen fysieke systeemkenmerken, maar ook de vegetatiedichtheid en -structuur bieden habitatstructuur; macrofauna en visgemeenschappen worden dus ook beïnvloed door de aanwezige vegetatie.', d1$ESF4),
                           ifelse(d1$ESF4_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor habitatgeschiktheid. Het water voldoet dus aan de belangrijkste eisen die planten en dieren aan hun leefomgeving stellen. Zowel de habitatstructuur (waterdiepte, de samenstelling van het slib, het oevertalud) als variaties in chemische samenstelling (chloride, macro-ionen) zijn geschikt voor de gewenste planten of dieren. De vegetatie is voldoende dicht en complex voor het voorkomen van gewenste macrofauna en visgemeenschappen.', d1$ESF4),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor habitatgeschiktheid. Het is niet bekend of het water voldoet aan de belangrijkste eisen die planten en dieren aan hun leefomgeving stellen.', d1$ESF4))))
  
  #ESF5
  d1$ESF5 <- ifelse(d1$ESF5_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor verspreiding. Gewenste planten (zaden) en/of macrofauna en/of vissen kunnen het watersysteem niet goed bereiken. ', d1$ESF5),
                    ifelse(d1$ESF5_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor verspreiding. Gewenste planten (zaden) en/of macrofauna en/of vissen kunnen het watersysteem mogelijk niet goed bereiken. ', d1$ESF5),
                           ifelse(d1$ESF5_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor verspreiding. Gewenste planten (zaden) en/of macrofauna en/of vissen kunnen het watersysteem goed bereiken. ', d1$ESF5),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor verspreiding. Het is niet bekend of gewenste planten (zaden) en/of macrofauna en/of vissen het watersysteem in voldoende mate kunnen bereiken.', d1$ESF5))))
  #ESF6
  d1$ESF6 <- ifelse(d1$ESF6_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor verwijdering. Maai- en baggerbeheer en/of vraat (door vogels, vissen of kreeften) hebben een nadelige invloed op het voorkomen van gewenste planten en dieren.', d1$ESF6),
                    ifelse(d1$ESF6_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor verwijdering. Maai- en baggerbeheer en/of vraat (door vogels, vissen of kreeften) hebben mogelijk (of lokaal) een nadelige invloed op het voorkomen van gewenste planten en dieren. ', d1$ESF6),
                           ifelse(d1$ESF6_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor verwijdering. Maai- en baggerbeheer en/of vraat (door vogels, vissen of kreeften) hebben geen nadelige invloed op het voorkomen van gewenste planten en dieren. ', d1$ESF6),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor verwijdering. Het is niet bekend of maai- en baggerbeheer en/of vraat (door vogels, vissen of kreeften) een nadelige invloed hebben op het voorkomen van gewenste planten en dieren. ', d1$ESF6))))
  #ESF7
  d1$ESF7 <- ifelse(d1$ESF7_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor organische belasting. De belasting met organisch materiaal vanuit riooloverstorten, ongezuiverde lozingen, mest, hondenpoep, ingewaaid blad, of brood voor de eenden is groter dan het watersysteem aan kan. Voor de afbraak van dit organisch materiaal is zuurstof nodig, waardoor er zuurstofloosheid ontstaat en dieren (zoals vissen) kunnen sterven en bacteriën gaan groeien die giftige stoffen produceren. ', d1$ESF7),
                    ifelse(d1$ESF7_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor organische belasting. De belasting met organisch materiaal vanuit riooloverstorten, ongezuiverde lozingen, mest, hondenpoep, ingewaaid blad, of brood voor de eenden is mogelijk (of lokaal) groter dan het watersysteem aan kan. Voor de afbraak van dit organisch materiaal is zuurstof nodig, waardoor er zuurstofloosheid kan ontstaan en dieren (zoals vissen) kunnen sterven. ', d1$ESF7),
                           ifelse(d1$ESF7_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor organische belasting. De belasting met organisch materiaal vanuit riooloverstorten, ongezuiverde lozingen, mest, hondenpoep, ingewaaid blad, of brood voor de eenden is lager dan het systeem aankan. ', d1$ESF7),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor organische belasting. Het is onbekend of de belasting met organisch materiaal vanuit riooloverstorten, ongezuiverde lozingen, mest, hondenpoep, ingewaaid blad, of brood voor de eenden hoger of lager is dan het systeem aankan. ', d1$ESF7))))
  #ESF8
  d1$ESF8 <- ifelse(d1$ESF8_nr == '3', paste('Het watersysteem voldoet niet aan de eisen van de sleutelfactor toxiciteit. Er zijn giftige verontreinigingen, zoals zware metalen, gewasbeschermingsmiddelen en medicijnresten aanwezig in het watersysteem. ', d1$ESF8),
                    ifelse(d1$ESF8_nr == '2',paste('Het watersysteem voldoet mogelijk (of lokaal) niet aan de eisen van de sleutelfactor toxiciteit. Er zijn mogelijk giftige verontreinigingen, zoals zware metalen, gewasbeschermingsmiddelen en medicijnresten aanwezig in het watersysteem. ', d1$ESF8),
                           ifelse(d1$ESF8_nr == '1',paste('Het watersysteem voldoet aan de eisen van de sleutelfactor toxiciteit. Er zijn geen giftige verontreinigingen, zoals zware metalen, gewasbeschermingsmiddelen en medicijnresten aanwezig in het watersysteem. ', d1$ESF8),
                                  paste('Het is onbekend of het watersysteem voldoet aan de eisen van de sleutelfactor toxiciteit. Het is onbekend of er giftige verontreinigingen, zoals zware metalen, gewasbeschermingsmiddelen en medicijnresten aanwezig zijn in het watersysteem. ', d1$ESF8))))
  
  
  # add trends and discrition of hydrobiological state --------
  # step 1 extract area codes (one row = one factsheet in ESFoordelen)
  gebieden <- data.table::tstrsplit(d1$OWL_SGBP3, split=c(','), fixed=TRUE)
  d1 <- cbind(d1, as.data.table(gebieden))
  d1[, id := 1:nrow(d1)]
  # step 2 transpose data format to create a format which can be merged with spatial data 
  cols <- colnames(d1)[grepl('^V', colnames(d1))]
  mapdata <- melt.data.table(d1, idvars = "OWMNAAM_SGBP3", measure.vars = cols)
  mapdata <- mapdata[!is.na(value), ]
  # step 3 is translate KRW waterlichaam codes en gaf codes 2 EAGIDENTS
  # 1 KRW waterbody = 1 or more EAGIDENT
  mapdata_krw <- mapdata[grepl("NL11",mapdata$value),]
  mapdata_krw <- merge(mapdata_krw, eag_wl, by.x = "value", by.y = "KRW_SGBP3", all.x = TRUE)
  # eag = eag
  mapdata_eag <- mapdata[grepl("EAG",mapdata$value),]
  mapdata_eag$EAGIDENT <- mapdata_eag$value
  # trim character columns from starting and ending space
  mapdata_eag$EAGIDENT <- gsub("^\\s+|\\s+$", "", mapdata_eag$EAGIDENT, perl = TRUE)
  # 1 gaf = 1 or more EAGIDENT
  mapdata_gaf <- mapdata[!grepl("EAG",mapdata$value) & !grepl("NL11",mapdata$value),]
  mapdata_gaf <- merge(mapdata_gaf, eag_wl, by.x = "value", by.y = "GAFIDENT", all.x = TRUE)
  # combine mapdata sets krw eag and gaf
  mapdata <- rbind(mapdata_krw[,c('id','OWMNAAM_SGBP3','EAGIDENT')], mapdata_eag[,c('id','OWMNAAM_SGBP3','EAGIDENT')],  mapdata_gaf[,c('id','OWMNAAM_SGBP3','EAGIDENT')])
  mapdata <- mapdata[!is.na(EAGIDENT),]
  # add toestand obv toestandbeschrijving
  mapdata <- merge(mapdata, toestandbeschrijving_eag, by = "EAGIDENT", all.x = T)
  # add trend hybi
  mapdata <- merge(mapdata, ekrtrendbeschrijving, by = "EAGIDENT", all.x = T)
  # add trend fychem
  mapdata <- merge(mapdata, trendfychem, by = "EAGIDENT", all.x = T)
  
  # hier dcast om tabel met header kwaliteitslementen te maken
  est_toestand <- dcast.data.table(setDT(mapdata), id + OWMNAAM_SGBP3 ~ EAGIDENT, value.var = c("toestandb"))
  # merge tekst
  est_toestand[,toestandb := do.call(paste, c(.SD, sep = " ")), .SDcols = 3:ncol(est_toestand)]
  # remove NA
  est_toestand$toestandb <- gsub("NA" , "", est_toestand$toestandb) 
  # trim character columns from starting and ending space
  est_toestand$toestandb  <- gsub("^ *|(?<= ) | *$", "", est_toestand$toestandb, perl = TRUE)
  
  # hier dcast om tabel met header kwaliteitslementen te maken
  est_trendekr <- dcast.data.table(setDT(mapdata), id + OWMNAAM_SGBP3 ~ EAGIDENT, value.var = c("mergetekst"))
  # merge tekst
  est_trendekr[,mergetekst := do.call(paste, c(.SD, sep = " ")), .SDcols = 3:ncol(est_trendekr)]
  # remove NA
  est_trendekr$mergetekst <- gsub("NA" , "", est_trendekr$mergetekst) 
  # trim character columns from starting and ending space
  est_trendekr$mergetekst  <- gsub("^ *|(?<= ) | *$", "", est_trendekr$mergetekst, perl = TRUE)
  
  # hier dcast om tabel met header kwaliteitslementen te maken
  est_trendwq <- dcast.data.table(setDT(mapdata), id + OWMNAAM_SGBP3 ~ EAGIDENT, value.var = c("trendNP"))
  # merge tekst
  est_trendwq[,trendNP := do.call(paste, c(.SD, sep = " ")), .SDcols = 3:ncol(est_trendwq)]
  # remove NA
  est_trendwq$trendNP <- gsub("NA" , "", est_trendwq$trendNP) 
  # trim character columns from starting and ending space
  est_trendwq$trendNP  <- gsub("^ *|(?<= ) | *$", "", est_trendwq$trendNP, perl = TRUE)
  
  d1 <- merge(d1, est_toestand[,c("id","toestandb")], by = 'id', all.x = TRUE)
  d1 <- merge(d1, est_trendekr[,c("id","mergetekst")], by = 'id', all.x = TRUE)
  d1 <- merge(d1, est_trendwq[,c("id","trendNP")], by = 'id', all.x = TRUE)
  
  d1$Toestand <- paste0(d1$Toestand, " ", d1$toestandb)
  d1$Trend <- paste0(d1$mergetekst)
  d1$TrendChemie <- paste0(d1$trendNP)
  # return ESF oordelen
  return(d1)
  
}

# read in the lastest data from maatregelen
ppr_maatregelen <- function(dir = 'data'){
  
  # select the latest file with maatregelen
  fname <- list.files(dir, pattern = '.csv$')
  fname <- sort(fname[grepl('^maatregelenKRW',fname)],decreasing = TRUE)[1]
  
  # read maatregelen
  d1 <- data.table::fread(paste0(dir,'/',fname),encoding = "Latin-1")
  
  # which colnames are character
  cols <- colnames(d1)[sapply(d1, is.character)]
  
  # trim character columns from starting and ending space
  d1[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  
  # select only the unique rows
  d1 <- unique(d1)
  
  # remove columns with no data or non-relevant information (judgement gerard)
  cols <- colnames(d1)[unlist(d1[,lapply(.SD,function(x) sum(is.na(x))==nrow(d1))])]
  d1[,c(cols) := NULL]
  
  # return maatregelen
  return(d1)
  
}

# importeer waterbalansen-------------
# functie om algemene gegevens van de excelsheet te laden 
loadAlgemeen = function(x,wdir){
  
  # file name including location
  fname <- paste0(wdir,x)
  
  # print progress
  print(paste0('algemene data van ',basename(fname),' worden ingelezen'))
  
  # read the tab uitangspunten
  tabbladen <-    excel_sheets(fname)
  n2 <- tabbladen[grepl("uitg", tolower(tabbladen))]
  alg = suppressMessages(readxl::read_xlsx(fname, sheet = n2, col_names = F, skip = 2)[1:34,1:9])
  
  # make data.table to store results
  out <- data.table(pol = basename(fname))
  
  # lees oppervlaktes
  cols <- c('a_tot','a_drain','a_verhard','a_gemengd','a_water')
  out[, c(cols) := as.list(as.numeric(unlist(alg[5:9,4]))/10000)]
  
  # lees bodemhoogte en slootdiepte
  cols <- c('a_bodemhoogte','a_slootdiepte')
  out[,c(cols) := as.list(as.numeric(unlist(alg[10,c(4,7)])))]
  
  # lees p concntraties verschillende posten
  cols <- c('p_neerslag','p_kwel','p_verhard','p_riol','p_drain','p_uitspoel','p_afstroom',paste0('p_inlaat',1:5))
  out[,c(cols) := as.list(as.numeric(unlist(alg[15:26,4])))]
  
  # lees inlaten en uitlaten
  cols <- c(paste0('a_inlaat',1:5),paste0('a_uitlaat',1:4))
  out[,c(cols) := as.list(as.character(unlist(alg[22:30,1])))]
  
  # return output
  return(out)
}
loadBalance2 = function(x,wdir){
  
  # file name including location
  fname <- paste0(wdir,x)
  
  # print commands to show progress
  print(paste0('water and P fluxes from ',basename(fname),' worden ingelezen'))
  
  # read excel water balance, different for xls and xlsx
  if(grepl(pattern = '.xls$', fname)){
    balans  = readxl::read_xls(fname, sheet = 'Q+P_mnd', col_names = F, na = '#N/A', skip = 13 )
  } else {
    balans = suppressMessages(readxl::read_xlsx(fname, sheet = 'Q+P_mnd',col_names = F, na = '#N/A')[-c(1:13),] )
  }
  
  # convert to data.table and give names (p1 to p..n)
  balans <- as.data.table(balans)
  setnames(balans,paste0('p',1:ncol(balans)))
  
  # add date and time
  balans[,date := as.Date(as.numeric(p1),origin = "1900-01-01")]
  balans[,maand := month(date)]
  balans[,jaar := year(date)]
  balans[,seiz := ifelse(maand %in% 4:9,'zomer','winter')]
  
  # peil [m NAP], volume [m3], debiet[mm/dag], berging [m3/dag] en sluitfout [m3/dag]
  cols <- paste0('p',c(2:4,28:30))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('w_',c('peil','volume','debiet','berging','sluitfout','maalstaat')))
  
  # IN waterposten [m3/dag]
  cols <- paste0('p',6:17)
  colsn <- c('neerslag','kwel','verhard','riol','drain','uitspoel','afstroom',paste0('inlaat',1:5))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('w_i_',colsn))
  
  # UIT waterposten [m3/dag]
  cols <- paste0('p',19:26)
  balans[,(cols) := lapply(.SD,function(x) as.numeric(x) * -1),.SDcols = cols]
  setnames(balans,cols,paste0('w_o_',c('verdamping','wegzijging','intrek',paste0('uitlaat',1:5))))
  
  # IN P-posten op basis van minimum [mg/m2/dag]
  cols <- paste0('p',35:46)
  colsn <- c('neerslag','kwel','verhard','riol','drain','uitspoel','afstroom',paste0('inlaat',1:5))
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('wp_min_',colsn))
  
  # IN P-posten op basis van increment [mg/m2/dag]
  cols <- paste0('p',48:59)
  balans[,(cols) := lapply(.SD,as.numeric),.SDcols = cols]
  setnames(balans,cols,paste0('wp_inc_',colsn))
  
  # UIT P-posten (bij gemaal)
  balans[,wp_meting_gm3 := as.numeric(p5)]
  balans[wp_meting_gm3 == 0, wp_meting_gm3 := NA]
  balans[,wp_meting_mgm2d := -1 * as.numeric(p63)]
  
  # remove columns not used
  cols <- colnames(balans)[grepl('^p',colnames(balans))]
  balans[,(cols) := NULL]
  
  # add poldernaam
  balans[,pol := basename(fname)]
  
  # filter data given availability of precipitation (per month)
  out <- balans[w_i_neerslag >0]
  
  # rest column order with polder name and time first
  setcolorder(out,c('pol','date','jaar','maand','seiz'))
  
  # return output
  return(out)
}
# wrapper function 
loadBalances_lm <- function(dir_bal, init, sfile = T){
  
  # select file names in the directory where waterbalansen are stored
  files <- list.files(dir_bal)
  
  # select only the xlsx files (all are renewed to xlsx)
  files <- files[grepl('xlsx$',files)]
  
  # read excel data from sheet 'uitgangspunten' and combine all output in one data.table
  alg <- lapply(files, function(x) loadAlgemeen(x,wdir = dir_bal))
  alg <- rbindlist(alg)
  
  # read excel data from sheet 'jaargemiddelden'
  bal <- lapply(files,function(x) loadBalance2(x,wdir = dir_bal))
  bal <- rbindlist(bal)
  
  #koppel uitgangspunten en q+p_mnd
  dat <- merge(bal,alg, by='pol',all.x = TRUE)
  
  # remove rows without maalstaat
  dat <- dat[!is.na(w_maalstaat)]
  dat <- dat[!is.na(w_i_neerslag)]
  dat <- dat[,lastyear := max(jaar), by = c('pol')]
  
  # add red DAW value
  dat[,p_i_redDAW := 0.1 * wp_min_uitspoel]
  
  # add wp_min_sum and wp_inc_sum
  dat[,wp_min_sum := rowSums(.SD,na.rm=T),.SDcols = grep("wp_min_",names(dat))]
  dat[,wp_inc_sum := rowSums(.SD,na.rm=T),.SDcols = grep("wp_inc_",names(dat))]
  
  # save file
  if(sfile){saveRDS(dat, file = paste0('./data/dat','.rds'))}
  
  # return data.table
  return(dat)
}



