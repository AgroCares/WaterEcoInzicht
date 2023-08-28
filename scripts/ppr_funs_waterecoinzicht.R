# Laura Moria - juni 2023

# postprocess functions to combine output Aquo-kit and aggregate data ----------------
# importeer alle uitvoerbestanden met toetsresultaten uit de Aquo-Kit
importAquoResult <- function(path = dirExportAquo, pattern = ".csv"){
  lijst <- list.files(path= path, pattern=pattern, full.names =  T)
  classes <- sapply(fread(lijst[2L], sep=';'), class)
  EKRlijst <- lapply(lijst, fread, sep=';', colClasses = unlist(classes))
  EKRlijst <- rbindlist(EKRlijst, fill =T, use.names = T)
  return(EKRlijst)
}    
# import all sample locations (import files aquo kit)
importMeetpunten <- function(path = dirMeetpuntenAquo, pattern = ".csv"){
  lijst <- as.list(list.files(path= path, pattern=pattern, full.names =T))
  meetpunten <- lapply(lijst, function(i) fread(i))
  loc <- rbindlist(meetpunten, fill =T, use.names =T)
  return(loc)
} 
# couple location info 2 results x = meetpuntenAquo; y = meetpuntenGebied; z = EKRlijst
kopDat <- function(x , y , z){
  #create a sample loc code
  x[,locatie := sapply(strsplit(Identificatie, '_'), `[`, 1)]
  x <- unique(x[ ,c('Identificatie','HoortBijGeoobject.identificatie','Wegingsfactor','locatie')])
  x<- x[!is.na(x$Identificatie),]
  #create a sample location db
  loc <- merge(y[,c('CODE','EAGIDENT',"OWMIDENT", 'XCOORD','YCOORD')], x, by.x = 'CODE', by.y='locatie', all.x = FALSE, all.y = FALSE) 
  
  checkloc <- z[!(z$Meetobject.lokaalID %in% loc$Identificatie),] # check locaties die niet gekoppeld zijn aan EAG
  z <- merge(loc[,c('CODE','EAGIDENT','OWMIDENT','XCOORD','YCOORD','Identificatie','HoortBijGeoobject.identificatie')], z, by.x = 'Identificatie', by.y = 'Meetobject.lokaalID', all.x = FALSE, all.y = TRUE)
  
  # complement OWMIDENT 
  z[GeoObject.code %in% unique(meetpuntenGebied$OWMIDENT[meetpuntenGebied$OWMIDENT!="" & meetpuntenGebied$OWMIDENT!="nietNodig"]), OWMIDENT := GeoObject.code]
  # complement EAGIDENT 2 aggregated results
  z[Identificatie %in% unique(meetpuntenGebied$EAGIDENT[meetpuntenGebied$EAGIDENT!=""]), EAGIDENT := Identificatie]
  
  #toevoegen unieke ID voor geaggregeerde toetsing (aggregated = without NL11 prefix = Identificatie = Meetobject.lokaalID)
  z[is.na(z$HoortBijGeoobject.identificatie), HoortBijGeoobject.identificatie:= Identificatie] 
  
  # remove data which cannot be mapped 2 location, eag or owm (fish data)
  checkloc <-  z[is.na(EAGIDENT)&is.na(OWMIDENT),]
  if(nrow(checkloc)>1){
    print(paste0('warning: locatie ', unique(checkloc$Identificatie), ' komt wel voor in toetsresultaten maar niet in meetpuntbestand'))}
  z <- z[!is.na(EAGIDENT) | !is.na(OWMIDENT),]
  
  return(z)
}
# pre process EKR information by adding names for filtering
ppr_ekr <- function(krwset, ovwatset, eag_wl, doelen){

  # combine both EKR from KRW and overig water into one data.table
  db <- data.table::rbindlist(list(krwset,ovwatset), fill=TRUE)
  EKRset$jaar <- as.numeric(EKRset$jaar) 
 
  # delte rows without information, rijen weg zonder informatie
  db <- db[!is.na(db$Numeriekewaarde),]

  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]

  # remove some columns based on judgement Gerard
  cols <- c('MEETNET_HISTORIE','REFERENTIEVLAK','GLOBALID','GN_CREATED_USER','GN_CREATED_DATE',
            'GN_LAST_EDITED_USER','GN_LAST_EDITED_DATE','MEETNET_ACTUEEL','FEWSFILTER_HISTORIE',
            'FEWSFILTER_ACTUEEL','PROGRAMMA_HISTORIE','PROGRAMMA_ACTUEEL','.',
            'LigtInGeoObjectCode','ï..Meetobject.namespace','CAS.nummer',
            'Begintijd','Eindtijd','Doel','bronddoel',"HandelingsperspectiefWBP",'KRWwatertype.code.y')
  # ensure that cols are present in colnames db
  cols <- cols[cols %in% colnames(db)]
  # remove columns
  db[,c(cols):=NULL]
  db[,GHPR := gsub(' $','',GHPR)]
  
  # merge EAG and KRW naam
  db <- merge(db, unique(eag_wl[,c("KRW_SGBP3","SGBP3_NAAM")]), by.x = "OWMIDENT", by.y = "KRW_SGBP3", all.x =T)
  db <- merge(db, eag_wl[,c("GAFIDENT","GAFNAAM")], by.x = "EAGIDENT", by.y = "GAFIDENT", all.x =T)
  # add naam van een toetsgebied (WL of EAG naam)
  db[,waterlichaam := fifelse(!(is.na(SGBP3_NAAM)|SGBP3_NAAM == ""), SGBP3_NAAM, GAFNAAM)]
   
  # make local copy (only within this function)
  doelen1 <- copy(doelen)
  # mean GEP per id (en niet per eag zoals in de doelenset staat)
  doelgeb <- doelen1[,.(GEP = mean(Doel,na.rm=TRUE), GEP_2022 = mean(Doel_2022,na.rm=TRUE)), by =.(HoortBijGeoobject.identificatie, brondoel_2022, GHPR)]
  # make copy, add new id where NL11_ is removed
  doelgeb2 <- copy(doelgeb)
  doelgeb2$HoortBijGeoobject.identificatie <- gsub("^NL11_*","",doelgeb2$HoortBijGeoobject.identificatie)
  doelgeb <- rbind(doelgeb,doelgeb2)
  # merge with doelen
  db <- merge(db, doelgeb, by.x = c('HoortBijGeoobject.identificatie','GHPR'), by.y = c('HoortBijGeoobject.identificatie','GHPR'), all.x = TRUE)

  # namen aanpassen
  db[,facet_wrap_code := as.factor(gsub("Maatlatten2018 ","",Waardebepalingsmethode.code))]

  return(db)
}

# calc functions -------------------------
# calculate mean ekr score per year, calc mean score last 3 measured years, calculate oordeel (score compared 2 goals) & calculate reference score 
tabelEKRPerWLEnEAGPerJaar <- function (EKRset, detail = "deel", minjaar = 2008){
  # make local copy (only within this function)
  db <- EKRset[EKRset$jaar > 2005, ]
  
  if(detail == "hoofd"){
    db <-  db[db$level == 1,]}
  
  # col group per jaar
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code','Waardebepalingsmethode.code',
               'facet_wrap_code','GHPR_level','GHPR','level','jaar','GEP','GEP_2022','waterlichaam','OWMIDENT')
  # rename columns and order data.table
  setnames(db,colgroup,c('id','EAGIDENT','watertype','wbmethode','facet_wrap_code','GHPR_level',
                         'GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3'))
  # columns to group with year
  colgroup <- c('id','EAGIDENT','watertype','wbmethode','facet_wrap_code','GHPR_level',
                'GHPR','level','jaar','GEP','GEP_2022','waterlichaam','KRW_SGBP3')
  # columns to group alle meetpunten over de jaren
  colg <- c('EAGIDENT','id','watertype','GHPR_level','GHPR','level','wbmethode','facet_wrap_code', 'GEP', 'GEP_2022', 'waterlichaam', 'KRW_SGBP3')
  db <- db[,lastyear := max(jaar),by = colg]
  
  # gemiddelde ekr per jaar voor draaitabel en selectie laatste 3 meetjaren
  d1 <- db[,.(EKRmean = mean(Numeriekewaarde,na.rm=T), EKRmedian = quantile(Numeriekewaarde,probs = c(0.50), na.rm=T), EKRperc90 = quantile(Numeriekewaarde,probs = c(0.90), na.rm=T), EKRperc95 = quantile(Numeriekewaarde,probs = c(0.95), na.rm=T)), by = c(colgroup,"lastyear")]
  d1$hdlprsp <- d1$EKRperc90 -d1$EKRmedian
  d1[ ,hdlprsp.mean.over.year := mean(hdlprsp), by = c('EAGIDENT','id','watertype','GHPR_level','GHPR','wbmethode')]
  # percentielen van alle meetlocaties (zowel meetpunt als geaggr per eag of waterlichaam) in alle jaren per gebied, 2 inspect best sites
  d2 <- db[,.(EKRmean = mean(Numeriekewaarde,na.rm=T), EKRmedian = quantile(Numeriekewaarde,probs = c(0.50), na.rm=T), EKRperc90 = quantile(Numeriekewaarde,probs = c(0.90), na.rm=T), EKRperc95 = quantile(Numeriekewaarde,probs = c(0.95), na.rm=T)), by = colg]
  d2$hdlprsp.mean.all.year <- d2$EKRperc90 -d2$EKRmedian
  # draaitabel voor wide format per jaar
  d3 <- dcast(d1, id+EAGIDENT+watertype+GHPR_level+GHPR+wbmethode+hdlprsp.mean.over.year+lastyear ~ jaar, value.var = c("EKRmean"), fun.aggregate = mean)
  # merge per jaar en percentielen over alle jaren (die niet worden gebruikt voor handelingsperspectief)
  d3 <- merge(d2, d3, by = c('EAGIDENT','id','watertype','GHPR_level','GHPR','wbmethode'))
  
  setorder(d1,id,EAGIDENT,watertype,wbmethode,GHPR_level,-jaar)
  # add year number (given ordered set), and take only three most recent years
  d1b <- d1[jaar > minjaar, yearid := seq_len(.N),by = colg][yearid < 4]
  # calculate mean EKR per group over the three years = krw score formeel die wordt vergeleken met doel
  d1b <- d1b[,.(EKR3jr = mean(EKRmean,na.rm=T)),by = colg]
  
  # add year number (given ordered set), and take only three first years
  setorder(d1,id,EAGIDENT,watertype,wbmethode,GHPR_level,jaar)
  d1$yearid <- NULL
  d1a <- d1[jaar < 2014, yearid := seq_len(.N),by = colg][yearid < 4]
  # calculate mean EKR per group over the three years = krw score formeel die wordt vergeleken met doel
  d1a <- d1a[,.(EKRref = mean(EKRmean,na.rm=T)),by = colg]
  
  # merge per jaar en percentielen
  d3 <- merge(d1b, d3, by = c('EAGIDENT','id','watertype','GHPR_level','GHPR','level','wbmethode','facet_wrap_code','GEP','GEP_2022','waterlichaam','KRW_SGBP3'), all.y = TRUE)
  d3 <- merge(d1a, d3, by = c('EAGIDENT','id','watertype','GHPR_level','GHPR','level','wbmethode','facet_wrap_code','GEP','GEP_2022','waterlichaam','KRW_SGBP3'), all.y = TRUE)
  
  # add classification for GEP (goals WBP 1+2)
  d3[EKR3jr < GEP/3,oordeel := 'slecht']
  d3[EKR3jr >= GEP/3 & EKR3jr < 2 * GEP / 3,oordeel := 'ontoereikend']
  d3[EKR3jr >= 2 * GEP / 3,oordeel := 'matig']
  d3[EKR3jr >= GEP, oordeel := 'goed']
  
  # add classification for GEP 2022 (goals WBP3)
  d3[EKR3jr < GEP_2022/3,oordeel_2022 := 'slecht']
  d3[EKR3jr >= GEP_2022/3 & EKR3jr < 2 * GEP_2022 / 3, oordeel_2022 := 'ontoereikend']
  d3[EKR3jr >= 2 * GEP_2022 / 3,oordeel_2022 := 'matig']
  d3[EKR3jr >= GEP_2022, oordeel_2022 := 'goed']
  # add a sorting column
  d3[ ,oordeelsort := EKR3jr / GEP_2022] 
  
  # add classification for calculated goals based on best sites
  d3$doelhndprs <- d3$EKR3jr + d3$hdlprsp.mean.over.year
  d3[EKR3jr < doelhndprs/3, oordeel_hndprs := 'slecht']
  d3[EKR3jr >= doelhndprs/3 & EKR3jr < 2 * doelhndprs / 3, oordeel_hndprs := 'ontoereikend']
  d3[EKR3jr >= 2 * doelhndprs / 3,oordeel_hndprs := 'matig']
  d3[EKR3jr >= doelhndprs, oordeel_hndprs := 'goed']
  
  # add classification for reference EKR (starting point KRW legislation/ first scores)
  d3[EKRref < GEP_2022/3,oordeel_ref := 'slecht']
  d3[EKRref >= GEP_2022/3 & EKR3jr < 2 * GEP_2022 / 3, oordeel_ref := 'ontoereikend']
  d3[EKRref >= 2 * GEP_2022 / 3,oordeel_ref := 'matig']
  d3[EKRref >= GEP_2022, oordeel_ref := 'goed']
  
  # zoek laagste oordeel per hoofdmaatlat/ lowest score per KRW metric
  d3 <- d3[!is.na(id) & level == 1, minscore := oordeelsort==min(oordeelsort,na.rm=T), by = c('id','EAGIDENT')]
  # zoek laagste score per (deel)maatlat waarbij oever en drijfblad niet meedoen 
  d3 <- d3[wbmethode =="Maatlatten2018 Ov. waterflora" &
                             level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')), minscore := EKR3jr==min(EKR3jr,na.rm=T), by = c('id','EAGIDENT','facet_wrap_code','level')] 
  
  write.table(d3, file = paste(getwd(),"/output/EKROordeelPerGebiedJaarWide",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(d3)
}
# calculate trends EKR scores over time 
trendkrw <- function(EKRset, agglevel = 'EAG'){ 
  
  EKRset$jaarlm <- as.numeric(EKRset$jaar-2006)
  # add last year when measures were executed
  # columns to group alle meetpunten over de jaren
  if(agglevel == 'EAG'){colg <- c('EAGIDENT','HoortBijGeoobject.identificatie','KRWwatertype.code','GHPR_level','facet_wrap_code', 'GEP', 'GEP_2022', 'waterlichaam', 'SGBP3_NAAM')}
  if(agglevel == 'KRW'){colg <- c('HoortBijGeoobject.identificatie','KRWwatertype.code','GHPR_level','facet_wrap_code', 'GEP', 'GEP_2022', 'waterlichaam', 'SGBP3_NAAM')}
  
  EKRset <- EKRset[,lastyear := max(jaar), by = colg]
  
  # calculate trend EKR per metric and EAG
  setDT(EKRset)[, c('intc', 'slope') := 
                        as.list(coef(lm(Numeriekewaarde ~ jaarlm, na.action = NULL))), by = colg]
  tb <- setDT(EKRset)[,as.list(broom::glance(lm(Numeriekewaarde ~ jaarlm, na.action = NULL))), by = c(colg , 'lastyear','intc', 'slope')]
  
  # adjust slope for trend in 6 years (planperiode)
  tb$estimate <- tb$slope*6
  # filter significance value (<0.1 = weak evidence or a trend) en relevant trends
  tb <- tb[tb$p.value > 0.1 &  tb$lastyear < 2015, estimate := NaN ]
  tb$estimate <- round(tb$estimate, digits = 2)
  
  # col group per jaar
  colgroup <-c('HoortBijGeoobject.identificatie','EAGIDENT','KRWwatertype.code')
    # rename columns and order data.table
  setnames(tb,colgroup,c('id','EAGIDENT','watertype'))
  
  # 2do check4  normality of residuals
  res <- setDT(EKRset)[,residuals(lm(Numeriekewaarde ~ jaarlm, na.action = NULL)), by = colg]
 
  return(tb)
}

# visualisations 4 KRW-app -----------------
# plot EKR per EAG
ppr_ekrplot_EAG <- function(ekr_score){
  
  # make local copy
  dt <- copy(ekr_score)
  
  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := paste0(EAGIDENT," ",facet_wrap_code)]
  
  # build background [Kan eleganter..]
  bg <- unique(dt[, c("id", "GEP_2022", "wlmt")])
  
  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]
  
  # reformat
  bg_gather <- melt(bg,id.vars = c('id','GEP_2022','wlmt'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast.data.table(bg_gather,id+GEP_2022+wlmt+doelen+sgbp_version~varrange,value.var='waarde')
  
  # add sgbp version
  bg_spr[sgbp_version=='new',sgbp_version := 'SGBP3']
  bg_spr[,Oordeel := as.factor(doelen)]
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)
  
  ## make plot
  plot <- ggplot(dt, aes(x = id, y = EKR3jr)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1,
                     y = EKR3jr, yend = EKR3jr, linetype = "Huidige toestand"),
                 col = "black", cex = 1.4) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(wlmt)) +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.x = element_text(size = 8), # maatlat
          #strip.text.y = element_text(size = 6), # y as
          axis.text.x = element_blank(), #
          axis.text.y = element_text(size= 7), # ekrscores
          axis.title = element_text(size= 7),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 8),
          legend.text  = element_text(size = 7),
          legend.position = "bottom")
  return(plot)
}
# plot EKR per waterbody or EAG if its overig water
ppr_ekrplot_gebied <- function(ekr_score){

  # make local copy
  dt <- copy(ekr_score)

  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := paste0(waterlichaam," ",facet_wrap_code)]

  # build background [Kan eleganter..]
  bg <- unique(dt[, c("id", "GEP_2022", "wlmt")])

  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]

  # reformat
  bg_gather <- melt(bg,id.vars = c('id','GEP_2022','wlmt'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast.data.table(bg_gather,id+GEP_2022+wlmt+doelen+sgbp_version~varrange,value.var='waarde')

  # add sgbp version
  bg_spr[sgbp_version=='new',sgbp_version := 'SGBP3']
  bg_spr[,Oordeel := as.factor(doelen)]

  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)

  ## make plot
  plot <- ggplot(dt, aes(x = id, y = EKR3jr)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1,
                     y = EKR3jr, yend = EKR3jr, linetype = "Huidige toestand"),
                 col = "black", cex = 1.4) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_linetype_manual("",values= c("Huidige toestand" = 1))+
    facet_grid(cols = vars(wlmt)) +
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          strip.text.x = element_text(size = 8), # maatlat
          #strip.text.y = element_text(size = 6), # y as
          axis.text.x = element_blank(), #
          axis.text.y = element_text(size= 7), # ekrscores
          axis.title = element_text(size= 7),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 8),
          legend.text  = element_text(size = 7),
          legend.position = "bottom")
  return(plot)
}
# plot EKR background
plotEKRlijnfs <- function(z, gebied = NULL){

  if(!is.null(gebied)){
    z <- z[!is.na(z$EAGIDENT),]
    z$OWMIDENT <- ""
  }

  z <- z %>%
    dplyr::arrange(GHPR_level) %>%               # sort your dataframe
    dplyr::mutate(GHPR = factor(GHPR, unique(GHPR))) # reset your factor-column based on that order
  z$jaar <- as.numeric(z$jaar)
  z$Numeriekewaarde <- as.numeric(z$Numeriekewaarde)
  z$facet_wrap_code <- as.character(z$facet_wrap_code)
  z$wlmt <- ifelse(is.na(z$OWMIDENT)|z$OWMIDENT == "", paste0(z$EAGIDENT," ",z$facet_wrap_code), z$facet_wrap_code)

  z <- z %>%
    dplyr::group_by(waterlichaam, wlmt ,GHPR , GHPR_level, level, jaar) %>%
    dplyr::summarise_at(c('Numeriekewaarde'),mean)

  z <- z %>%
    ungroup(waterlichaam)

  ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col = GHPR, group = GHPR))+
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    scale_y_continuous(limits= c(0, 1), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1))+
    scale_x_continuous(breaks=c(2006, 2008, 2010, 2012, 2014,2016,2018,2020))+
    facet_grid(vars(level),vars(wlmt), scales = "free")+
    ylab('')+xlab('')+
    guides(col=guide_legend(title=""))+
    ggtitle("", subtitle = z$waterlichaam)+
    theme_bw()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 2), #level
      axis.text.x = element_text(size= 7, angle=90), #jaar
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 8),
      legend.text  = element_text(size = 9),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right"
      )
    
}
# fractieplots
# 4 tab per EAG
plotFractiePerMaatlatFacetEAG <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgebied meenemen
  l<- l[is.na(l$Monster.lokaalID)|l$Monster.lokaalID == "",] # alleen scores per meetCODE per jaar
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  l$klasse <- factor(l$klasse, levels = c("3", "4", "5", "6","7"), labels = c("0.8-1","0.6-0.8","0.4-0.6","0.2-0.4","0-0.2"))
  
  ggplot(l, aes(x = GHPR_level, fill = klasse)) +
    geom_bar(position = "fill") +
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c("0.8-1"="blue","0.6-0.8"="green","0.4-0.6"="yellow","0.2-0.4"="orange","0-0.2"="red"))+
    guides(fill=guide_legend(title='EKR score'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 6), #level
      axis.text.x = element_text(size= 6, angle=90), #jaar
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 7),
      legend.text  = element_text(size = 6),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")+
    coord_flip()+
    facet_grid(jaar~facet_wrap_code+EAGIDENT)+
    labs(x="",y="")
}
# 4 tab per waterbody
plotFractiePerMaatlat <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgeied meenemen
  l<- l[is.na(l$Monster.lokaalID)|l$Monster.lokaalID == "",] # alleen scores per meetCODE per jaar
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  #titel = paste(unique(l[ ,c('HoortBijGeoobject.identificatie','KRWwatertype.code')]),sep="",collapse=" ")
  l$klasse <- factor(l$klasse, levels = c("3", "4", "5", "6","7"), labels = c("0.8-1","0.6-0.8","0.4-0.6","0.2-0.4","0-0.2"))
  
  ggplot(l, aes(x = GHPR_level, fill = klasse)) +
    geom_bar(position = "fill") +
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c("0.8-1"="blue","0.6-0.8"="green","0.4-0.6"="yellow","0.2-0.4"="orange","0-0.2"="red"))+
    guides(fill=guide_legend(title='EKR score'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 6), #level
      axis.text.x = element_text(size= 6, angle=90), #jaar
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 7),
      legend.text  = element_text(size = 6),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")+
    coord_flip()+
    facet_grid(jaar~facet_wrap_code)+
    #ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="")
}

# visualisations atlas current state/ toestand aquatic ecology--------------
# kaart ekr3jr, oordeel, doel per EAG
KRWmapEAG <- function(gEAG, ekr_scores2, maatlat = "2V1 Overige waterflora", param = "oordeelv2" ){
  
  gebiedData <- ekr_scores2[!is.na(ekr_scores2$EAGIDENT),]
  gebiedData <- gebiedData[gebiedData$GHPR_level %in% maatlat,]
  
  if(param == 'oordeel'){
    gebiedData$param <- as.factor(gebiedData$oordeel_2022)
    gebiedData$param = factor(gebiedData$param, levels = c( "goed", "matig", "ontoereikend","slecht"))
    gebiedData <- gebiedData[!is.na(gebiedData$oordeel_2022),]
    
    col <- c('goed'="green",'matig'="yellow",'ontoereikend'="orange",'slecht'="red")
    labels <- c('goed'='goed','matig'='matig','ontoereikend'='ontoereikend','slecht'='slecht')
  }
  
  if(param == 'oordeelv2'){
    gebiedData$param <- as.factor(gebiedData$oordeel_hndprs)
    gebiedData$param = factor(gebiedData$param, levels = c( "goed", "matig", "ontoereikend","slecht"))
    gebiedData <- gebiedData[!is.na(gebiedData$oordeel_hndprs),]
    
    col <- c('goed'="green",'matig'="yellow",'ontoereikend'="orange",'slecht'="red")
    labels <- c('goed'='goed','matig'='matig','ontoereikend'='ontoereikend','slecht'='slecht')
  }
  
  if(param == 'oordeeloud'){
    gebiedData$param <- as.factor(gebiedData$oordeel)
    gebiedData$param = factor(gebiedData$param, levels = c( "goed", "matig", "ontoereikend","slecht"))
    gebiedData <- gebiedData[!is.na(gebiedData$oordeel),]
    
    col <- c('goed'="green",'matig'="yellow",'ontoereikend'="orange",'slecht'="red")
    labels <- c('goed'='goed','matig'='matig','ontoereikend'='ontoereikend','slecht'='slecht')
  }
  
  if(param == 'ekr3jr'){
    '7' -> gebiedData$klasse[gebiedData$EKR3jr < 0.2]
    '6' -> gebiedData$klasse[gebiedData$EKR3jr >= 0.2 & gebiedData$EKR3jr < 0.4]
    '5' -> gebiedData$klasse[gebiedData$EKR3jr >= 0.4 & gebiedData$EKR3jr < 0.6]
    '4' -> gebiedData$klasse[gebiedData$EKR3jr >= 0.6 & gebiedData$EKR3jr < 0.8]
    '3' -> gebiedData$klasse[gebiedData$EKR3jr >= 0.8]
    gebiedData$param <- as.factor(gebiedData$klasse)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7"))
    col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
    labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")
    gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  }
  
  if(param == 'ekrref'){
    '7' -> gebiedData$klasse[gebiedData$EKRref < 0.2]
    '6' -> gebiedData$klasse[gebiedData$EKRref >= 0.2 & gebiedData$EKRref < 0.4]
    '5' -> gebiedData$klasse[gebiedData$EKRref >= 0.4 & gebiedData$EKRref < 0.6]
    '4' -> gebiedData$klasse[gebiedData$EKRref >= 0.6 & gebiedData$EKRref < 0.8]
    '3' -> gebiedData$klasse[gebiedData$EKRref >= 0.8]
    gebiedData$param <- as.factor(gebiedData$klasse)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7"))
    col <- c('3'="blue",'4'="green",'5'="yellow",'6'="orange",'7'="red")
    labels <- c('3'="0.8-1",'4'="0.6-0.8",'5'="0.4-0.6",'6'="0.2-0.4",'7'="0-0.2")
    gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  }
  
  if(param == 'doel'){
    #gebiedData <- doelen  
    #gebiedData$GEP_2022 <-   gebiedData$Doel_2022_v2
    '8' -> gebiedData$klasse[gebiedData$GEP_2022 < 0.2]
    '7' -> gebiedData$klasse[gebiedData$GEP_2022 >= 0.2 & gebiedData$GEP_2022 < 0.3]
    '6' -> gebiedData$klasse[gebiedData$GEP_2022 >= 0.3 & gebiedData$GEP_2022 < 0.4]
    '5' -> gebiedData$klasse[gebiedData$GEP_2022 >= 0.4 & gebiedData$GEP_2022 < 0.5]
    '4' -> gebiedData$klasse[gebiedData$GEP_2022 >= 0.5 & gebiedData$GEP_2022 < 0.6]
    '3' -> gebiedData$klasse[gebiedData$GEP_2022 >= 0.6]
    gebiedData$param <- as.factor(gebiedData$klasse)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7","8"))
    gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
    col <- c('3'="deepskyblue",'4'="seagreen",'5'="gold",'6'="orange",'7'="chocolate", "8"="orangered")
    labels <- c('3'='>0.6', '4'='0.5-0.6','5'='0.4-0.5','6'='0.3-0.4','7'='0.2-0.3', '8'='0-0.2')
  }
  
  if(param == 'doel_gemhndprs'){
    '8' -> gebiedData$klasse[gebiedData$doelhndprs < 0.2]
    '7' -> gebiedData$klasse[gebiedData$doelhndprs >= 0.2 & gebiedData$doelhndprs < 0.3]
    '6' -> gebiedData$klasse[gebiedData$doelhndprs >= 0.3 & gebiedData$doelhndprs < 0.4]
    '5' -> gebiedData$klasse[gebiedData$doelhndprs >= 0.4 & gebiedData$doelhndprs < 0.5]
    '4' -> gebiedData$klasse[gebiedData$doelhndprs >= 0.5 & gebiedData$doelhndprs < 0.6]
    '3' -> gebiedData$klasse[gebiedData$doelhndprs >= 0.6]
    gebiedData$param <- as.factor(gebiedData$klasse)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7","8"))
    gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
    col <- c('3'="deepskyblue",'4'="seagreen",'5'="gold",'6'="orange",'7'="chocolate", "8"="orangered")
    labels <- c('3'='>0.6', '4'='0.5-0.6','5'='0.4-0.5','6'='0.3-0.4','7'='0.2-0.3', '8'='0-0.2')
  }
  
  if(param == 'doeloud'){
    '8' -> gebiedData$klasse[gebiedData$GEP < 0.2]
    '7' -> gebiedData$klasse[gebiedData$GEP >= 0.2 & gebiedData$GEP < 0.3]
    '6' -> gebiedData$klasse[gebiedData$GEP >= 0.3 & gebiedData$GEP < 0.4]
    '5' -> gebiedData$klasse[gebiedData$GEP >= 0.4 & gebiedData$GEP < 0.5]
    '4' -> gebiedData$klasse[gebiedData$GEP >= 0.5 & gebiedData$GEP < 0.6]
    '3' -> gebiedData$klasse[gebiedData$GEP >= 0.6]
    gebiedData$param <- as.factor(gebiedData$klasse)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7","8"))
    gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
    col <- c('3'="deepskyblue",'4'="seagreen",'5'="gold",'6'="orange",'7'="chocolate", "8"="orangered")
    labels <- c('3'='>0.6', '4'='0.5-0.6','5'='0.4-0.5','6'='0.3-0.4','7'='0.2-0.3', '8'='0-0.2')
  }
  
  if(param == 'doelgatoud'){
    gebiedData$doelgat <- gebiedData$GEP-gebiedData$EKR3jr
    '7' -> gebiedData$doelgat[gebiedData$doelgat >= 0.2 & gebiedData$doelgat < 1]
    '6' -> gebiedData$doelgat[gebiedData$doelgat < 0.2 & gebiedData$doelgat >= 0.1]
    '5' -> gebiedData$doelgat[gebiedData$doelgat < 0.1 & gebiedData$doelgat >= 0.05]
    '4' -> gebiedData$doelgat[gebiedData$doelgat < 0.05 & gebiedData$doelgat > 0.025]
    '3' -> gebiedData$doelgat[gebiedData$doelgat <= 0.025]
    gebiedData$param <- as.factor(gebiedData$doelgat)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7"))
    gebiedData <- gebiedData[!is.na(gebiedData$param),]
    col <- c('3'="chartreuse",'4'="gold",'5'="orange",'6'="darkorange",'7'="saddlebrown")
    labels <- c('3'='<=0.025','4'='0.025-0.05','5'='0.05-0.1','6'='0.1-0.2','7'='>0.2')
  }
  
  if(param == 'doelgat'){
    gebiedData$doelgat <- gebiedData$GEP_2022-gebiedData$EKR3jr
    '7' -> gebiedData$doelgat[gebiedData$doelgat >= 0.2 & gebiedData$doelgat < 1]
    '6' -> gebiedData$doelgat[gebiedData$doelgat < 0.2 & gebiedData$doelgat >= 0.1]
    '5' -> gebiedData$doelgat[gebiedData$doelgat < 0.1 & gebiedData$doelgat >= 0.05]
    '4' -> gebiedData$doelgat[gebiedData$doelgat < 0.05 & gebiedData$doelgat > 0.025]
    '3' -> gebiedData$doelgat[gebiedData$doelgat <= 0.025]
    gebiedData$param <- as.factor(gebiedData$doelgat)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7"))
    gebiedData <- gebiedData[!is.na(gebiedData$param),]
    col <- c('3'="chartreuse",'4'="gold",'5'="orange",'6'="darkorange",'7'="saddlebrown")
    labels <- c('3'='<=0.025','4'='0.025-0.05','5'='0.05-0.1','6'='0.1-0.2','7'='>0.2')
  }
  
  if(param == 'doelgatv2'){
    gebiedData$doelgat <- gebiedData$hdlprsp
    '7' -> gebiedData$doelgat[gebiedData$doelgat >= 0.2 & gebiedData$doelgat < 1]
    '6' -> gebiedData$doelgat[gebiedData$doelgat < 0.2 & gebiedData$doelgat >= 0.1]
    '5' -> gebiedData$doelgat[gebiedData$doelgat < 0.1 & gebiedData$doelgat >= 0.05]
    '4' -> gebiedData$doelgat[gebiedData$doelgat < 0.05 & gebiedData$doelgat > 0.025]
    '3' -> gebiedData$doelgat[gebiedData$doelgat <= 0.025]
    gebiedData$param <- as.factor(gebiedData$doelgat)
    gebiedData$param = factor(gebiedData$param, levels = c("3", "4", "5", "6","7"))
    gebiedData <- gebiedData[!is.na(gebiedData$param),]
    col <- c('3'="chartreuse",'4'="gold",'5'="orange",'6'="darkorange",'7'="saddlebrown")
    labels <- c('3'='<=0.025','4'='0.025-0.05','5'='0.05-0.1','6'='0.1-0.2','7'='>0.2')
  }
  
  pal <- colorFactor(palette = col,  domain = gebiedData$param)
  map <- sp::merge(gEAG, gebiedData[, c('param','EKR3jr','EKRref','oordeel','oordeel_2022','EAGIDENT','GEP','GEP_2022','watertype',
                                        'GHPR_level','EKRperc90')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  
  map2 <- map[map$GAFIDENT %in% c('3000-EAG-3','3000-EAG-4','3000-EAG-2','2000-EAG-7','2000-EAG-2','2000-EAG-3','2000-EAG-4','2000-EAG-5','2000-EAG-6'),]
  
  
  leaflet() %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                                 "EKR score:", map$EKR3jr, "<br>",
                                                                 "Referentiescore:", map$EKRref, "<br>",
                                                                 "Oordeel:", map$oordeel_2022, "<br>",
                                                                 "Doel:", map$GEP_2022, "<br>",
                                                                 "Percentiel90:", map$EKRperc90, "<br>",
                                                                 "Maatlat:", map$GHPR_level ),
                stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(param), fillOpacity = 0.6) %>%
    addPolygons(data= map2, layerId = map2$GAFIDENT, popup= paste("EAG naam", map2$GAFNAAM, "<br>",
                                                                  "EKR score:", map2$EKR3jr, "<br>",
                                                                  "Referentiescore:", map2$EKRref, "<br>",
                                                                  "Oordeel:", map2$oordeel_2022, "<br>",
                                                                  "Doel:", map2$GEP_2022, "<br>",
                                                                  "Percentiel90:", map2$EKRperc90.allejaren, "<br>",
                                                                  "Maatlat:", map2$GHPR_level),
                stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(param), fillOpacity = 1) %>%
    addLegend("bottomright", colors=col, labels=labels, title = param)%>%
    addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
}
# plot trend per EAG
plottrendEAG <- function(gebiedData, gEAG, maatlat = "Macrofauna-kwaliteit"){
  
  # gebiedData <- trendekreag[trendekreag$GHPR_level == '2V21 Soortensamenstelling macrofyten',]
  gebiedData <- gebiedData[GHPR %in% maatlat,]
  '1' -> gebiedData$klasse[gebiedData$estimate < -0.3]
  '2' -> gebiedData$klasse[gebiedData$estimate >= -0.3 & gebiedData$estimate < -0.05]
  '3' -> gebiedData$klasse[gebiedData$estimate >= -0.05 & gebiedData$estimate < 0.05]
  '5' -> gebiedData$klasse[gebiedData$estimate >= 0.05 & gebiedData$estimate < 0.3]
  '6' -> gebiedData$klasse[gebiedData$estimate >= 0.3]
  '7' -> gebiedData$klasse[gebiedData$p.value > 0.1] # geen trend
  '8' -> gebiedData$klasse[is.na(gebiedData$p.value)] # slechts 1 of 2 jaar data
  gebiedData$klasse[gebiedData$r.squared == 1] <- "8"
  gebiedData$klasse <- factor(gebiedData$klasse, levels = c("1", "2", "3", "5","6","7","8"))
  
  col <- c('1'= 'darkred','2'="red", '3'="#fff7bc",'5'="green",'6'="darkgreen",'7' ="lightyellow",'8'="grey")
  labels <- c('1'="< -0.3",'2'="-0.3 - -0.05" ,'3'="geen relevante trend",'5'="0.05 - 0.3",
              '6'=">0.3",'7' ="geen significante trend",'8'="onvoldoende data")
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  map <- sp::merge(gEAG, gebiedData[, c('klasse','estimate','p.value','r.squared','EAGIDENT', 'watertype', 'GHPR',
                                        'GHPR_level','facet_wrap_code')], by.x = 'GAFIDENT', by.y = 'EAGIDENT'
                   ,duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "EAG code", map$GAFIDENT, "<br>",
                                                     "EKR trend:", map$estimate, "<br>",
                                                     "trend significantie:", map$p.value, "<br>",
                                                     "R2:", map$r.squared, "<br>"
    ),
    stroke = T, color= 'black', opacity=0.8, weight = 1, smoothFactor = 0.8,
    fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors = col, labels = labels, title = unique(map$GHPR))%>%
    addProviderTiles("Esri.WorldGrayCanvas")
}
plottrendKRW <- function(gebiedData, gKRW){
  # gebiedData <- ekrtrend[ekrtrend$GHPR_level %in% c('4VI2 Massafractie Visgilde - plantminnende soort (Pm)','4VI2 Soortenrijkdom Visgilde - plantminnende en migrerende soort (PmM)'),]
  
  pl <- gKRW %>% dplyr::group_by(OWMIDENT, OWMNAAM) 
 
  '1' -> gebiedData$klasse[gebiedData$estimate < -0.3]
  '2' -> gebiedData$klasse[gebiedData$estimate >= -0.3 & gebiedData$estimate < -0.05]
  '3' -> gebiedData$klasse[gebiedData$estimate >= -0.05 & gebiedData$estimate < 0.05]
  '5' -> gebiedData$klasse[gebiedData$estimate >= 0.05 & gebiedData$estimate < 0.3]
  '6' -> gebiedData$klasse[gebiedData$estimate >= 0.3]
  '7' -> gebiedData$klasse[gebiedData$p.value > 0.1] # geen trend
  '8' -> gebiedData$klasse[is.na(gebiedData$p.value)] # slechts 1 of 2 jaar data
  gebiedData$klasse[gebiedData$r.squared == 1] <- "8"
  gebiedData$klasse <- factor(gebiedData$klasse, levels = c("1", "2", "3", "5","6","7","8"))
  
  col <- c('1'= 'darkred','2'="red", '3'="#fff7bc",'5'="green",'6'="darkgreen",'7' ="lightyellow",'8'="grey")
  labels <- c('1'="< -0.3",'2'="-0.3 - -0.05" ,'3'="geen relevante trend",'5'="0.05 - 0.3",
              '6'=">0.3",'7' ="geen significante trend",'8'="onvoldoende data")
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  map <- sp::merge(pl, gebiedData[, c('klasse','estimate','p.value','r.squared','KRW_SGBP3', 'watertype',
                                      'GHPR_level','facet_wrap_code')], by.x = 'OWMIDENT', by.y =
                     'KRW_SGBP3', duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("KRW naam", map$OWMNAAM, "<br>",
                                                     "EAG code", map$OWMIDENT, "<br>",
                                                     "EKR trend:", map$estimate, "<br>",
                                                     "trend significantie:", map$p.value, "<br>",
                                                     "R2:", map$r.squared, "<br>"
    ),
    stroke = T, color= ~pal(klasse), opacity=0.8, weight = 1, smoothFactor = 0.8,
    fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors = col, labels = labels, title = unique(map$GHPR))%>%
    addProviderTiles("Esri.WorldGrayCanvas")
}
# ekr per eag en mp
ekrmap_mpeag <- function(EKRset, gEAG, maatlat = "2V1 Overige waterflora", col=col, labels=labels){
  # gebiedData <- EKRset[EKRset$jaar >= '2006' & EKRset$jaar < '2009',]
  #select maatlat, filter mp met coords, transform voor leaflet
  gebiedData <- EKRset[EKRset$GHPR_level == maatlat,]
  gebiedData <- gebiedData[!is.na(gebiedData$EAGIDENT),]
  gebiedData1 <- gebiedData[!(is.na(gebiedData$XCOORD)) & !(is.na(gebiedData$YCOORD))
                            & !is.na(gebiedData$CODE),]
  gebiedData1 <- st_transform(st_as_sf(gebiedData1,coords = c("XCOORD","YCOORD"),crs = 28992),4326)
  gebiedData1$klasse = factor(gebiedData1$klasse, levels = c("3", "4", "5", "6","7"))
  pal1 <- colorFactor(palette = col,  domain = gebiedData1$klasse)
  gebiedData1 <- gebiedData1[order(gebiedData1$jaar),]
  
  # add year number and take only most recent years (selection per EAG)
  groups <- c('EAGIDENT','HoortBijGeoobject.identificatie','waterlichaam','KRWwatertype.code','OWMIDENT','GHPR_level','GHPR','level','Waardebepalingsmethode.code') 
  db <- gebiedData[, yearid := frank(-jaar, ties.method = 'dense'), by = groups][yearid <= 1]
  # calculate mean EKR value per group per jaar
  gebiedData <- db[,.(EKR = mean(Numeriekewaarde,na.rm=TRUE),
                      GEP_2022 = mean(GEP_2022, na.rm=TRUE)), by = c(groups,'jaar')]
  
  '7' -> gebiedData$klasse[gebiedData$EKR < 0.2]
  '6' -> gebiedData$klasse[gebiedData$EKR >= 0.2 & gebiedData$EKR < 0.4]
  '5' -> gebiedData$klasse[gebiedData$EKR >= 0.4 & gebiedData$EKR < 0.6]
  '4' -> gebiedData$klasse[gebiedData$EKR >= 0.6 & gebiedData$EKR < 0.8]
  '3' -> gebiedData$klasse[gebiedData$EKR >= 0.8]
  gebiedData$klasse <- as.factor(gebiedData$klasse)
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  gebiedData <- gebiedData[!is.na(gebiedData$klasse),]
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  
  x <- as.character(sort(unique(gebiedData$jaar)))
  x <- paste0(x,' ', collapse = '')
  titel1 <- paste0("EKR scores in ",x)
  
  map <- sp::merge(gEAG, gebiedData[, c('EKR','klasse','EAGIDENT','GEP_2022','KRWwatertype.code',
                                        'GHPR_level')], by.x = 'GAFIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  leaflet() %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                                 "EKR score:", map$EKR, "<br>",
                                                                 "Doel:", map$GEP_2022),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addCircles(data = gebiedData1, popup = paste("CODE", as.character(gebiedData1$CODE), "<br>",
                                                 "EKR score:", as.character(gebiedData1$Numeriekewaarde), "<br>",
                                                 "Meetjaar:", as.character(gebiedData1$jaar)),
               radius= ~(100/(gebiedData1$jaar-2000)), color= ~pal1(klasse), fillOpacity = 0.8) %>%
    addLegend("bottomright", colors=col, labels=labels, title = titel1) %>%
    addTiles()
}
chlorofylA <- function (wq, gEAG){
  b = dcast(wq1, locatie.EAG+jaar+locatie.KRW.watertype ~ fewsparameter,
            value.var = "meetwaarde", fun.aggregate = mean, na.rm =TRUE, fill = NaN, subset = .(fewsparameter == 'CHLFa_ug_l'))
  b <- b[!is.na(b$CHLFa_ug_l)& !is.na(b$jaar) & !is.na(b$locatie.EAG) & !b$locatie.EAG == '', ]
  
  '1' -> b$CHLFAkl[b$CHLFa_ug_l > 200]
  '2' -> b$CHLFAkl[b$CHLFa_ug_l <= 200 & b$CHLFa_ug_l > 100]
  '3' -> b$CHLFAkl[b$CHLFa_ug_l <= 100 & b$CHLFa_ug_l > 50]
  '4' -> b$CHLFAkl[b$CHLFa_ug_l <= 50 & b$CHLFa_ug_l > 25]
  '5' -> b$CHLFAkl[b$CHLFa_ug_l <= 25 & b$CHLFa_ug_l > 10]
  '6' -> b$CHLFAkl[b$CHLFa_ug_l <= 10]
  b$CHLFAkl <- as.factor(b$CHLFAkl)
  
  colc <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue')
  labelsc <- c('1'=">200",'2'="200-100" ,'3'="100-50",'4'="50-25",'5'="25-10",'6' = '<10')
  pal <- colorFactor(palette = colc,  domain = b$CHLFAkl)
  map <- sp::merge(gEAG, b[, c('CHLFAkl','CHLFa_ug_l','jaar','locatie.EAG')],
                   by.x = 'GAFIDENT', by.y =
                     'locatie.EAG', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  leaflet(map) %>%
    addPolygons(layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "Chlorofyl:", map$CHLFa_ug_l, "<br>",
                                                     "jaar:", map$jaar, "<br>"),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(CHLFAkl), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=colc, labels=labelsc, title = "")%>%
    addTiles()
}
plotmafa <- function(mafa,TWNev){
  #mafa  <- hybi 
  mafa <- mafa[mafa$analysecode == 'MEA',] %>% as.data.table()
  mafa <- merge.data.frame(TWNev, mafa, by.x = 'taxonname', by.y ='TWN.naam', all.x = FALSE, all.y = TRUE) %>% as.data.table()
  # mafa <- dcast(mafa, taxongroup+locatie.EAG+datum+jaar~ ., value.var = "meetwaarde", fun.aggregate = sum, drop = TRUE)# som aantallen per groep
  mafa <- dcast(mafa, monsterident+locatie.KRW.watertype+locatie.KRWmeetpuntlocatie +taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = "locatie.EAG", fun.aggregate = lengthUnique, drop = TRUE) # gemiddelde van locaties en jaren
  mafa<- mafa[!is.na(mafa$taxongroup) & !mafa$taxongroup == "" & !is.na(mafa$.),]
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = ".", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  mafa <-as.data.table(mafa)
  mafasel <- mafa[,length(unique(jaar))>3, by=locatie.KRWmeetpuntlocatie]
  # mafasel <- mafa[,length(unique(jaar))>3, by=locatie.EAG]
  # mafa <- mafa[ocatie.EAG %in% mafasel$locatie.EAG[mafasel$V1 == TRUE]]
  mafa <- mafa[locatie.KRWmeetpuntlocatie %in% mafasel$locatie.KRWmeetpuntlocatie[mafasel$V1 == TRUE]]
  mafa <- mafa[mafa$locatie.KRW.watertype %in% c('M10','M27','M14','M20'),]
  mafa$meetwaarde <- mafa$.
  mafa <- dcast(mafa, locatie.KRW.watertype+taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  eptindex <- mafa[mafa$taxongroup %in% c("Insecta - Ephemeroptera","Insecta - Remaining","Insecta - Trichoptera"),]
  
  mafaplot <- ggplot(eptindex)+
    geom_bar(aes(x = jaar, y = ., fill = taxongroup), stat= "identity", position = "stack") +
    guides(fill= guide_legend(title='Groepen macrofauna', label.theme = element_text(size = 7), ncol = 2))+
    # facet_wrap(.~locatie.KRW.watertype, scales = 'fixed')+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 6), #EAG
      strip.text.y = element_text(size = 5), #EKR
      axis.text.x = element_text(size= 5, angle = 90),
      axis.text.y = element_text(size= 5),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      #plot.background = element_blank()
    )+
    ggtitle( "Gemiddeld aantal individuen macrofauna per monster") +
    labs(x="jaar",y="n")
  return(mafaplot)
}
# kaart ekr scores per jaar in krw waterlichaam
ekrmapKRW <- function(gebiedData, gKRW, maatlat = "4VI1 Vis-kwaliteit", col= col, labels=labels){
  #gebiedData <- EKRset[EKRset$jaar >= '2006' & EKRset$jaar < '2020' & EKRset$Waardebepalingsmethode.code == "Maatlatten2018 Vis",]
  #maatlat = "4VI1 Vis-kwaliteit"
  gebiedData <- gebiedData[!grepl('^NL11*', gebiedData$HoortBijGeoobject.identificatie),]
  #gebiedData <- gebiedData[!gebiedData$GeoObject.code == 'nietNodig',]
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  # koppel identKRW aan gebieddata
  
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  x <- as.character(sort(unique(gebiedData$jaar)))
  x <- paste0(x,' ', collapse = '')
  titel1 <- paste0("EKR scores in ",x)
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gKRW, gebiedData, by.x = 'OWMIDENT', by.y =
                     'GeoObject.code', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  
  leaflet(map) %>%
    addPolygons(layerId = map$OWMIDENT, popup= paste("Waterlichaam", map$HoortBijGeoobject.identificatie, "<br>",
                                                     "EKR score:", map$Numeriekewaarde, "<br>",
                                                     "EKR klasse:", map$klasse, "<br>",
                                                     "jaar:", map$jaar, "<br>",
                                                     "Doel:", map$Doel),
                stroke = T, color= ~pal(klasse), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=col, labels=labels, title = titel1)%>%
    addTiles()
}
krwmap <- function(gKRW, gEAG){
  
  # Find a center point for each region
  # centers <- st_centroid(gKRW, byid = TRUE)
  
  ### Create n colors for fill
  n <- length(gKRW$OWMNAAM)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  mypal <- sample(col_vector, n)
  
  leaflet() %>%
    addPolygons(data = gKRW, layerId = gKRW$OWMIDENT,
                popup = paste("naam", gKRW$OWMNAAM, "<br>", "Ident:", gKRW$OWMIDENT, "<br>", "watertype:", gKRW$watertype),
                stroke = T, color= mypal, fillColor = mypal, opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillOpacity = 0.6) %>%
    # addLabelOnlyMarkers(data = centers,
    #                     lng = ~x, lat = ~y, label = ~OWMNAAM,
    #                     labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
    addPolygons(data = gEAG, layerId = gEAG$GAFIDENT,
                stroke = T, color= 'grey' , fillColor = NA, opacity=0.8, weight = 1,
                smoothFactor = 0.8,
                fill=F, fillOpacity = 0.6)%>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    addLegend("bottomright", colors=mypal, labels=gKRW$OWMNAAM)
}
eagoverzicht <- function(gEAG, eag_wl){
  
  tabset1 <- merge(gEAG, eag_wl, by = 'GAFIDENT',  all.x = TRUE, duplicateGeoms = T)
  tabset1 <- tabset1[tabset1$KRW_SGBP3 == "",]
  setorder(tabset1,watertype)
  ### Create n colors for fill
  n <- length(unique(tabset1$watertype))
  mypal <- colorRampPalette(brewer.pal(9, "Paired"))(n-2)
  mypal1 <- colorRampPalette(brewer.pal(3, "Greys"))(1)
  mypal <- c(mypal,mypal1)
  pal <- colorFactor(palette = mypal,  domain = tabset1$watertype)
  
  # krw <- 
  leaflet(tabset1) %>%
    addPolygons(layerId = tabset1$GAFIDENT, popup= paste("naam", tabset1$GAFNAAM.x, "<br>",
                                                         "Ident:", tabset1$GAFIDENT,"<br>",
                                                         "watertype:", tabset1$watertype),
                stroke = T, color= ~pal(watertype) , fillColor = ~pal(watertype), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillOpacity = 0.6) %>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    addLegend("bottomright", pal=pal, values=tabset1$watertype)
}


# Additional visualisations stand van zaken report ------------------
ppr_slootbodem <- function(db, wtype = NULL,mlocs = NULL){
  
  # make a local copy
  db <- copy(db)
  
  # adapt few properties
  db[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[,jaar := year(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5]
  
  # merge with GAFIDENT from eag_wl (be aware: EAG 3300 are few missing)
  db <- merge.data.table(db, mlocs[,c('CODE','EAGIDENT')],by.x ='locatiecode', by.y = 'CODE')
  db <- merge.data.table(db, wtype[,c('watertype','GAFIDENT')],by.x='EAGIDENT',by.y = 'GAFIDENT',all.x = TRUE)
  
  # wijzig relevante namen van bodemfews database
  cols <- colnames(db)
  
  # adapt unit sign in parm.fews to simply reference
  db[,fewsparameter := gsub("/","_",fewsparameter)]
  
  # return updated database
  return(db)
}
ppr_pmaps <- function(dat, Overzicht_kp, hybi, nomogram, pYEAR = FALSE){
  # debug: onterecht gaf 2150, 2503 ea niet in data
  # make local copy of soil and waterbalance data
  d1 <- copy(dat)
  
  # update soiltype d1
  d1[,bodem := i_bt1]
  d1[is.na(i_bt1) & watertype %in% c('M8','M10','M27','M25'), bodem := "VEEN"]
  d1[is.na(i_bt1) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"), bodem:='KLEI']
  
  # remove initiator data and filter only recent years
  cols <- colnames(d1)[grepl('^i_|^b_',colnames(d1))]
  dg <- d1[jaar %in% 2006:2024,][,c(cols):=NULL]
  
  # addgroup and estimate meerjarig mean for numeric values
  colsg <- colnames(dg)[grepl('^a_in|^a_uit|^EAG|^GAF|^KRW$|watertype|^bodem$|^pol|^naam',colnames(dg))]
  if(pYEAR){colsg <- c(colsg,'jaar')}
  colss <- colnames(dg)[grepl('^a_|^wp_|jaar|maand|^w_|^p_|ret',colnames(dg))]
  colss <- colss[!colss %in% colsg]
  dg <- dg[,lapply(.SD,mean,na.rm=TRUE),.SDcols=colss,by=colsg]
  
  # add total sum of P load
  dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # mean water depth per EAG
  mdPtb <- hybi[jaar %in% 2012:2024 & fewsparameter == 'WATDTE_m']
  mdPtb <- mdPtb[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.EAG']
  mdPtb[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per GAF
  mdPtbG <- hybi[jaar %in% 2010:2017 & fewsparameter == 'WATDTE_m']
  mdPtbG <- mdPtbG[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.afaanvoergebied']
  mdPtbG[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # mean water depth per KRW meetpunt
  mdPtbK <- hybi[jaar %in% 2010:2017 & fewsparameter == 'WATDTE_m']
  mdPtbK <- mdPtbK[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='locatie.KRWmeetpuntlocatie']
  mdPtbK[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # merge met kP ----------------------------------------------------------
  
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte  <- merge.data.table(dg[is.na(GAF),], mdPtb, by.x = 'EAG', by.y = 'locatie.EAG', all.x = F)
  dgwatdteG <- merge.data.table(dg[is.na(EAG),], mdPtbG, by.x = 'GAF', by.y = 'locatie.afaanvoergebied', all.x = F)
  dgwatdteK <- merge.data.table(dg[is.na(EAG) & is.na(GAF),], mdPtbK, by.x = 'KRW', by.y = 'locatie.KRWmeetpuntlocatie', all.x = T)
  dgwatdte <- rbind(dgwatdte,dgwatdteG,dgwatdteK,fill = TRUE)  # mis 1 balans
  
  # update merged table
  dgwatdte[,watdte := meetwaarde][, meetwaarde := NULL]
  dgwatdte[watdte > 0.7, watdteF := '(0.5,0.7]']
  
  # retreive kP from meta-model PCditch ----
  
  # make local copy and simplify debiet column name
  dbnomogram <- copy(nomogram)
  setnames(dbnomogram,"debiet (mm/dag)","debiet",skip_absent=TRUE)
  
  # add depth category, similar to dbhybi dataset
  dbnomogram[,watdteF := cut(watdte_m, breaks = c('0','0.3','0.5','0.7','7.0'))]
  
  # model to predict kP as function of debiet (given soil and water depth)
  m1 <- lm(kP~bodemtype*watdteF*debiet*I(debiet^0.5)*I(debiet^2)*I(debiet^3),data=dbnomogram)
  
  # predict kP for dataset (suppress warnings ivm rank-deficient fit)
  suppressWarnings(dgwatdte[,kP := predict(m1,newdata = data.frame(debiet = w_debiet, bodemtype = tolower(bodem), watdteF = watdteF))])
  
  # renamed by Laura
  dgwatdte[,kPDitch := kP]
  
  # calc critical P-concentration
  dgwatdte[,PvskPDitch := wp_min_sum / kP]
  
  # koppel kp plassen obv invoertabel per EAG ----
  
  # make local copu
  kP_plas <- copy(Overzicht_kP)
  
  # relevant columns to be merged
  cols <- colnames(kP_plas)[grepl('^pc_|^lake|^p_bel|^EAG$|^GAF$',colnames(kP_plas))]
  
  # merge per EAG and per GAF, and combine both (assuming its either EAG or GAF)
  PvskPplas1 <- merge.data.table(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(EAG),!c('GAF')],
                                 kP_plas[,mget(cols)], by='EAG',all.y = TRUE,all.x = FALSE)
  PvskPplas2 <- merge.data.table(dgwatdte[watertype %in% c('M20','M27','M25',"M14") & !is.na(GAF),!c('EAG')],
                                 kP_plas[,mget(cols)],by = 'GAF',all.y = TRUE,all.x = FALSE)
  pvskp <- rbindlist(list(PvskPplas1,PvskPplas2),fill = TRUE)
  
  # merge plas kP with original water balance db
  dgwatdte <- merge.data.table(dgwatdte, pvskp[,c('pol','jaar','pc_troebel_helder', 'p_bel_year',
                                                  'pc_helder_troebel', 'lake_ditch_vol')], by = c('pol', 'jaar'), all.x = TRUE, allow.cartesian = TRUE)
  
  # calc PvskP for lakes
  dgwatdte[!is.na(p_bel_year),wp_min_sum := p_bel_year]
  dgwatdte[,PvskPlake := wp_min_sum / pc_helder_troebel]
  
  # remove rows without estimated P-belasting
  dgwatdte <- dgwatdte[!is.na(dgwatdte$pol) & !is.na(dgwatdte$wp_min_sum ),] 
  
  return(dgwatdte)
  
}
ppr_pcditch <- function(db){
  
  # make local copy
  db <- copy(db)
  
  # rename columns
  setnames(db,c('EAG','GAF','EAGnaam','plv_o2','plv','opp','diepte','fr_moeras','strijklengte',
                'debiet','inflow','extinctie','sedimenttype','pc_helder_troebel',
                'pc_troebel_helder','lake_ditch_vol','morfologie','systeemgrens','p_bel_year'))
  
  # remove columns
  cols <- c('opp','EAGnaam')
  db[,c(cols):=NULL]
  
  # return updated database
  return(db)
}
# ekr per eag en mp
ppr_mapPpercWatb <- function(pvskp, gEAG,param = 'p_uitspoel'){
  
  if(param == 'p_retentie'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(-40,-30,-20,-10,-5,-1,0,1,2,5,10,20,30,40)))}else{
    if(param == 'A_P_AL'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0,10,15,20,25,30,35,40,45,50,60)))}else{
      if(param == 'a_tot'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0,50,150,200,300,400,500,1000,2000,3000,4000)))}else{
        pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,10)))}}}
  pal <- colorFactor(palette = topo.colors(14),  domain = pvskp[[param]])
  gEAG$gaf <- substr(gEAG$GAFIDENT,1,4)
  pvskp <- pvskp[!is.na(pvskp[[param]]),]
  
  # merge met EAG
  map <- sp::merge(gEAG, pvskp, by.x = 'gaf', by.y ='GAF', all.x = FALSE, duplicateGeoms = F)
  # merge met GAF
  map2 <- sp::merge(gEAG, pvskp, by.x = 'GAFIDENT', by.y ='EAG', all.x = FALSE, duplicateGeoms = F)
  
  leaflet(map) %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("GAF naam", map$GAFNAAM, "<br>", param, map[[param]]), 
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map[[param]]), fillOpacity = 0.6)    %>%
    addPolygons(data = map2, layerId = map2$GAFIDENT, popup= paste("EAG naam", map2$GAFNAAM, "<br>", param, map2[[param]]), 
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map2[[param]]), fillOpacity = 0.6)    %>%
    addLegend("bottomright", pal, values=~map[[param]], title = param) %>%
    addTiles()
}
# waterdiepte per eag
ppr_dieptekaart<- function (hybi, gebieden = gEAG, gbrpAGV, kansrijk = TRUE, handelperspectief = TRUE){ # kaart mediane diepte per eag + slibdikte is handelingsperspectief
  
  b <- dcast(hybi,locatiecode+locatie.EAG+jaar ~ parametercode, value.var = "meetwaarde", fun.aggregate = median, na.rm =TRUE, fill = NaN)
  b <- b[!is.na(b$WATDTE)&!is.na(b$SUBMSPTN),]
  b$kansloc <- 0
  b$SUBMSPTN <- b$SUBMSPTN - b$FLAB
  b$kansloc[(b$SUBMSPTN < 35|b$SUBMSPTN >75) & b$WATDTE < 0.35] <- 1 # alleen locaties waar de bedekking niet voldoet en waterdiepte te gering is
  b$handel <- 0
  b$handel[b$SLIBDTE + b$WATDTE > 0.35 & b$SLIBDTE > 0.05] <- 1
  c <- b[,c('locatie.EAG','locatiecode','jaar','handel','kansloc','WATDTE','SUBMSPTN','SLIBDTE')]
  
  if(handelperspectief){
    0 -> c$kansloc[c$handel == 0]
  }
  
  if(kansrijk){
    c <- c[,lapply(.SD, mean),.SDcols = c('handel','kansloc','WATDTE','SUBMSPTN','SLIBDTE'), by= c('locatie.EAG','jaar')]
    c <- c[order(c$kansloc),]
    c$fact <- cut(c$kansloc,  breaks = c('0','0.1','0.25','0.5','0.75','1'))
    c <- c[!is.na(c$fact),]
    col <- c('1'= 'lightyellow','2'="yellow", '3'="deepskyblue",'4'="blue",'5'="blue")
    labels <- c('1'="0-10%",'2'="10-25%" ,'3'="25-50%",'4'="50-75%",'5'="75-100%")
  }
  
  if(!kansrijk){
    c <- c[,lapply(.SD, median),.SDcols = c('kansloc','WATDTE','SUBMSPTN','SLIBDTE'), by= c('locatie.EAG','jaar')]
    c$fact <- cut(c$WATDTE,  breaks = c('0','0.1','0.2','0.3','0.35','0.4','0.5','1'))
    col <- c('1'="darkred",'2'="red", '3'="tomato",'4'="salmon",'5'="lightblue",'6'="deepskyblue", '7'= 'blue')
    labels <- c('1'="0-0.1",'2'="0.1-0.2" ,'3'="0.2-0.3",'4'="0.3-0.35",'5'="0.35-0.4",'6'="0.4-0.5",'7'="0.5-1")
  }
  
  
  
  # kaart waterdiepte
  map <- sp::merge(gebieden, c, by.x = 'GAFIDENT', by.y =
                     'locatie.EAG', all.x = FALSE, duplicateGeoms = T)
  pal <- colorFactor(palette = col,  domain = map$fact)
  
  map <- map[order(map$jaar),]
  
  leaflet() %>%
    addPolygons(data = gbrpAGV,
                stroke = T, color= 'green', opacity=0.05, weight = 0.2, smoothFactor = 0.8,
                fill=T, fillColor = 'green', fillOpacity = 0.3) %>%
    addPolygons(data = map, layerId = map$GAFIDENT, popup= paste("EAG naam", map$GAFNAAM.x, "<br>",
                                                                 "Diepte:", map$WATDTE, "<br>",
                                                                 "Slibdikte:", map$SLIBDTE, "<br>",
                                                                 "Percentage kansrijke locaties:", round(map$kansloc,2)*100, "<br>",
                                                                 "jaar:", map$jaar),
                stroke = FALSE, color= NULL, opacity=0.5, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map$fact), fillOpacity = 0.4) %>%
    
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addProviderTiles("Esri.WorldGrayCanvas")
  
}


# l <- EKRlijst[EKRlijst$facet_wrap_code %in% 'Ov. waterflora',]
# l <- l[EAGIDENT == '6430-EAG-1',]
# l <- l[GHPR == '',]

plotFractiePerMaatlat <- function(l){
  l <- l[!is.na(l$CODE),] # geen totaal scores per toetsgeied meenemen
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  l$klasse <- factor(l$klasse, levels = c("3", "4", "5", "6","7"), labels = c("0.8-1","0.6-0.8","0.4-0.6","0.2-0.4","0-0.2"))
  
  ggplot(l, aes(x = GHPR_level, fill = klasse)) +
    geom_bar(position = "fill") +
    scale_x_discrete(position = "left") +
    scale_fill_manual(values= c("0.8-1"="blue","0.6-0.8"="green","0.4-0.6"="yellow","0.2-0.4"="orange","0-0.2"="red"))+
    guides(fill=guide_legend(title='EKR score'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 6), #level
      axis.text.x = element_text(size= 6, angle=90), #jaar
      axis.text.y = element_text(size= 7),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 7),
      legend.text  = element_text(size = 6),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")+
    #ggtitle(titel, subtitle = "Fractie meetlocaties per EKR klasse ") +
    labs(x="",y="")
}
