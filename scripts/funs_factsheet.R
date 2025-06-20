# preprocess functions to clean up data.bases
# Laura Moria maart 2024

# extractfunctie for extracting relevant properties and data needed for single factsheet ----
factsheetExtract <- function(i,brondata,splot = TRUE){ 
  with(brondata, {
  # i <- 23
  # subset data ----
  # subset ESFoordelen and get ESF
  waterlichamenwl <- ESFoordelen[i,]
  ESF <- ESFoordelen[i,]
  
  # waterlichaam of eagcode
  wl <- waterlichamenwl$OWL_SGBP3
  wl <- unlist(strsplit(wl, split=c(', ',',','_'), fixed=TRUE))
  
  # select data for water body and extract name
  if(any(wl %in% eag_wl$KRW_SGBP3)){
    # als eenheid factsheet KRW wl
    eagwl <- eag_wl[KRW_SGBP3 %in% wl,]
    # extract the name as text after the first underscore
    wlname <- sub('.*_', '',unique(eagwl[,KRWmonitoringslocatie_SGBP3]))
    
  } else {
    
    # als eenheid factsheet is eag of gaf
    eagwl <- eag_wl[EAGIDENT %in% wl|substr(EAGIDENT,1,4) %in% wl,]
    # extract the name and remove prefix 'NL11_'
    wlname <- unique(eagwl$GAFNAAM)
    if(identical(wlname, character(0))){wlname <- "nietbeschikbaar"}
  }
  
  # get title
  my_title <- paste0(waterlichamenwl$OWMNAAM_SGBP3)
  my_title2 <- gsub('\\/','',gsub(' ','',my_title))
  my_title2 <- gsub(',','_',gsub(' ','',my_title2))
  
  # get P load vs critical p load
  pvskpsel <- pvskp[EAGIDENT %in% eagwl$EAGIDENT | GAFIDENT %in% substr(eagwl$EAGIDENT, 1, 4),]
  pvskpversie <- unique(pvskpsel$lastyear)
  pvskpsel[,naam := ifelse(is.na(EAGIDENT), GAFIDENT, EAGIDENT)]
  # select legenditems
  cols <- c('pol','naam',colnames(pvskpsel)[grepl('^a_in', colnames(pvskpsel)) & sapply(pvskpsel, is.character)])
  pvskp_legend <- pvskpsel[,mget(cols)]
  pvskp_legend <- unique(pvskp_legend)[,pol := NULL]
  
  # get maatregelen
  maatregelen1 <- maatregelen[HoortbijKRWWaterlichaam2021 %in% wl,]
  
  # update waterlichaam
  waterlichamenwl[,motstat := MotiveringBegrenzing]
  waterlichamenwl[,prov := Provincies]
  waterlichamenwl[,gem := Gemeenten]
  waterlichamenwl[,typebesch := paste0(tolower(watertypen[Code %in% unique(eagwl$watertype),Omschrijving]),collapse = ', ')]
  
  # get water quality for relevant EAG
  wq1 <- wq[EAGIDENT %in% eagwl$EAGIDENT,]
  wqversie <- paste0(min(year(wq1$datum)), '-', max(year(wq1$datum)))
  
  # get hydrobiological data
  hybi1 <- hybi[EAGIDENT %in% eagwl$EAGIDENT,]
  hybitotaalversie <- paste0(min(year(hybi$datum)), '-', max(year(hybi$datum)))
  hybiversie <- paste0(min(year(hybi1$datum)), '-', max(year(hybi1$datum)))
  
  
  # get soil ditch properties
  if('EAGIDENT' %in% colnames(bod)){
    bod1 <- bod[EAGIDENT %in% eagwl$EAGIDENT,]
    bod1 <- bod1[jaar == max(jaar),]
    bodversie <- unique(bod1$jaar)
  }
  
  # get values from EKR
  if (any(wl %in% EKRset$KRW_SGBP3)){
    EKRset1 <- EKRset[KRW_SGBP3 %in% wl,]
    ekr_scores1 <- ekr_scores[KRW_SGBP3 %in% wl,]
  } else {
    EKRset1 <- EKRset[EAGIDENT %in% eagwl$EAGIDENT,]
    ekr_scores1 <- ekr_scores[EAGIDENT %in% eagwl$EAGIDENT,]
  }
  toetsjaar <- max(year(EKRset1$Toetsdatumtijd))
  
  # get shape of EAG
  gEAG_sel <- EAG[EAG$EAGIDENT %in% eagwl$EAGIDENT, ]
  
  # get shape of water per eag
  waterpereag_sel <- waterpereag[waterpereag$GAFIDENT %in% eagwl$EAGIDENT, ]
  
  # get deelgebieden
  deelgebieden <- as.data.table(eagwl[,c('EAGIDENT','GAFNAAM')])
  deelgebieden[,samen := paste0(EAGIDENT," (",GAFNAAM,")")]
  
  # get hydrobiological data
  EST_sel <- EST[EAGIDENT %in% eagwl$EAGIDENT,]
  
  # get Ecosystem Status ----
  
  if(nrow(EST_sel)>0){
    
    ## gather O en W for the last year
    EST_sel <- EST_sel[,yearid := frank(-jaar, ties.method = 'dense')][yearid <= 1]
    # extract mean O and W for all EAGs
    colnames_w <- colnames(EST_sel)[grepl("^W",colnames(EST_sel))]
    colnames_w <- colnames_w[!(colnames_w %in% c('W','O'))]
    colnames_o <- colnames(EST_sel)[grepl("^O",colnames(EST_sel))]
    colnames_o <- colnames_o[!(colnames_o %in% c('W','O'))]
    
    # aggregate data to area
    estgeb <- EST_sel[, lapply(.SD, sum, na.rm=TRUE), by=c('jaar','watertype','StedelijkLandelijk'),.SDcols = c(colnames_w, colnames_o)]
    estgeb <- estgeb[1]
    estgeb$W <- colnames_w[max.col(estgeb[,..colnames_w],ties.method="first")]
    estgeb$O <- colnames_o[max.col(estgeb[,..colnames_o],ties.method="first")]
    estgeb$ESTnaam2[estgeb$watertype == 'M20'] <-  'DM'
    estgeb$ESTnaam2[estgeb$watertype %in% c('M14','M27',"M25","M11")] <-  'OM'
    estgeb$ESTnaam2[estgeb$watertype %in% c('M1a','M1b','M8',"M10","M3")] <- 'Sl'
    estgeb$ESTnaam2[estgeb$watertype %in% c("M6b",'M30',"M7b", "M6a")] <- 'K'
    estgeb$ESTnaam3[estgeb$StedelijkLandelijk == 'Stedelijk'] <- 'St'
    estgeb$ESTnaam3[estgeb$StedelijkLandelijk == 'Landelijk'] <- 'L'
    estgeb$estnaam <- paste0(estgeb$W,'_',estgeb$O,'_',estgeb$ESTnaam2,'_', estgeb$ESTnaam3)
    
        # final ESTnaam
    ESTnaam <- estgeb$estnaam
    setDT(EST_kt)
    ESTnaam <- paste0("esticon/", EST_kt$tekening[EST_kt$estnaam %in% ESTnaam[1]], ".jpg") 
    
    # if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_3_8')){ESTnaam <- "esticon/W6_O7_DM_L.jpg"}
    # if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_5_1')){ESTnaam <- "esticon/W4_O6_OM_L.jpg"}
    # if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_1_2')){ESTnaam <- "esticon/W6_O6_K_St.jpg"}
    # if(unique(dtEST2$KRW_SGBP3) %in% c('NL11_1_1')){ESTnaam <- "esticon/W6_O6_K_St.jpg"}
    
  }else(ESTnaam <- "NietBeschikbaar")
  
  # --- make kaarten plots ----
  ## plot locatie EAG binnen beheergebied AGV
  
  # bounding box needed
  bboxEAG <- st_bbox(EAG)
  
  # create map
  mapEAG <- ggplot2::ggplot() +
    geom_sf(data = EAG, color = 'grey25', fill = "white", size = 0.2) +
    geom_sf(data = waterpereag_sel, color = NA, fill = '#345bdb', linewidth = 50) +#3498DB
    geom_sf(data = gEAG_sel, color = '#d73027', fill = NA, linewidth = 2) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#e0f3f8"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position="none") +
    coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax), ylim = c(bboxEAG$ymin,bboxEAG$ymax), datum = NA)
  
  # make filename for directory
  pdir <- gsub(',| ','_',my_title2)
  
  # create dir to save plots
  if(!pdir %in% list.files('factsheets/routput')){
    dir.create(paste0('factsheets/routput/',pdir))}
  
  # save plot, and location where map is saved
  if(splot){ggsave(mapEAG, file = paste0('factsheets/routput/',pdir,'/mapEAG.png'),
                   width = 20,height = 18,units='cm',dpi=1000)    }
  mapEAG <- paste0('routput/',pdir,'/mapEAG.png')
  
  ## plot locatie deelgebieden binnen EAG
  
  # bounding box needed
  bboxEAG <- st_bbox(gEAG_sel)
  
  # create map
  mapDEELGEBIED <-ggplot2::ggplot() +
    geom_sf(data = EAG, color = 'grey25', fill = "white", size = 0.2) +
    geom_sf(data = waterpereag_sel,color = NA, fill = '#3498DB') +
    geom_sf(data = gEAG_sel,color = '#d73027', fill = NA, linewidth = 1.5) +
    geom_sf_label(data = gEAG_sel, aes(label = EAGIDENT))+
    #geom_text(data=randomMap.df, aes(label = id, x = Longitude, y = Latitude)) +
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = "#e0f3f8"),  ## azure
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          #panel.border = element_rect(colour = "lightgrey", fill=NA, size=1),
          legend.position="none") +
    coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax) + c(-250,250),
             ylim = c(bboxEAG$ymin,bboxEAG$ymax) + c(-250,250), datum = NA)
  
  # save plot, and location where map is saved
  if(splot){ggplot2::ggsave(plot = mapDEELGEBIED,file=paste0('factsheets/routput/',pdir,'/mapDEELGEBIED.png'), 
                            width = 20,height = 18,units='cm',dpi=1000)}
  mapDEELGEBIED <- paste0('routput/',pdir,'/mapDEELGEBIED.png')
  
  # --- make EKR plot ------------
  
  # subset 1, en zoek laagste score hoofdmaatlat
  ekr_scores2 <- ekr_scores1[!is.na(waterlichaam) & level == 1 & aggregated == 'ja']
  d3 <- ekr_scores2[oordeelsort==min(oordeelsort,na.rm=T),]
  # subset 2, en zoek laagste score deelmaatlat
  ekr_scores2 <- ekr_scores1[facet_wrap_code %in% d3$facet_wrap_code & level == 2 & aggregated == 'ja',]
  d3_deel <- ekr_scores2[EKR3jr==min(EKR3jr,na.rm=T),]
  ## select score per deelmaatlat non aggregated
  ekr_scores2 <- ekr_scores1[aggregated == 'nee',]
  # subset 1a, en zoek laagste score 
  ekr_scores2 <- ekr_scores2[facet_wrap_code == "Ov. waterflora" &
                              level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')),]
  d3_deelptn <- ekr_scores2[EKR3jr==min(EKR3jr,na.rm=T),]
  
  if(nrow(ekr_scores1)> 0){
    # plot figure EKR
    mapEKR <- ppr_ekrplot(ekr_scores1)
    # create plt met ekr scores in de tijd
    plotEKRlijn <- plotEKRlijnfs(EKRset1)
  } else {
    # plot figure EKR when no data is available
    db <- data.frame()
    mapEKR = plotEmpty(db = db, type='plotekrplot')
    plotEKRlijn <- plotEmpty(db = db, type='plotekrplot')
  }
  
  # save plot, and location where map is saved
  if(splot){ggplot2::ggsave(mapEKR,file=paste0('factsheets/routput/',my_title2,'/mapEKR.png'),
                            width = 20, height = 15, units='cm', dpi=1000)}
  mapEKR <- paste0('routput/',my_title2,'/mapEKR.png')
  
 
  # --- Ecologische SleutelFactoren (ESF tabel) ------
  
  # ESF is doubled in columns: one string and one number, detect via lenght of the first row
  cols <- colnames(ESF)[grepl('ESF', colnames(ESF))]
  cols_nr <- cols[grepl('_nr', cols)]
  cols <- cols[!cols %in% cols_nr]
  
  # make data.table
  ESFtab <- melt.data.table(ESF, id.vars = "OWMNAAM_SGBP3", measure.vars = cols, variable.name = "ESF", value.name = 'oms')
  ESFtab2 <- melt.data.table(ESF, id.vars = "OWMNAAM_SGBP3", measure.vars = cols_nr, variable.name = "ESF_nr", value.name = 'kleur')
  ESFtab <- cbind(ESFtab, ESFtab2)
  ESFtab[,esf:= 1:8]
  
  # add oordeel
  ESFtab[kleur==1,OORDEEL := 'groennummer.jpg']
  ESFtab[kleur==2,OORDEEL := 'oranjenummer.jpg']
  ESFtab[kleur==3,OORDEEL := 'roodnummer.jpg']
  ESFtab[!kleur %in% 1:3,OORDEEL := 'grijsnummer.jpg']
  
  # add link to figuur for html as well as latex
  ESFtab[,OORDEEL := paste0(esf,OORDEEL)]
  ESFtab[,OORDEEL := sprintf("![](%s){width=50px}", paste0("esf/",OORDEEL, " "))]
  
  # select relevant columns
  ESFtab <- ESFtab[,.(esf,OORDEEL,kleur,oms)]
  
  # adapt text to latex format
  ESFtab[,oms_latex := gsub('%','\\%',oms,fixed=TRUE)]
  ESFtab[,oms_latex := gsub('  ',' ',gsub('\r\n','',oms_latex))]
  
  # --- uitgevoerde maatregelen ----------
  
  # uitgevoerd in SGBP 1 en 2, in planvorming of in fasering dan wel ingetrokken of vervangen
  maatregelen1[,Uitgevoerd1 := pmax(0,as.numeric(Uitgevoerd),na.rm=T)+pmax(0,as.numeric(Uitvoering),na.rm=T)]
  maatregelen1[,Plan := as.numeric(Plan)]
  maatregelen1[,Gefaseerd := as.numeric(Gefaseerd)]
  
  # percentage per type
  periode <- c("SGBP1 2009-2015", "SGBP2 2015-2021", "SGBP3 2021-2027") # hier mist een periode (niet in sgbp)
  rates <- list(
    rate_uit = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode & Uitgevoerd1 > 0,]),
    rate_max = nrow(maatregelen1[SGBPPeriode.omschrijving %in% periode]),
    rate_plan = nrow(maatregelen1[Plan > 0 & SGBPPeriode.omschrijving %in% periode,]),
    rate_fase = nrow(maatregelen1[Gefaseerd > 0 & SGBPPeriode.omschrijving %in% periode,]),
    rate_del = nrow(maatregelen1[Vervangen > 0 | Ingetrokken > 0  & SGBPPeriode.omschrijving %in% periode,]),
    rate_nieuw = nrow(maatregelen1[SGBPPeriode.omschrijving %in% c("SGBP3 2021-2027"),]))
  
  # add nieuw information to maatregelen
  maatregelen1[,Gebiedspartner := `gebiedspartner (gesprekspartner bij financiering, uitvoering of beleid)`]
  maatregelen1[,SGBPPeriode := SGBPPeriode.omschrijving]
  maatregelen1[,Initiatiefnemer := Initiatiefnemer.naam]
  maatregelen1[,BeoogdInitiatiefnemer := Initiatiefnemer.waternet]
  maatregelen1[,esffrst := substr(esf,1,4)]
  maatregelen1[nchar(esffrst)==0, esffrst := NA]
  # maatregelen1[!is.na(`Totale belasting P`) & eenheid == 'mg/m2/dag', OmvangWaternet := OmvangWaternet/`Totale belasting P`]
  # maatregelen1[!is.na(`Totale belasting P`) & eenheid == 'mg/m2/dag', eenheid := '% reductie']
  
  # join measures with ESF-tabel
  cols <- c('Naam','Toelichting','SGBPPeriode','esffrst','Initiatiefnemer','BeoogdInitiatiefnemer',
            'Gebiedspartner','OmvangWaternet','eenheid','UitvoeringIn',"afweging")
  ESFtab[,esf:= paste0('ESF',esf)]
  maatregelen2 <- merge.data.table(ESFtab, maatregelen1[,mget(cols)],by.x = 'esf', by.y = 'esffrst', all.y = T)
  
  # als meerdere esf aan een maatregel gekoppeld zijn dan wordt de eerste geselecteerd
  cols <- c('ESFoordeel','SGBPPeriode','Naam','Toelichting','Initiatiefnemer','BeoogdInitiatiefnemer','OmvangWaternet','eenheid','Gebiedspartner','UitvoeringIn','afweging')
  maatregelen2[,ESFoordeel := OORDEEL]
  maatregelen2[is.na(ESFoordeel), ESFoordeel := '![esficon](esf/9grijsnummer.jpg ){width=50px}']
  maatregelen2 <- maatregelen2[,mget(cols)]
  
  # sorteer met oplopend jaar en ESF
  setorder(maatregelen2,ESFoordeel,-SGBPPeriode)
  
  # formatting (avoid characters that conflict with latex)
  maatregelen2[, Toelichting := gsub('%','\\%',Toelichting,fixed=TRUE)]
  maatregelen2[, Gebiedspartner := gsub('\\?','onbekend',Gebiedspartner)]
  maatregelen2[, Naam := gsub('%','\\%',Naam,fixed=TRUE)]
  
  # --- plot ESF1: productiviteit ----
  plotPwbal.ref <- paste0('routput/',my_title2,'/plotPwbal.png')
  if(length(pvskpsel$pol)>=1){
    
    # plot figure ESF1
    plotPwbal = ppr_pvskpplot(pvskpsel)
    class(plotPwbal.ref) <- 'plotref'
    
  } else {
    
    # plot figure ESF1 when no data is available
    plotLeegDB = data.frame(EAG = eagwl$EAGIDENT)
    plotLeegDB$pload <- 0
    plotPwbal = plotEmpty(db = plotLeegDB, type='Pwbal')
    class(plotPwbal.ref) <- 'plotempty'
  }
  
  # save plot
  if(splot){ggplot2::ggsave(plotPwbal,file=paste0('factsheets/routput/',my_title2,'/plotPwbal.png'),
                            width = 20,height = 15,units='cm',dpi=1000)}
  
  
  # --- plot ESF2: lichtklimaat ----
  plotLichtklimaat.ref <- paste0('routput/',my_title2,'/plotLichtklimaat.png')
  if(nrow(wq1[parameterid == "EXTTCEFCELBT__m_L400-700nm" & jaar > '2015',]) > 0) {
    
    # plot ESF 2
    plotLichtklimaat = ppr_extinctie(wq = wq1, hybi = hybi1, filter = c('EXTTCEFCELBT__m_L400-700nm','EXTTCEFCELBT_m_L400-700nm','WATDTE_m'))
    class(plotLichtklimaat.ref) <- 'plotref'
  } else {
    
    # plot figure ESF2 when no data is available
    plotLeegDB = data.frame(EAG = eagwl$EAGIDENT, Lext = 0)
    plotLichtklimaat = plotEmpty(db = plotLeegDB,type='plotLichtklimaat')
    class(plotLichtklimaat.ref) <- 'plotempty'
  }
  
  # save plot is saved
  if(splot){ggplot2::ggsave(plotLichtklimaat,file=paste0('factsheets/routput/',my_title2,'/plotLichtklimaat.png'),
                            width = 20,height = 15,units='cm',dpi=1000)}
  
  
  # --- plot ESF4: waterdiepte ----
  plotWaterdiepte.ref <- paste0('routput/',my_title2,'/plotWaterdiepte.png')
  
  if(nrow(hybi1[parameterid == 'WATDTE_m',])>0){
    
    # plot ESF 4
    plotWaterdiepte = ppr_waterdieptesloot(hybi1[!is.na(parameterid == 'WATDTE_m'),], filter = c('WATDTE_m','ZICHT_m'))
    class(plotWaterdiepte.ref) <- 'plotref'
  } else {
    
    # plot figure ESF4 when no data is available
    plotLeegDB = data.frame(EAG = eagwl$EAGIDENT, wd = 0,krwwt ='onbekend')
    plotWaterdiepte = plotEmpty(db = plotLeegDB,type='plotWaterdiepte')
    class(plotWaterdiepte.ref) <- 'plotempty'
    
  }
  
  # save plot
  if(splot){ggplot2::ggsave(plotWaterdiepte,file=paste0('factsheets/routput/',my_title2,'/plotWaterdiepte.png'),
                            width = 20,height = 15,units='cm',dpi=1000)}
  
  
  # --- plot ESF3 : waterbodem ----
  # plotbodFW.ref <- paste0('routput/',my_title2,'/plotWaterbodem_FW.png')
  # plotqPW.ref <- paste0('routput/',my_title2,'/plotWaterbodem_qPW.png')
  plotWaterbodem.ref <- paste0('routput/',my_title2,'/plotWaterbodem.png')
  
  if(nrow(bod1) > 0) {
    
    # plot ESF 4
    # plotbodFW = ppr_plotbod(bod1,type = 'plotFW')
    # plotqPW = ppr_plotbod(bod1,type = 'plotqPW')
    plotWaterbodem = ppr_plotbod(bod1,type='grid')
    # class(plotbodFW.ref) <- 'plotref'
    # class(plotqPW.ref) <- 'plotref'
    class(plotWaterbodem.ref) <- 'plotref'
    
  }else{
    
    # plot figure ESF3 when no data is available
    plotLeegDB = data.frame(EAG = eagwl$EAGIDENT, plv = 0, ijzerval ='onbekend')
    plotWaterbodem = plotEmpty(db = plotLeegDB,type='plotqPW')
    # class(plotbodFW.ref) <- 'plotempty'
    # class(plotqPW.ref) <- 'plotempty'
    class(plotWaterbodem.ref) <- 'plotempty'
    
  }
  
  # save plots
  if(splot){
    # ggplot2::ggsave(plotbodFW,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem_FW.png'),width = 20,height = 15,
                    # units='cm',dpi=1000)
    # ggplot2::ggsave(plotqPW,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem_qPW.png'),width = 20,height = 15,
                    # units='cm',dpi=1000)
    ggplot2::ggsave(plotWaterbodem,file=paste0('factsheets/routput/',my_title2,'/plotWaterbodem.png'),
                    width = 20, height = 15,units='cm',dpi=1000)
  }
  
  
  
  # make a list to store the output
  out <- list(waterlichamenwl = waterlichamenwl,
              wlname = wlname,
              my_title2 = my_title2,
              my_title = my_title,
              eagwl = eagwl,
              deelgebieden = deelgebieden,
              ESTnaam = ESTnaam,
              mapEAG = mapEAG,
              mapDEELGEBIED = mapDEELGEBIED,
              mapEKR = mapEKR,
              plotPwbal = plotPwbal.ref,
              plotLichtklimaat = plotLichtklimaat.ref,
              plotWaterdiepte = plotWaterdiepte.ref,
              plotWaterbodem = plotWaterbodem.ref,
              ESFtab = ESFtab,
              maatregelen1 = maatregelen1,
              maatregelen2 = maatregelen2,
              rates = rates,
              d3 = d3,
              d3_deel = d3_deel,
              d3_deelptn = d3_deelptn,
              plotEKRlijn = plotEKRlijn,
              hybiversie = hybiversie,
              wqversie =wqversie,
              bodversie = bodversie,
              pvskpversie =pvskpversie,
              pvskp_legend =pvskp_legend,
              toetsjaar = toetsjaar
  )
  
  # return list with relevant properties
  return(out)})
}

# functions to make plots (in factsheet extract)----------
# plot EKR versie 2
ppr_ekrplot <- function(ekr_scores1){

  # make local copy
  dt <- copy(ekr_scores1[aggregated == 'ja' & level == 1,])

  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[KRW_SGBP3 == "",KRW_SGBP3 := NA]
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := fifelse(is.na(KRW_SGBP3), paste0(EAGIDENT," ",facet_wrap_code), facet_wrap_code)]

  # filter unique goal (per area)
  bg <- unique(dt[,c('waterlichaam','wlmt','facet_wrap_code','GEP_2022')])
  
  # calc axis limits
  max_y_axis <- ifelse(max(dt$EKR3jr, na.rm = TRUE) == 1, 1.05, 
                       ifelse(max(dt$EKR3jr, na.rm = TRUE) > max(bg$GEP_2022), 1, (1-max(bg$GEP_2022))/2+max(bg$GEP_2022)))
  
  # define background goals (GEP_2022 is goal which is set in 2022)
  bg[,c('goed_ymin','goed_ymax') := .(GEP_2022,max_y_axis)]
  bg[,c('matig_ymin','matig_ymax') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin','ontoereikend_ymax') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin','slecht_ymax') := .(0,GEP_2022 / 3)]
  bg <- melt(bg,id.vars = c('waterlichaam','wlmt','facet_wrap_code','GEP_2022'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg[,varrange := fifelse(grepl('_ymin',doelen),'ymin','ymax')]
  bg[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg <- dcast.data.table(bg,waterlichaam+wlmt+GEP_2022+facet_wrap_code+doelen~varrange,value.var='waarde')
  bg[,doelen:= as.factor(doelen)]
  
  legend_colors <- c("lawngreen", "yellow", "orange","red")
  names(legend_colors) <- levels(bg$doelen)
  
  ## make plot
  plot <- 
    ggplot(dt, aes(x = wmlt, y = EKR3jr)) +
    geom_rect(data = bg, inherit.aes = FALSE, aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = doelen), alpha = 0.3) +
    scale_fill_manual('Doel waterkwaliteit:', values = legend_colors, labels = c("goed", "matig","ontoereikend","slecht"), guide = guide_legend(override.aes = list(alpha = 0.3)))+
    geom_segment(aes(x = 0, xend = 1,y = EKR3jr, yend = EKR3jr, linetype = "Actuele toestand"),col = "black", cex = 1.4) + # linetype = 2 -> dashed
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y_axis), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    scale_linetype_manual("", values= c("Actuele toestand" = 1))+
    facet_grid(cols = vars(wlmt)) +
    xlab('') + ylab('EKR')+
    theme_minimal()+
    theme(axis.ticks.x=element_blank(),
          axis.line = element_line(),
          strip.text.x = element_text(size = 9), 
          strip.text.y = element_text(size = 9), 
          axis.text.x = element_text(size= 10, angle = 45, vjust = 0.6), 
          axis.text.y = element_text(size= 10), 
          axis.title = element_text(size= 10),
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 10),
          legend.text  = element_text(size = 10),
          legend.position = "right")
  return(plot)
}
# plot EKR background
plotEKRlijnfs <- function(EKRset1, gebied = NULL){
  
  # ongewogen scores obv koppeling locatie en EAG
  z <- rbind(EKRset1[aggregated == 'ja',], EKRset1[aggregated == 'nee'& level == 3,])
  
  if(!is.null(gebied)){
    z <- z[!is.na(z$EAGIDENT),]
    z$KRW_SGBP3 <- ""
  }
  
  z[order(GHPR_level)]
  z[,jaar := as.numeric(jaar)]
  z[,Numeriekewaarde := as.numeric(Numeriekewaarde)]
  z[,facet_wrap_code := as.character(facet_wrap_code)]
  z[,wlmt := ifelse(is.na(KRW_SGBP3)|KRW_SGBP3 == "", paste0(EAGIDENT," ",facet_wrap_code), facet_wrap_code)]
  z <- dcast(z, waterlichaam+wlmt+GHPR+GHPR_level+level+GEP_2022+jaar~., value.var = 'Numeriekewaarde', fun.aggregate=mean)
  z[,Numeriekewaarde := `.`]
  z[,maatlatniv := factor(level, levels = c("1","2","3"), labels = c("hoofdmaatlat","deelmaatlat","indicator"))]
  
  max_y_axis <- ifelse(max(z$Numeriekewaarde) == 1, 1.05, 
                       ifelse(max(z$Numeriekewaarde) > max(z$GEP_2022), 1, max(z$GEP_2022)+0.1))
  
  # define background goals (GEP_2022 is goal which is set in 2022)
  bg <- unique(z[,c('wlmt','GEP_2022')])
  # define background goals (GEP_2022 is goal which is set in 2022)
  bg[,c('goed_ymin','goed_ymax') := .(GEP_2022,max_y_axis)]
  bg[,c('matig_ymin','matig_ymax') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin','ontoereikend_ymax') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin','slecht_ymax') := .(0,GEP_2022 / 3)]
  bg <- melt(bg,id.vars = c('wlmt','GEP_2022'),
             variable.name = 'doelen',value.name = 'waarde')
  bg[,varrange := fifelse(grepl('_ymin',doelen),'ymin','ymax')]
  bg[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg <- dcast.data.table(bg,wlmt+GEP_2022+doelen~varrange,value.var='waarde')
  bg[,doelen:= as.factor(doelen)]
  bg[,xmin := min(z$jaar)-2]
  bg[,xmax := max(z$jaar)+2]
  legend_colors <- c("lawngreen", "yellow", "orange","red")
  names(legend_colors) <- levels(bg$doelen)
  
  # prep colours and linetype
  cols <- c("black","#969696","#252525","grey40","grey24","#636363","#CCCCCC")
  cols <- as.data.table(cols)[,id := rep(1:7)]
  cols[,linetype:= c("solid","dashed","dotted","dotdash","longdash","twodash","dashed")]
  cols[, shape:= c('15','16','17','18','19','20','21')]
  setorder(z, level)
  z[level == 1, id := .GRP, by = c('GHPR')]
  z[level == 2, id := .GRP, by = c('GHPR')]
  z[level == 3, id := .GRP, by = c('GHPR')]
  
  z <- merge(z, cols, by = 'id', all.x = TRUE)
  cols <- unique(z[,c('GHPR','cols','linetype','shape')])
  # cols[, shape := as.list(shape)] # nodig voor ggplot, maar werkt niet in plotly
  
  # prep order legend
  setorder(z, GHPR_level, jaar)
  ghpr_order <- unique(z$cols)
  # shape4override <- as.list(z[jaar == max(jaar) & waterlichaam == unique(waterlichaam)[1], shape])
  # line4override <- z[jaar == max(jaar) & waterlichaam == unique(waterlichaam)[1], linetype]

  p <- ggplot(data= z, aes(x=jaar, y=Numeriekewaarde, col = GHPR, group = GHPR, linetype = linetype, shape = shape))+
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    scale_colour_manual(values = cols$cols, breaks = as.vector(cols$GHPR))+
    scale_shape_manual(values = cols$shape)+
    scale_linetype_manual(values = c('solid'='solid','dashed'='dashed', "dotted"="dotted","longdash"="longdash",'dotdash'='dotdash',"twodash"="twodash",'dashed'='dashed', "dotted"="dotted"), breaks = ghpr_order)+
    scale_x_continuous(expand = c(0, 0), limits= c(min(z$jaar)-2, max(z$jaar)+2), n.breaks = 8, labels = scales::number_format(accuracy = 1, big.mark = ''))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, max_y_axis), breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    
    geom_rect(data= bg, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = doelen), 
              inherit.aes = FALSE, alpha = 0.15, ) +
    scale_fill_manual(values = legend_colors, labels = c("goed", "matig","ontoereikend","slecht"))+
    
    facet_grid(vars(maatlatniv),vars(wlmt))+
    ylab('')+xlab('')+
    guides(shape = "none", linetype = "none", fill = 'none',col=guide_legend(title="(Deel)maatlat: ", ncol = 1))+
    theme_minimal()+
    theme(axis.ticks.x=element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text.x = element_text(size = 9), 
          strip.text.y = element_text(size = 0.01), 
          axis.text.x = element_text(size= 10, angle = 45, vjust = 0.6), 
          axis.text.y = element_text(size= 10), 
          axis.ticks =  element_line(colour = "black"),
          axis.title = element_text(size= 10),
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA),
          plot.background = element_blank(),
          # panel.ontop = F,
          legend.title = element_text(size = 10),
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")

return(p)
}

# maak plot van p VS Kp
ppr_pvskpplot <- function(pvskpsel, lakeditch = 'ditch'){

  # make local copy
  d1 <- copy(pvskpsel)
 
  # colomns needed
  cols <- colnames(d1)[sapply(d1, is.numeric)]
  cols_char <- colnames(d1)[sapply(d1, is.character)]
  
  # estimate mean by name and select only those with a name
  d1 <- d1[,lapply(.SD,mean,na.rm=T),.SDcols = cols, by=c('pol','EAGIDENT',
                                                          "a_inlaat1","a_inlaat2","a_inlaat3","a_inlaat4","a_inlaat5",
                                                          "a_uitlaat1","a_uitlaat2","a_uitlaat3","a_uitlaat4",
                                                          "GAFIDENT","lake_ditch_vol")][!is.na(pol)]

  # set measurement to zero when not available
  d1[is.na(wp_meting_mgm2d), wp_meting_mgm2d := 0]

  # set colors
  colWat1 <-  c("green4","darkorange", "darkred", "red", "red1",  "red2","red3", "brown",  "blue", "black","darkgreen", "grey")
  colWat2 <- adjustcolor(colWat1, alpha.f = 0.2)
  colWat <- paste(c(colWat2,"yellow", colWat1))

  # select columns
  cols <- colnames(d1)[grepl('^pol|EAGIDENT|GAFIDENT|^wp_|^kP|^pc_helder|^lake_ditch_vol',colnames(d1))]
  cols <- cols[!grepl('_sum$|_gm3$',cols)]
  d1 <- d1[,mget(cols)][,wp_meting_mgm2d := -wp_meting_mgm2d]

  # reshape data.table for figure
  d2 <- melt.data.table(d1,id.vars = c('pol','EAGIDENT','GAFIDENT','kP','pc_helder_troebel','lake_ditch_vol'),
        variable.name = 'source',value.name = 'value', variable.factor = FALSE)
  d2$kP_comb <- ifelse(!is.na(d2$pc_helder_troebel), d2$pc_helder_troebel, d2$kP)
  d2[,naam := ifelse(is.na(EAGIDENT), GAFIDENT, EAGIDENT)]
  
  
  # plot figure
  plot <- ggplot() +
    geom_bar(data = d2, inherit.aes = FALSE, aes(x = naam, y = value, fill = source), stat = 'identity') +
    scale_fill_manual("Bronnen (minimaal = min, incrementeel = inc)", values = colWat, 
                      guide = guide_legend(override.aes = list(size = 7)))+
    geom_point(inherit.aes = FALSE, data= d2, aes(x = naam, y= kP_comb, col = lake_ditch_vol), shape = 95, size = 20) + 
    guides(col=guide_legend(title="Kritische belasting op basis van"))+
    scale_color_manual(name = "", values= c('black','darkgrey','grey'))+
    xlab('') + ylab('mg P/m2/dag')+
    ggtitle("Fosfor- en kritische fosforbelasting per deelgebied")+
    theme_minimal() +
    theme(axis.ticks.x=element_blank(),
          axis.line = element_line(),
          strip.text.x = element_text(size = 9), 
          strip.text.y = element_text(size = 9), 
          axis.text.x = element_text(size= 10, angle = 45, hjust = 1), 
          axis.text.y = element_text(size= 10), 
          axis.title = element_text(size= 10),
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_line(),
          panel.ontop = F,
          legend.title = element_text(size = 10),
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")
    

  return(plot)
  }
# make empty plots for factsheets when data is missing
plotEmpty <-function(db,type){

  # middle x-axis
  midax = if(nrow(db)==1) 1 else nrow(db)*0.5 + 0.5

  # plot Pwbal
  if(type=='Pwbal'){
    plot <-  ggplot(db) +
      geom_bar(aes(x = EAG, y = pload), stat = 'identity') +
      xlab('') + ylab('mg P/m2/dag')+
      theme_classic() + ylim(0,2) +
      theme(legend.title = element_blank(),
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1)) +
      annotate("text", x = midax , y=1,
               label = "P-belasting en bronnen\nzijn (nog) niet beschikbaar.",
               hjust = 'middle',size=5,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }

  if(type=='plotLichtklimaat'){

    plot <-  ggplot(db) +
      geom_bar(aes(x = EAG, y = Lext), stat = 'identity') +
      xlab('') + ylab('Vertical extinctie')+
      theme_classic() + ylim(0,4) +
      theme(legend.title = element_blank(),
            legend.text  = element_text(size = 6),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      theme(axis.text.x = element_text(angle = 30, hjust =1),
            axis.ticks =  element_line(colour = "black"),
            axis.line = element_line(colour='black')) +
      annotate("text", x = midax , y=2,
               label = "Gegevens over het lichtklimaat\nzijn voor deze EAGs\n(nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      theme(axis.text =element_text(colour="black"))
  }

  if(type == 'plotWaterdiepte'){

    plot <- ggplot(db, aes(x= EAG, y= wd, col = krwwt))+
      geom_boxplot() +
      theme_minimal()+ scale_y_reverse(limits=c(3.5,0)) +
      guides(col=guide_legend(title="KRW watertype"))+
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(size= 7, angle=0,colour = 'black'),
        axis.text.y = element_text(size= 7, hjust=2,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank() )+
      annotate("text", x = midax , y=1.6,
               label = "Metingen waterdiepte \nzijn (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      ggtitle('') +
      labs(x= '', y = 'waterdiepte (m)\n')
  }

  if(type=='plotbodFW'){

    plot <- ggplot(db, aes(x= EAG, y= plv, fill = ijzerval))+
      geom_boxplot() +
      theme_minimal()+ ylim(-0.5,0.5)+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 30, hjust = 1, colour = 'black'),
        axis.text.y = element_text(size= 7,colour = 'black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = midax , y=0,
               label = "Potentiele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'),
                        labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Potentiele nalevering") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }

  if(type=='plotqPW'){

    plot <- ggplot(db, aes(x= EAG, y= plv, fill = ijzerval))+
      geom_boxplot() + ylim(-0.5,1.5)+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), #EAG
        strip.text.y = element_text(size = 5), #EKR
        axis.text.x = element_text(size= 7, angle = 30, hjust = 1, colour='black'),
        axis.text.y = element_text(size= 7, colour='black'),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = midax , y=0.8,
               label = "Actuele nalevering \nis (nog) niet bekend.",
               hjust = 'middle',size=5,color='blue') +
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem\nobv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  if(type=='plotekrplot'){
    plot <- ggplot(db) + geom_point() + xlim(0, 10) + ylim(0, 100) +
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks =  element_blank(),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank())+
      annotate("text", x = 5 , y = 50,
               label = "Ecologische toestand en doelen \nzijn (nog) niet bekend.",
               hjust = 'middle', size=5, color='blue') +
      labs(x="",y=" ", fill = '')
  
   
  }


  # return plot
  return(plot)

}
# functie voor lichtklimaat en waterdiepte
ppr_extinctie <- function(wq1, hybi1, filter = c('EXTTCEFCELBT__m_L400-700nm', 'WATDTE_m')){

  # median depth of hydrobiological data
  medianewd <- median(hybi1[parameterid %in% filter, meetwaarde],na.rm = T)
  # meax depth of hydrobiological data
  maxwd <- max(hybi1[parameterid %in% filter, meetwaarde],na.rm = T)

  # mean extinctie
  wq1 <- wq1[parameterid %in% filter & jaar > 2015 & meetwaarde > 0,]
  wq1 <- wq1[!is.na(watertype) & !is.na(EAGIDENT) & EAGIDENT != '',]
  meanext <- mean(wq1[parameterid %in% 'EXTTCEFCELBT__m_L400-700nm', meetwaarde])
  wq1[jaar<2019&jaar>2015,jaar_int:='2016 t/m 2018']
  wq1[jaar<2022&jaar>2018,jaar_int:='2019 t/m 2021']
  wq1[jaar<2025&jaar>2021,jaar_int:='2022 t/m 2024']
  
  # plot figure
  p <- ggplot()+
    geom_boxplot(data = wq1[parameterid %in% 'EXTTCEFCELBT__m_L400-700nm',], aes(x= jaar_int, y= meetwaarde), outliers = FALSE) +
    geom_hline(aes(yintercept = (log(25)/0.5), col = '0.5 meter'), show.legend = T)+
    geom_rect(inherit.aes = FALSE, aes(xmin = 0, xmax = Inf, ymin = Inf, ymax = log(25)/0.5),fill=  'grey2', alpha = 0.3) +
    geom_hline(aes(yintercept = (log(25)), col = '1 meter'), show.legend = T)+ #vec voor 1 meter >4%
    geom_hline(aes(yintercept = (log(25))/maxwd, col = paste0(as.character(maxwd), ' meter (max diepte bemonsterd)')), show.legend = T)+ #vec voor 4 meter >4%
    geom_hline(aes(yintercept = (log(25))/medianewd, col = paste0(as.character(medianewd), ' meter (mediane diepte)/n Op ')), show.legend = T)+ #vec voor 4 meter >4%
    geom_rect(inherit.aes = FALSE, aes(xmin = 0, xmax = Inf, ymin = log(25)/0.5, ymax = log(25)/medianewd),fill=  'darkgrey', alpha = 0.3) +
    geom_hline(aes(yintercept = (log(25))/7, col = '7 meter'), show.legend = T)+ #vec+ voor 7 meter 4%
    scale_y_reverse()+
    facet_grid(.~EAGIDENT)+
    guides(col=guide_legend(title="4 % licht voor waterplanten op"))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_text(size= 10, angle = 45, hjust =1), 
      axis.text.y = element_text(size= 10), 
      axis.title = element_text(size= 10),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 10, face = 'bold'),
      legend.text  = element_text(size = 10),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")+
    ggtitle('') +
    labs(x= 'Ecologisch analysegebied', y = 'Verticale extinctie (/m)')
  # return plot
  return(p)

}

ppr_waterdieptesloot <- function(hybi1, filter = c('WATDTE_m','ZICHT_m')){

  # diepte4licht <- log(25)/1.2
  hybi2 <- hybi1[parameterid %in% filter ,]
  # remove values that cannot exists (negative depths)
  hybi2[meetwaarde < 0, meetwaarde := NA]
  # ##---------------
  # p <-
  #   ggplot()+
  #   geom_boxplot(data = hybi2,aes(x= EAGIDENT, y= meetwaarde, col = parameterid_naam)) +
  #   # geom_boxplot(data = dz,aes(x= EAGIDENT, y= meetwaarde, col = watertype)) +
  #   theme_minimal()+ 
  #   scale_y_reverse(limits=c(max(hybi2$meetwaarde)+0.1,0)) +
  #   guides(col=guide_legend(title="Parameter"))+
  #   theme(
  #     strip.background = element_blank(),
  #     axis.text.x = element_text(angle = 30, hjust =1),
  #     axis.text.y = element_text(size= 10),
  #     panel.background = element_blank(),
  #     plot.background = element_blank(),
  #     axis.title = element_text(size=10),
  #     axis.ticks =  element_line(colour = "black"),
  #     axis.line = element_line(colour='black'),
  #     legend.title = element_text(size = 10, face = 'bold'),
  #     legend.text  = element_text(size = 10),
  #     legend.key.size = unit(0.9, "lines"),
  #     legend.position = "right")+
  #   ggtitle('') +
  #   labs(x= '', y = 'diepte (m)')
  ##------------------
  hybi2[jaar<2019&jaar>2015,jaar_int:='2016 t/m 2018']
  hybi2[jaar<2022&jaar>2018,jaar_int:='2019 t/m 2021']
  hybi2[jaar<2025&jaar>2021,jaar_int:='2022 t/m 2024']
  # add N obs
  hybi2[,n_obs:=uniqueN(locatie),by =c('EAGIDENT','jaar_int')]
  hybi2[,n_obs:=paste0('Aantal monsters: ',n_obs)]

  sel_agg <- dcast(hybi2, EAGIDENT+jaar_int+n_obs~parameterid, value.var = 'meetwaarde', fun.aggregate = mean, fill=FALSE)
  sel_agg <- sel_agg[!is.na(jaar_int),]
  p<-ggplot() +
    geom_col(data = sel_agg, aes(x= jaar_int, y = -1*WATDTE_m, fill = 'maximale waterdiepte (m)')) +
    geom_col(data = sel_agg, aes(x= jaar_int, y = -1*ZICHT_m, fill = 'doorzicht (m)')) +
    scale_fill_manual(values = c("darkblue","skyblue"), na.value = "#A6761D")+
    facet_grid(.~ EAGIDENT+n_obs, space = 'free_x', scales = 'free_x', switch = 'x')+
    theme_minimal(base_size = 15)+
    theme(
      strip.background = element_blank(),
      strip.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 15, vjust = 0.8, angle = 90),
      axis.text.y = element_text(size = 15),
      axis.title = element_text(size= 15),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      plot.title = element_text(size =18, face="bold", hjust = 0.5),
      panel.background = element_blank(),
      panel.border = element_rect(colour='black', fill = NA),
      plot.background = element_blank(),
      legend.position = "bottom",
      legend.box.just = "center"
    )+
    guides(fill = guide_legend(title = '', title.vjust = 1))+
    guides(color = guide_legend(title = ''))+
    ggtitle(paste0("Waterdiepte & doorzicht")) +
    labs(x= '' , y= 'meter')
  
  # return
  return(p)

}
ppr_plotbod <- function(bod1, type='grid'){

  # dcast slootbodem
  selb <- dcast.data.table(bod1, EAGIDENT+locatie+datum+jaar ~ parameterid+compartiment, value.var = "meetwaarde", fun.aggregate = mean)
  selb[,jaar:= as.character(jaar)]
  
  # calculate relevant ratios SB (ng & dg)
  selb[,FESP_DWratio := (Fe_mg_kg_dg_SB/55.845 - Stot_mgS_kg_dg_SB/32.065)/(Ptot_gP_kg_dg_SB*1000/30.974)]
  selb[,FESP_DWratio_FeP := (Fe_mg_kg_dg_SB/55.845)/(Ptot_gP_kg_dg_SB*1000/30.974)]
  if(is.null(selb$Stot_mgS_l_ng_SB)){
    selb[,FESP_FWratio := FESP_DWratio]}
  if(!is.null(selb$Stot_mgS_l_ng_SB)){
    selb[,FESP_FWratio := (Fe_mg_l_ng_SB/55.845 - Stot_mgS_l_ng_SB/32.065)/(Ptot_mgP_l_ng_SB/30.974)]
    selb[,FESP_FWratio_FeP := (Fe_mg_l_ng_SB/55.845)/(Ptot_mgP_l_ng_SB/30.974)]}
  # convert iron into similar parcode
  if(is.null(selb$Fe_mg_l_nf_PW)&!is.null(selb$Fe_mg_l_PW)){
    selb$Fe_mg_l_nf_PW <- selb$Fe_mg_l_PW}
  if(is.null(selb$Ptot_mgP_l_nf_PW)&!is.null(selb$Ptot_mgP_l_PW)){
    selb$Ptot_mgP_l_nf_PW <- selb$Ptot_mgP_l_PW}
  if(is.null(selb$Stot_mgS_l_nf_PW)&!is.null(selb$Stot_mgS_l_PW)){
    selb$Stot_mgS_l_nf_PW <- selb$Stot_mgS_l_PW}
  # add SP-ratio PW
  if(is.null(selb$Stot_mgS_l_nf_PW)&!is.null(selb$SO4_mg_l_nf_PW)){
    selb[!is.na(SO4_mg_l_nf_PW), FESP_PWratio := (Fe_mg_l_nf_PW/55.845 - SO4_mg_l_nf_PW/96.06)/(Ptot_mgP_l_nf_PW/30.974)]
    selb[!is.na(SO4_mg_l_nf_PW), FESP_PWratio_FeS := (Fe_mg_l_nf_PW/55.845)/(SO4_mg_l_nf_PW/96.06)]
    selb[, FESP_PWratio_FeP := (Fe_mg_l_nf_PW/55.845)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  if(!is.null(selb$Stot_mgS_l_nf_PW)){
    selb[!is.na(Stot_mgS_l_nf_PW),FESP_PWratio := (Fe_mg_l_nf_PW/55.845 - Stot_mgS_l_nf_PW/32.065)/(Ptot_mgP_l_nf_PW/30.974)]
    selb[!is.na(Stot_mgS_l_nf_PW),FESP_PWratio_FeS := (Fe_mg_l_nf_PW/55.845)/(Stot_mgS_l_nf_PW/32.065)]
    selb[,FESP_PWratio_FeP := (Fe_mg_l_nf_PW/55.845)/(Ptot_mgP_l_nf_PW/30.974)]
  }
  # calculate nalevering
  selb[,nlvr_FW := 0.0247 * Ptot_mgP_l_ng_SB - 1.6035]
  if(!is.null(selb$Ptot_mgP_l_nf_PW)){
    selb[,nlvr_PW := 0.8095 * selb$Ptot_mgP_l_nf_PW - 0.2905]  
  }
  
  # add N obs
  selb[,n_obs:=uniqueN(locatie),by =c('EAGIDENT','jaar')]
  selb[,n_obs:=paste0('Aantal monsters: ',n_obs)]
  
  if(nrow(selb)>0){
    #Function for scaling y axis 2 decimals
    scaleFUN <- function(x) sprintf("%.2f", x)
  
  if(!is.null(selb$FESP_FWratio)){
  # filter only op samples where FESPFWratio, FESPDWratio and FESPPWratio are present
  selb <- selb[!(is.na(FESP_FWratio)),]
  # FW
  selb[,classFESP_FWratio := cut(FESP_FWratio, breaks = c((min(FESP_FWratio)-1), 1.4, 4, max(FESP_FWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[FESP_FWratio >= 4, nlvr_FW := 0.1 * nlvr_FW] # BaggerNut Tool zegt 0-1
  selb[FESP_FWratio < 4 & FESP_FWratio > 1.4, nlvr_FW := 0.5 * nlvr_FW] # BaggerNut zegt < nlvrFW & > 0-1
  selb[FESP_FWratio <= 1.4, nlvr_FW := nlvr_FW]
  selb[nlvr_FW < 0,nlvr_FW := 0]
  selb[,jaar:= as.character(jaar)]
  
  plotFW <- ggplot(selb, aes(x= jaar, y= nlvr_FW, fill = classFESP_FWratio))+
    geom_boxplot(outliers = FALSE) +
    scale_y_continuous(labels=scaleFUN)+
    theme_minimal()+
    facet_grid(.~EAGIDENT+n_obs)+
    theme(
      strip.background = element_blank(),
      title = element_text(size= 10),
      axis.text.x = element_text(size= 10),
      axis.text.y = element_text(size= 10),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title=element_text(size=10),
      legend.title = element_text(size = 10, face = 'bold'),
      legend.text  = element_text(size = 10),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right")+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
    ggtitle( "Potentiële nalevering") +
    labs(x="",y="P mg/m2/dag\n", fill = '')}

  if(!is.null(selb$FESP_PWratio)){
    # filter only op samples where FESPFWratio, FESPDWratio and FESPPWratio are present
    selb <- selb[!(is.na(FESP_PWratio)),]
    selb[FESP_PWratio_FeP > 10 & FESP_PWratio_FeS > 1, c('nlvr_PW','classFESP_PWratio') := list(0.1*nlvr_PW,'functionele ijzerval') ]  # dit is ook onder zuurstofloze omstandigheden gunstiger # BaggerNut zegt lage nalevering
    selb[FESP_PWratio_FeP > 1 & FESP_PWratio_FeS > 1, c('nlvr_PW','classFESP_PWratio') := list(0.5*nlvr_PW,'functionele ijzerval') ]  # BaggerNut zegt lage nalevering
    selb[FESP_PWratio_FeP > 1 & FESP_PWratio_FeS <= 1, c('nlvr_PW','classFESP_PWratio') := list(0.5*nlvr_PW,'beperkte ijzerval')] # BaggerNut zegt < nlvrPW 
    selb[FESP_PWratio_FeP <= 1, c('nlvr_PW','classFESP_PWratio') := list(nlvr_PW,'geen ijzerval')]
    selb[nlvr_PW < 0,nlvr_PW := 0]
    
  qPW <- ggplot(selb, aes(x= jaar, y= nlvr_PW, fill = classFESP_PWratio))+
      geom_boxplot(outliers = FALSE) +
      scale_y_continuous(labels=scaleFUN)+
      theme_minimal()+
      facet_grid(.~EAGIDENT+n_obs)+
      theme(
        strip.background = element_blank(),
        title = element_text(size= 10),
        axis.text.x = element_text(size= 10),
        axis.text.y = element_text(size= 10),
        axis.ticks =  element_line(colour = "black"),
        axis.line = element_line(colour='black'),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.title=element_text(size=10) )+
      theme(legend.title = element_text(size = 10, face = 'bold'),
            legend.text  = element_text(size = 10),
            legend.key.size = unit(0.9, "lines"),
            legend.position = "right")+
      scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'), drop = FALSE)+
      ggtitle( "Actuele nalevering uit de waterbodem obv poriewatermetingen") +
      labs(x="",y="P mg/m2/dag\n", fill = '')
  }
  
  if(!is.null(selb$Ptot_gP_kg_dg_SB)){  
    selb <- selb[!(is.na(FESP_DWratio)),]
    selb[,classFESP_DWratio := cut(FESP_DWratio, breaks = c((min(FESP_DWratio)-1), 1.4, 4, max(FESP_DWratio)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  qBS <- ggplot(selb, aes(x= jaar, y= Ptot_gP_kg_dg_SB, fill = classFESP_DWratio))+
    geom_boxplot(outliers = FALSE) +
    scale_y_continuous(labels=scaleFUN)+
      facet_grid(.~EAGIDENT+n_obs)+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      title = element_text(size= 10),
      axis.text.x = element_text(size= 10),
      axis.text.y = element_text(size= 10),
      axis.ticks =  element_line(colour = "black"),
      axis.line = element_line(colour='black'),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title=element_text(size=10) )+
    theme(legend.title = element_text(size = 10, face = 'bold'),
          legend.text  = element_text(size = 10),
          legend.key.size = unit(0.9, "lines"),
          legend.position = "right")+
    scale_fill_manual(values = c('red', 'salmon', 'lightblue'), labels = c('FE/P < 1.4', 'FE/P < 4', 'FE/P >= 4'), drop = FALSE)+
    ggtitle( "Voedselrijkdom waterbodem") +
    labs(x="",y="P g/kg dg", fill = '')
  }
    
  # plot figure
  if(type=='plotFW'){out = plotFW}
  if(type=='plotqPW'){out = qPW}
  if(type=='grid' & !is.null(selb$FESPPWratio) & !is.null(selb$FESPFWratio)){out = plot_grid(plotFW, qPW, ncol = 1, align = "v")}
  if(type=='grid' & !is.null(selb$FESPPWratio) & !is.null(selb$FESPFWratio) & !is.null(selb$Ptot_gP_kg_dg_SB)){out = plot_grid(plotFW, qPW, qBS, ncol = 1, align = "v")}
  if(type=='grid' & is.null(selb$FESPPWratio) & !is.null(selb$FESPFWratio)){out = plotFW}
  if(type=='grid' & is.null(selb$FESPPWratio) & !is.null(selb$FESPFWratio)& !is.null(selb$Ptot_gP_kg_dg_SB)){out = plot_grid(plotFW, qBS, ncol = 1, align = "v")}
  if(type=='grid' & !is.null(selb$FESPPWratio) & is.null(selb$FESPFWratio)){out = qPW}
  if(type=='grid' & !is.null(selb$FESPPWratio) & is.null(selb$FESPFWratio)& !is.null(selb$Ptot_gP_kg_dg_SB)){out = plot_grid(qPW, qBS, ncol = 1, align = "v")}
  if(type=='grid' & is.null(selb$FESPPWratio) & is.null(selb$FESPFWratio) & !is.null(selb$Ptot_gP_kg_dg_SB)){out = qBS}  
  # return output
  return(out)
  }
}



    
  

  

          