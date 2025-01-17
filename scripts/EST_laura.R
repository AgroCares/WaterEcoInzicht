
#i <- unique(macft$monsterident)[1]
# i <- 'WP556666' w3, w6, w7
# i <- 'WP527261'
# i<- "WP527185"
# i <- macft$monsterident[macft$compartiment == 'OR'][1]


monstextract <- function(i, macft, soortenlijst_submers, soortenlijst_kroos, soortenlijst_oever, grenswaarden_EST){
  # monsters zijn slechts 1 compartiment en komen samen op loc niveau, dus niet te veel filteren op dit niveau, later wordt er geaggregeerd naar loc
  # i <- unique(macft$monsterident)[1]
  sel <- unique(macft[macft$monsterident == i,])
  # parameters water---------  
  doorz_diep <- ifelse(length(sel$meetwaarde[sel$parameter %in% "ZICHT"])>0 &
                         length(sel$meetwaarde[sel$parameter %in% "WATDTE"])>0,
                       sel$meetwaarde[sel$parameter %in% "ZICHT"]/sel$meetwaarde[sel$parameter %in% "WATDTE"], NA)
  
  if(length(doorz_diep)==0){out <- NULL}else{ # hiermee worden automatisch alle oeverlocaties eruit gefilterd want op de droge oever worden geen doorzicht en diepte gemeten
  
  diepte <- ifelse(length(sel$meetwaarde[sel$parameter %in% "WATDTE"])>0, sel$meetwaarde[sel$parameter %in% "WATDTE"], NA)
  slib <- ifelse(length(sel$meetwaarde[sel$parameter %in% "SLIBDTE"])>0, sel$meetwaarde[sel$parameter %in% "SLIBDTE"], NA)
  talud <- ifelse(length(sel$meetwaarde[sel$parameterid %in% "TALBVWTR_graad"])>0, sel$meetwaarde[sel$parameterid %in% "TALBVWTR_graad" ], NA)
   
  n_soort_sub <- nrow(sel[sel$parameter %in% "" & sel$parameterfractie %in% "" & sel$biotaxonnaam %in% soortenlijst_submers,]) #LET OP: DIT IS VOOR W5 en verder
  
  woeker <- ifelse(length(sel$meetwaarde[sel$parameter %in% "" & sel$biotaxonnaam %in% soortenlijst_submers])==0,
                   0, max(sel$meetwaarde[sel$parameter %in% "" & sel$biotaxonnaam %in% soortenlijst_submers])) # max bedekking van 1 submers soort
  
  SUBMS <- sel$meetwaarde[sel$parameter %in% "SUBMSPTN" & sel$grootheid %in% "BEDKG"]
  if(!length(SUBMS)>0){
    SUBMS <- min(100, sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_submers])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  
  KROOS <- sel$meetwaarde[sel$parameter %in% "KROOS" & sel$grootheid %in% "BEDKG"]
  if(!length(KROOS)>0){
    KROOS <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_kroos])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  
  FLAB <- sel$meetwaarde[sel$parameter %in% "FLAB" & sel$parameterfractie %in% "DRIJVD" & sel$grootheid %in% "BEDKG"]
  if(!length(FLAB)>0){
    FLAB <- 0.001 # als ontbreekt dan is er geen flab
  }
  
  # parameters oever----------  
  beschoeid <- if(length(sel$meetwaarde[sel$parameter %in% "OEVBSIG"])==0){NA}else{if(any(sel$meetwaarde[sel$parameter %in% "OEVBSIG"] %in% c('31','32','41','52','35'))){'ja'}else{"nee"}}
  n_emsoort <- nrow(sel[sel$biotaxonnaam %in% soortenlijst_emers & sel$compartiment == 'EZ',])
  emers <- sel$meetwaarde[sel$parameter %in% "EMSPTN" & sel$compartiment == 'EZ' & sel$grootheid %in% "BEDKG"]
  
 if(!length(emers)>0){
    emers <- min(100,sum(sel$meetwaarde[sel$biotaxonnaam %in% soortenlijst_oever])) #als groeivormmeting ontbreekt, dan bedekkingen optellen
  }
  riet <- sel$meetwaarde[sel$biotaxonnaam %in% "Phragmites australis"]
  if(length(riet)<1){riet <- 0}
  
  #W1 -----------
  #water met flab/draadalgen, weinig soorten en geen woekerende planten
  W1 <- NA
  if(FLAB >= grens_flab & n_soort_sub < grens_n_soort & woeker < grens_woeker){W1 <- 1}else{W1  <- 0}
  
  
  #W2----
  #water met kroos en weinig soorten (en wel of geen woekerende/drijfblad planten)
  W2 <- NA
  if(KROOS >= grens_kroos & n_soort_sub < grens_n_soort){W2 <- 1}else{W2  <- 0}
  
  #W3-----
  #water met drijfbladplanten
  w3_sel <- sel[sel$parameter %in% "DRIJFBPTN", ]
  W3 <- NA
  if(nrow(w3_sel)==0){W3  <- 0}else{
  if(w3_sel$meetwaarde >= grens_drijf & n_soort_sub < grens_n_soort & woeker < grens_woeker){W3 <- 1} #1=ja, 0=nee, 99=onbekend
  if(w3_sel$meetwaarde < grens_drijf){W3 <- 0}
  }
  
  #W6----
  #water met veel woekerende waterplanten (en weinig soorten)
  if(n_soort_sub < grens_n_soort & woeker >= grens_woeker){W6<-1}
  if(!(n_soort_sub < grens_n_soort & woeker >= grens_woeker)){W6 <-0}
  
  if(!is.na(doorz_diep)){
  #W4----
  # troebel, weinig planten
  if(doorz_diep < grens_zicht & SUBMS < grens_submers){W4 <- 1}else{W4 <- 0}
  
  #W4a----
  # troebel, veel planten: hier is doorzicht/diepte dus geen goede indicator
  if(doorz_diep < grens_zicht & SUBMS >= grens_submers & woeker < grens_woeker){W4a <- 1}else{W4a <- 0}
  
  #W5----
  #helder water met veel waterplanten in hoge bedekking (en meer dan 5 soorten)
  if(doorz_diep >= grens_zicht & n_soort_sub >= grens_n_soort & SUBMS >= grens_submers ){W5 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort_sub >= grens_n_soort & SUBMS >= grens_submers )){W5 <- 0}

  #W7-----
  #helder water met weinig soorten (1 en 5) niet woekerende, ondergedoken waterplanten 
  if(doorz_diep >= grens_zicht & n_soort_sub <= grens_n_soort & n_soort_sub >= 1 & woeker < grens_woeker){W7<-1}
  if(!(doorz_diep >= grens_zicht & n_soort_sub <= grens_n_soort & n_soort_sub >= 1 & woeker < grens_woeker )){W7<-0}
  
  #W8----
  #helder water met veel soorten ondergedoken waterplanten in lage dichtheid
  # waterdiepte toevoegen?
  W8 <- NA
  if(doorz_diep >= grens_zicht & n_soort_sub > grens_n_soort & SUBMS < grens_submers ){W8 <- 1}
  if(!(doorz_diep >= grens_zicht & n_soort_sub > grens_n_soort & SUBMS < grens_submers )){W8 <- 0}

  #W9----
  #helder water zonder waterplanten
  if(doorz_diep >= grens_zicht & n_soort_sub < 1 & FLAB < grens_flab & KROOS < grens_kroos){W9<-1}else{W9<-0}
  }else{W4<-NA ;W4a<-NA;W5<-NA;W7<-NA;W8<-NA;W9<-NA}
 
  if(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet < gr_riet){O1 <- 1}
  if(!(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet < gr_riet)){O1 <- 0}
  
  if(beschoeid %in% "ja" & n_emsoort >= gr_soorten & riet < gr_riet){O2 <- 1}
  if(!(beschoeid %in% "ja" & n_emsoort >= gr_soorten & riet < gr_riet)){O2 <- 0}
  
  if(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet >= gr_riet){O3 <- 1}
  if(!(beschoeid %in% "ja" & n_emsoort < gr_soorten & riet >= gr_riet)){O3 <- 0}
  
  if(beschoeid %in% "ja" & n_emsoort >= gr_soorten & riet >= gr_riet){O4 <- 1}
  if(!(beschoeid %in% "ja" & n_emsoort >= gr_soorten & riet >= gr_riet)){O4 <- 0}
  
  if(beschoeid %in% "nee" &  n_emsoort < gr_soorten & riet < gr_riet){O5 <- 1}
  if(!(beschoeid %in% "nee" &  n_emsoort < gr_soorten & riet < gr_riet)){O5 <- 0}
  
  if(beschoeid %in% "nee" & n_emsoort >= gr_soorten & riet < gr_riet){O6 <- 1}
  if(!(beschoeid %in% "nee" & n_emsoort >= gr_soorten & riet < gr_riet)){O6 <- 0}
  
  if(beschoeid %in% "nee" & n_emsoort < gr_soorten & riet >= gr_riet){O7 <- 1}
  if(!(beschoeid %in% "nee" & n_emsoort < gr_soorten & riet >= gr_riet)){O7 <- 0}
  
  if(beschoeid %in% "nee" & n_emsoort >= gr_soorten & riet >= gr_riet){O8 <- 1}
  if(!(beschoeid %in% "nee" & n_emsoort >= gr_soorten & riet >= gr_riet)){O8 <- 0}
  
  # make a list to store the output
  out <- data.table(EAGIDENT= unique(sel$EAGIDENT), 
              jaar = unique(sel$jaar), 
              locatie = unique(sel$locatie),
              watertype = unique(sel$watertype),
              monsterident= unique(sel$monsterident),
              compartiment= unique(sel$compartiment),
              doorz_diep,
              diepte,
              slib,
              talud,
              FLAB,
              KROOS,
              SUBMS,
              emers,
              woeker,
              n_soort_sub,
              n_emsoort,
              beschoeid,
              W1,W2,W3,W4,W4a,W5,W6,W7,W8,W9,O1,O2,O3,O4,O5,O6,O7,O8
          
  )
  }
  # return list with relevant properties
  return(out)
}

EST_aggloc <- function(est){
  cols <- c('monsterident','doorz_diep','diepte','slib','talud','FLAB','KROOS','SUBMS','emers','woeker','n_soort_sub','n_emsoort','beschoeid')
  estloc <- estout[,lapply(.SD, sum, na.rm=TRUE), by=c('EAGIDENT','locatie','compartiment','jaar','watertype'),.SDcols = -cols]
  cols2 <- c('monsterident',"W1","W2","W3","W4","W4a","W5","W6","W7","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  estloc2 <- estout[,lapply(.SD, median, na.rm=TRUE), by=c('EAGIDENT','locatie','compartiment','jaar','watertype'),.SDcols = -cols2]
  estloc <- merge(estloc,estloc2, by=c('EAGIDENT','locatie','compartiment','jaar','watertype'))
  write.table(estloc, paste0("output/estlocatie_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estloc)
}
EST_addnameloc <- function(estloc, eag_wl){  
  estloc <- estloc[rowSums(estloc[,6:15]) > 0,]
  estloc$W <- colnames(estloc[,6:15])[max.col(estloc[,6:15],ties.method="first")]
  estloc$O <- colnames(estloc[,16:23])[max.col(estloc[,16:23],ties.method="first")]
  estmergl <- merge(estloc[,-'watertype'], eag_wl[,c('EAGIDENT', "type","StedelijkLandelijk","watertype")], by = c('EAGIDENT'))
  estmergl$ESTnaam2[estmergl$watertype == 'M20'] <-  'DM'
  estmergl$ESTnaam2[estmergl$watertype %in% c('M14','M27',"M25","M11")] <-  'OM'
  estmergl$ESTnaam2[estmergl$watertype %in% c('M1a','M1b','M8',"M10","M3")] <- 'Sl'
  estmergl$ESTnaam2[estmergl$watertype %in% c("M6b",'M30',"M7b", "M6a")] <- 'K'
  estmergl$ESTnaam3[estmergl$StedelijkLandelijk == 'Stedelijk'] <- 'St'
  estmergl$ESTnaam3[estmergl$StedelijkLandelijk == 'Landelijk'] <- 'L'
  estmergl$estnaam <- paste0(estmergl$W,'_',estmergl$O,'_',estmergl$ESTnaam2,'_', estmergl$ESTnaam3)
  
  write.table(estmergl, paste0("output/estlocnaam_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estmergl)
}

EST_aggeag <- function(estloc){
  agg_cols <- c("W1","W2","W3","W4","W4a","W5","W6","W7","W8","W9","O1","O2","O3","O4","O5","O6","O7","O8")
  esteag <- estloc[, lapply(.SD, sum, na.rm=TRUE), by=c('EAGIDENT','compartiment','jaar','watertype'),.SDcols = agg_cols]
  agg_cols <- c('doorz_diep','diepte','slib','FLAB','KROOS','SUBMS','woeker','n_soort_sub','talud','emers','n_emsoort','beschoeid')
  esteag2 <- estloc[,lapply(.SD, median, na.rm=TRUE), by=c('EAGIDENT','compartiment','jaar','watertype'),.SDcols = agg_cols]
  
  esteag <- merge(esteag, esteag2, by=c('EAGIDENT','compartiment','jaar','watertype'))
  # bepaalde pars zijn alleen in EZ compartiment relevant en komen dus op NA (die niet wordt meegenomen in gemiddelde van compartimenten)
  esteag[esteag$compartiment == 'OW', c('talud','emers','n_emsoort','beschoeid')] <- NA
  # 50/50 weging van EST per compartiment vanwege vertegenwoordiging verschillende compartimenten
  esteag <- esteag[,lapply(.SD, mean, na.rm=TRUE), by=c('EAGIDENT','jaar','watertype'),.SDcols = -c('compartiment')]
  write.table(esteag, paste0("output/esteag_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(esteag)
}
EST_addnameeag <- function(esteag, grenswaarden_EST, eag_wl){
  esteag <- esteag[rowSums(esteag[,4:13]) > 0,]
  esteag$W <- colnames(esteag[,4:13])[max.col(esteag[,4:13],ties.method="first")]
  esteag$O <- colnames(esteag[,14:21])[max.col(esteag[,14:21],ties.method="first")]
  estmerg <- merge(esteag[,-'watertype'], eag_wl[,c('EAGIDENT', "type","StedelijkLandelijk","watertype","GAFNAAM","KRW_SGBP3","KRWmonitoringslocatie_SGBP3","SGBP3_NAAM")], by = c('EAGIDENT'))
  estmerg$ESTnaam2[estmerg$watertype == 'M20'] <-  'DM'
  estmerg$ESTnaam2[estmerg$watertype %in% c('M14','M27',"M25","M11")] <-  'OM'
  estmerg$ESTnaam2[estmerg$watertype %in% c('M1a','M1b','M8',"M10","M3")] <- 'Sl'
  estmerg$ESTnaam2[estmerg$watertype %in% c("M6b",'M30',"M7b", "M6a")] <- 'K'
  estmerg$ESTnaam3[estmerg$StedelijkLandelijk == 'Stedelijk'] <- 'St'
  estmerg$ESTnaam3[estmerg$StedelijkLandelijk == 'Landelijk'] <- 'L'
  estmerg$estnaam <- paste0(estmerg$W,'_',estmerg$O,'_',estmerg$ESTnaam2,'_', estmerg$ESTnaam3)
  estmerg <- merge(grenswaarden_EST[,c('omschrijving','type')], estmerg, by.y = 'W', by.x = 'type', all.y = T, allow.cartesian =T)
  estmerg <- merge(grenswaarden_EST[,c('omschrijving','type')], estmerg, by.y = 'O', by.x = 'type', all.y = T, allow.cartesian =T)
  estmerg$type <- NULL; estmerg$type.y <-NULL
  estmerg$estnaamvol <- paste0(estmerg$estnaam,'_',estmerg$omschrijving.y,'_', estmerg$omschrijving.x)
  write.table(estmerg, paste0("output/esteagnaam_", Sys.Date(),".csv"), sep=";", dec=".", row.names=F)
  return(estmerg)
}  

printestplots <- function(estekr){
  
  for(i in unique(paste0(estekrloc$W,estekrloc$ESTnaam2, estekrloc$ESTnaam3))){
    i <- unique(paste0(estekrloc$W,estekrloc$ESTnaam2,estekrloc$ESTnaam3))[1]
    plotdata <- estekrloc[paste0(estekrloc$W,estekrloc$ESTnaam2,estekrloc$ESTnaam3) == i,]
    p <- ggplot(plotdata, aes(x= estnaam, y= `Overige waterflora-kwaliteit`, label = paste0(EAGIDENT, jaar)))+
      geom_boxplot() +
      #facet_grid(~jaar, scales = 'free')+
      theme_minimal()+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_text(size = 6), 
        strip.text.y = element_text(size = 5), 
        axis.text.x = element_text(size= 8, angle=90,hjust=1),
        axis.text.y = element_text(size= 8, hjust=2),
        axis.ticks =  element_line(colour = "black"),
        panel.background = element_blank(),
        plot.background = element_blank()
      )+
      ggtitle('') +
      labs(x= 'ecosysteem toestand' , y= 'ekr flora')
    ggsave(paste0("./output/ekrest_W",i,".png"))
  }
}

kaartEST <- function(){
  library(sp)
  library(plotGoogleMaps)
  estmap <- merge(gEAG,estmergl, by.x = "GAFIDENT" , by.y = "EAGIDENT" )
  # coordinates(estmap)<-~x+y
  
  #using plotGoogleMaps::pieSP to generate the spatial data.frame for pie-chart
  pies <- pieSP(estmap,zcol=unique(estmap$estnaam), max.radius=50)
  pies$pie=rep(unique(estmap$estnaam),155)
  
  # m=plotGoogleMaps(pies, zcol='pie') #run this to show the java-based output of piechart on map
  
  #Extract spatial polygon data.frame 
  library(broom)
  library(ggplot2)
  
  names(pies@polygons)<-pies$pie
  pi<-tidy(pies)
  
  ggplot() +
    geom_polygon(data=pi, aes(x=long, y=lat, group=id, fill=.id))
}

kaartEKRmp <- function(dt = dt,
                       EAGsel = EAGsel,
                       waterpereagsel = waterpereagsel,
                       glocs = glocs,
                       ekr_col = c("red", "orange", "yellow", "green"),
                       ekr_labels = c("slecht","ontoereikend","matig","goed"), 
                       ekr_breaks = c(0, 0.2, 0.4, 0.6, 1)){
  
  dt <- dt[!is.na(EKR),  cat1 := as.integer(cut(EKR, ekr_breaks, labels = 1:4, include.lowest = T))]
  pl <- merge(glocs, dt, by.y ='mpid2', by.x = 'CODE', all.y = TRUE)
  sort(pl$jaar, decreasing = T)
            
  
  bboxEAG <- st_bbox(EAGsel)
  
  p <- 
    ggplot()+
    geom_sf(data = waterpereagsel, color = NA, fill = 'blue') +
    geom_sf(data = EAGsel, color = 'black', fill = NA, size = 0.5, inherit.aes = F) +
    geom_sf(data = pl, aes(fill = as.factor(cat1), color = as.factor(cat1)), 
            size = 5, colour="black", pch=21, show.legend = TRUE, inherit.aes = F) +
    #scale_size(range= c(2,9))+
    scale_fill_manual(values = c("1" =  ekr_col[1],
                                 "2" = ekr_col[2],
                                 "3" = ekr_col[3],
                                 "4" = ekr_col[4]), drop =T, labels = ekr_labels) +
    scale_colour_manual(values = c("1" =  ekr_col[1],
                                   "2" = ekr_col[2],
                                   "3" = ekr_col[3],
                                   "4" = ekr_col[4]), drop =T, labels = ekr_labels, guide = "none") +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks =  element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.key.size = unit(1, 'cm'),
      legend.title=element_text(size=10), 
      legend.text=element_text(size=9)
    )+
    guides(fill = guide_legend(title = unique(pl$GHPR)))+
    labs(x = "", y = "")+
    coord_sf(xlim = c(bboxEAG$xmin,bboxEAG$xmax), ylim = c(bboxEAG$ymin,bboxEAG$ymax), datum = NA)+
    ggtitle(unique(pl$waterlichaam), subtitle= paste0(unique(pl$GPHRnew),unique(pl$jaar)))
    
    ggsave(p, file = paste0('output/ekrstippen/', unique(pl$gebied),unique(pl$GHPR),unique(pl$jaar),'.png'), units='cm', dpi=600)
    print(paste0(unique(pl$EAGIDENT),unique(pl$GHPR)))
    }


toestandbeschrijving <- function(hybiest){
  # hybiest <- est_wq

  hybiest$toestandb <- paste0(hybiest$omschrijving.y, 
                            ifelse(!is.na(hybiest$CHLFa_ug_l_spectro),
                            ifelse(hybiest$CHLFa_ug_l_spectro > 25 & !is.na(hybiest$CHLFa_ug_l_blauwalg) & hybiest$CHLFa_ug_l_blauwalg > 12, paste0(" en er zaten veel blauwalgen in het water in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ". "),
                            ifelse(hybiest$CHLFa_ug_l_spectro > 25 & !is.na(hybiest$CHLFa_ug_l_groenalg) & hybiest$CHLFa_ug_l_groenalg > 15, paste0(" en er zaten veel groenalgen in het water in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ". "),
                            ifelse(hybiest$CHLFa_ug_l_spectro > 25, paste0(" en er zaten veel algen in het water in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ". " ), paste0(" en er zaten weinig algen in het water in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ". " )))), paste0(" in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ". ")),
                            hybiest$omschrijving.x, " in ", hybiest$jaar, " in ", hybiest$EAGIDENT, ".")
  hybiest <- hybiest[,c("EAGIDENT","jaar", "SUBMS","FLAB","KROOS","n_soort_sub","n_emsoort","CHLFa_ug_l_spectro","CHLFa_ug_l_blauwalg","CHLFa_ug_l_groenalg","toestandb")]
  return(hybiest)
}

toestandbeschrijving_uitgebreid <- function(hybiest){
  
  hybiest$toestandb <- paste0("In ",hybiest$EAGIDENT,": ",hybiest$omschrijving.y, 
                              ". In ", hybiest$jaar, " zijn de mediane bedekkingen met onderwaterplanten, drijvende draadalgen en kroos respectievelijk ", round(hybiest$SUBMS, 1),
                              ", ", round(hybiest$FLAB,1)," en ",round(hybiest$KROOS,1), "%",
                              ". Het mediane aanal soorten onderwaterplanten per meetlocatie is ", as.integer(hybiest$n_soort_sub),
                              ifelse(!is.na(hybiest$CHLFa_ug_l_spectro),
                                     ifelse(hybiest$CHLFa_ug_l_spectro > 25 & !is.na(hybiest$CHLFa_ug_l_blauwalg) & hybiest$CHLFa_ug_l_blauwalg > 12, paste0(". In ", hybiest$jaar, "zaten er veel blauwalgen in het water. "),
                                            ifelse(hybiest$CHLFa_ug_l_spectro > 25 & !is.na(hybiest$CHLFa_ug_l_groenalg) & hybiest$CHLFa_ug_l_groenalg > 15, paste0(". In ", hybiest$jaar, "zaten er veel groenalgen in het water. "),
                                                   ifelse(hybiest$CHLFa_ug_l_spectro> 25, paste0(". In ", hybiest$jaar, " zaten er veel algen in het water. "),paste0(". In ", hybiest$jaar, " zaten er weinig algen in het water. " )))),". "),
                              hybiest$omschrijving.x,
                              ". De mediane bedekking met emerse planten was in ",  hybiest$jaar, " ",round(hybiest$emers,1),
                              "% en het mediane aanal soorten natte oeverplanten (emerse planten) per meetlocatie was ", as.integer(hybiest$n_emsoort),
                              ".")
  
  hybiest <- hybiest[,c("EAGIDENT","jaar", "SUBMS","FLAB","KROOS","n_soort_sub","n_emsoort","CHLFa_ug_l_spectro","CHLFa_ug_l_blauwalg","CHLFa_ug_l_groenalg","toestandb")]
  return(hybiest)
}


