# visualisations 4 KRW-app -----------------
# plot EKR per EAG
ppr_ekrplot_EAG <- function(ekr_score){
  
  # make local copy
  dt <- copy(ekr_score)
  dt <- dt[level == 1,]
  
  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := paste0(EAGIDENT," ",facet_wrap_code)]
  
  # build background [Kan eleganter..]
  bg <- unique(dt[, c("waterlichaam", "GEP_2022", "wlmt")])
  
  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]
  
  # reformat
  bg_gather <- melt(bg,id.vars = c('waterlichaam','GEP_2022','wlmt'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast.data.table(bg_gather,waterlichaam+GEP_2022+wlmt+doelen~varrange,value.var='waarde')
  bg_spr[,Oordeel := as.factor(doelen)]
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)
  
  ## make plot
  plot <- ggplot(dt, aes(x = waterlichaam, y = EKR3jr)) +
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
  dt <- dt[level == 1,]
  
  # facet per maatlat en EAG (als FS niveau is GAF en meerdere EAGs)
  dt[,facet_wrap_code := as.character(facet_wrap_code)]
  dt[,wlmt := paste0(waterlichaam," ",facet_wrap_code)]
  
  # build background [Kan eleganter..]
  bg <- unique(dt[, c("waterlichaam", "GEP_2022", "wlmt")])
  
  # add boundaries for new GEP
  bg[,c('goed_ymin_new','goed_ymax_new') := .(GEP_2022,1)]
  bg[,c('matig_ymin_new','matig_ymax_new') := .(GEP_2022 / 3 * 2,GEP_2022)]
  bg[,c('ontoereikend_ymin_new','ontoereikend_ymax_new') := .(GEP_2022 / 3,GEP_2022 / 3 * 2)]
  bg[,c('slecht_ymin_new','slecht_ymax_new') := .(0,GEP_2022 / 3)]
  
  # reformat
  bg_gather <- melt(bg,id.vars = c('waterlichaam','GEP_2022','wlmt'),
                    variable.name = 'doelen',value.name = 'waarde')
  bg_gather[,sgbp_version := fifelse(grepl('_new$',doelen),'new','old')]
  bg_gather[,varrange := fifelse(grepl('_ymin_',doelen),'ymin','ymax')]
  bg_gather[,doelen := gsub("(.+?)(\\_.*)", "\\1", doelen)]
  bg_spr <- dcast.data.table(bg_gather,waterlichaam+GEP_2022+wlmt+doelen~varrange,value.var='waarde')
  
  # 
  bg_spr[,Oordeel := as.factor(doelen)]
  
  #Create a custom color scale
  myColors <- c("#00FF00", "#FFFF33", "#FF8000", "#FF0000")
  names(myColors) <- levels(bg_spr$doelen)
  
  ## make plot
  plot <- ggplot(dt, aes(x = waterlichaam, y = EKR3jr)) +
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
    z$KRW_SGBP3 <- ""
  }
  
  z <- z %>%
    dplyr::arrange(GHPR_level) %>%               # sort your dataframe
    dplyr::mutate(GHPR = factor(GHPR, unique(GHPR))) # reset your factor-column based on that order
  z$jaar <- as.numeric(z$jaar)
  z$Numeriekewaarde <- as.numeric(z$Numeriekewaarde)
  z$facet_wrap_code <- as.character(z$facet_wrap_code)
  z$wlmt <- ifelse(is.na(z$KRW_SGBP3)|z$KRW_SGBP3 == "", paste0(z$EAGIDENT," ",z$facet_wrap_code), z$facet_wrap_code)
  
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
 
  l[,GHPR_level := as.factor(GHPR_level)]
  l[,GHPR_level := factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))]
  l[,klasse := factor(l$klasse, levels = c("[0,0.2]","(0.2,0.4]","(0.4,0.6]","(0.6,0.8]","(0.8,1]"), labels = c("0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))]
  
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
 
  l$GHPR_level <- as.factor(l$GHPR_level)
  l$GHPR_level <- factor(l$GHPR_level, levels = rev(levels(l$GHPR_level)))
  l[,klasse := factor(l$klasse, levels = c("[0,0.2]","(0.2,0.4]","(0.4,0.6]","(0.6,0.8]","(0.8,1]"), labels = c("0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"))]
  
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
  map <- sp::merge(gEAG, gebiedData[, c('param','EKR3jr','EKRref','oordeel_2022','EAGIDENT','GEP_2022','KRWwatertype.code',
                                        'GHPR_level','EKRperc90.all.year')], by = 'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  
  map2 <- map[map$EAGIDENT %in% c('3000-EAG-3','3000-EAG-4','3000-EAG-2','2000-EAG-7','2000-EAG-2','2000-EAG-3','2000-EAG-4','2000-EAG-5','2000-EAG-6'),]
  
  
  leaflet() %>%
    addPolygons(data = map, layerId = map$EAGIDENT, popup= paste("EAG naam", map$EAGNAAM, "<br>",
                                                                 "EKR score:", map$EKR3jr, "<br>",
                                                                 "Referentiescore:", map$EKRref, "<br>",
                                                                 "Oordeel:", map$oordeel_2022, "<br>",
                                                                 "Doel:", map$GEP_2022, "<br>",
                                                                 "Percentiel90:", map$EKRperc90.all.year, "<br>",
                                                                 "Maatlat:", map$GHPR_level ),
                stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(param), fillOpacity = 0.6) %>%
    addPolygons(data= map2, layerId = map2$EAGIDENT, popup= paste("EAG naam", map2$EAGNAAM, "<br>",
                                                                  "EKR score:", map2$EKR3jr, "<br>",
                                                                  "Referentiescore:", map2$EKRref, "<br>",
                                                                  "Oordeel:", map2$oordeel_2022, "<br>",
                                                                  "Doel:", map2$GEP_2022, "<br>",
                                                                  "Percentiel90:", map2$EKRperc90.all.year, "<br>",
                                                                  "Maatlat:", map2$GHPR_level),
                stroke = T, color= 'grey', opacity=0.8, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(param), fillOpacity = 1) %>%
    addLegend("bottomright", colors=col, labels=labels, title = param)%>%
    addProviderTiles("Esri.WorldGrayCanvas")#addTiles()
}
# plot trend per EAG
plottrendEAG <- function(gebiedData, gEAG, maatlat = "2V21 Soortensamenstelling macrofyten"){
  
  # gebiedData <- trendekreag[trendekreag$GHPR_level %in% "1F3 Aantal per volume Fytopl. - bloeisoort (01-02) Planktothrix agardhii",]
  
  gebiedData <- gebiedData[GHPR_level %in% maatlat,]
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
  
  map <- sp::merge(gEAG, gebiedData[, c('klasse','estimate','p.value','r.squared','EAGIDENT', 'KRWwatertype.code', 'GHPR',
                                        'GHPR_level','facet_wrap_code')], by = 'EAGIDENT', duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$EAGIDENT, popup= paste("EAG naam", map$EAGNAAM, "<br>",
                                                     "EAG code", map$EAGIDENT, "<br>",
                                                     "EKR trend:", map$estimate, "<br>",
                                                     "trend significantie:", map$p.value, "<br>",
                                                     "R2:", map$r.squared, "<br>"
    ),
    stroke = T, color= 'black', opacity=0.8, weight = 1, smoothFactor = 0.8,
    fill=T, fillColor = ~pal(klasse), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors = col, labels = labels, title = unique(map$GHPR))%>%
    addProviderTiles("Esri.WorldGrayCanvas")
  
}
plottrendEAG2 <- function(gebiedData, gEAG, maatlat = "Soortensamenstelling macrofyten Waterplanten"){
  
  # gebiedData <- trendekreag[trendekreag$GHPR_level %in% "1F3 Aantal per volume Fytopl. - bloeisoort (01-02) Planktothrix agardhii",]
  
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
  
  map <- sp::merge(gEAG, gebiedData[, c('klasse','estimate','p.value','r.squared','EAGIDENT', 'KRWwatertype.code', 'GHPR',
                                        'GHPR_level','facet_wrap_code')], by = 'EAGIDENT', duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$EAGIDENT, popup= paste("EAG naam", map$EAGNAAM, "<br>",
                                                     "EAG code", map$EAGIDENT, "<br>",
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
  # gebiedData <- ekrtrend[ekrtrend$facet_wrap_code == 'Vis',]
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
  
  map <- sp::merge(pl, gebiedData[, c('klasse','estimate','p.value','r.squared','KRW_SGBP3', 'KRWwatertype.code',
                                      'GHPR_level','facet_wrap_code')], by.x = 'OWMIDENT', by.y =
                     'KRW_SGBP3', duplicateGeoms = T)
  
  leaflet(map) %>%
    addPolygons(layerId = map$KRW_SGBP3, popup= paste("KRW naam", map$OWMNAAM, "<br>",
                                                      "EAG code", map$KRW_SGBP3, "<br>",
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
  groups <- c('EAGIDENT','waterlichaam','KRWwatertype.code','KRW_SGBP3','GHPR_level','GHPR','level','Waardebepalingsmethode.code') 
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
                                        'GHPR_level')], by.x = 'EAGIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  leaflet() %>%
    addPolygons(data = map, layerId = map$EAGIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
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
  b = dcast(wq1, EAGIDENT+jaar+WATERTYPE ~ parameterid,
            value.var = "meetwaarde", fun.aggregate = mean, na.rm =TRUE, fill = NaN, subset = .(parameterid == 'CHLFa_ug_l_spectro'))
  b <- b[!is.na(b$CHLFa_ug_l_spectro)& !is.na(b$jaar) & !is.na(b$EAGIDENT) & !b$EAGIDENT == '', ]
  
  '1' -> b$CHLFAkl[b$CHLFa_ug_l_spectro > 200]
  '2' -> b$CHLFAkl[b$CHLFa_ug_l_spectro <= 200 & b$CHLFa_ug_l_spectro > 100]
  '3' -> b$CHLFAkl[b$CHLFa_ug_l_spectro <= 100 & b$CHLFa_ug_l_spectro > 50]
  '4' -> b$CHLFAkl[b$CHLFa_ug_l_spectro <= 50 & b$CHLFa_ug_l_spectro > 25]
  '5' -> b$CHLFAkl[b$CHLFa_ug_l_spectro <= 25 & b$CHLFa_ug_l_spectro > 10]
  '6' -> b$CHLFAkl[b$CHLFa_ug_l_spectro <= 10]
  b$CHLFAkl <- as.factor(b$CHLFAkl)
  
  colc <- c('1'="darkred",'2'="red", '3'="orange",'4'="yellow",'5'="deepskyblue", '6'= 'blue')
  labelsc <- c('1'=">200",'2'="200-100" ,'3'="100-50",'4'="50-25",'5'="25-10",'6' = '<10')
  pal <- colorFactor(palette = colc,  domain = b$CHLFAkl)
  map <- sp::merge(gEAG, b[, c('CHLFAkl','CHLFa_ug_l_spectro','jaar','EAGIDENT')],
                   by.x = 'EAGIDENT', by.y =
                     'EAGIDENT', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  leaflet(map) %>%
    addPolygons(layerId = map$EAGIDENT, popup= paste("EAG naam", map$GAFNAAM, "<br>",
                                                     "Chlorofyl:", map$CHLFa_ug_l_spectro, "<br>",
                                                     "jaar:", map$jaar, "<br>"),
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(CHLFAkl), fillOpacity = 0.6) %>%
    addLegend("bottomright", colors=colc, labels=labelsc, title = "")%>%
    addTiles()
}
plotmafa <- function(mafa,TWNev){
  #mafa  <- hybi 
  mafa$watertype <- mafa$WATERTYPE
  mafa <- mafa[mafa$analysecode == 'MEA',] %>% as.data.table()
  mafa <- merge.data.frame(TWNev, mafa, by.x = 'taxonname', by.y ='TWN.naam', all.x = FALSE, all.y = TRUE) %>% as.data.table()
  # mafa <- dcast(mafa, taxongroup+EAGIDENT+datum+jaar~ ., value.var = "meetwaarde", fun.aggregate = sum, drop = TRUE)# som aantallen per groep
  mafa <- dcast(mafa, monsterident+watertype+KRW_SGBP3 +taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = "EAGIDENT", fun.aggregate = lengthUnique, drop = TRUE) # gemiddelde van locaties en jaren
  mafa<- mafa[!is.na(mafa$taxongroup) & !mafa$taxongroup == "" & !is.na(mafa$.),]
  #mafa <- dcast(mafa, taxongroup+jaar~ ., value.var = ".", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  mafa <-as.data.table(mafa)
  mafasel <- mafa[,length(unique(jaar))>3, by=KRW_SGBP3]
  # mafasel <- mafa[,length(unique(jaar))>3, by=EAGIDENT]
  # mafa <- mafa[ocatie.EAG %in% mafasel$EAGIDENT[mafasel$V1 == TRUE]]
  mafa <- mafa[KRW_SGBP3 %in% mafasel$KRW_SGBP3[mafasel$V1 == TRUE]]
  mafa <- mafa[mafa$watertype %in% c('M10','M27','M14','M20'),]
  mafa$meetwaarde <- mafa$.
  mafa <- dcast(mafa, watertype+taxongroup+jaar~ ., value.var = "meetwaarde", fun.aggregate = mean, drop = TRUE) # gemiddelde van locaties en jaren
  eptindex <- mafa[mafa$taxongroup %in% c("Insecta - Ephemeroptera","Insecta - Remaining","Insecta - Trichoptera"),]
  
  mafaplot <- ggplot(eptindex)+
    geom_bar(aes(x = jaar, y = ., fill = taxongroup), stat= "identity", position = "stack") +
    guides(fill= guide_legend(title='Groepen macrofauna', label.theme = element_text(size = 7), ncol = 2))+
    # facet_wrap(.~watertype, scales = 'fixed')+
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
ekrmapKRW <- function(gebiedData, gKRW, maatlat = "4VI1 Vis-kwaliteit" , col= col, labels=labels){
  # gebiedData <- EKRset[EKRset$jaar >= '2006' & EKRset$jaar < '2020' & EKRset$facet_wrap_code == "Vis",]
  gebiedData <- gebiedData[aggregated == 'ja',]
  gebiedData <- gebiedData[gebiedData$GHPR_level == maatlat,]
  # koppel identKRW aan gebieddata
  
  gebiedData$klasse = factor(gebiedData$klasse, levels = c("3", "4", "5", "6","7"))
  x <- as.character(sort(unique(gebiedData$jaar)))
  x <- paste0(x,' ', collapse = '')
  titel1 <- paste0("EKR scores in ",x)
  
  pal <- colorFactor(palette = col,  domain = gebiedData$klasse)
  map <- sp::merge(gKRW, gebiedData, by.x = 'OWMIDENT', by.y =
                     'KRW_SGBP3', all.x = TRUE, duplicateGeoms = T)
  
  map <- map[order(map$jaar),]
  
  leaflet(map) %>%
    addPolygons(layerId = map$KRW_SGBP3, popup= paste("Waterlichaam", map$waterlichaam, "<br>",
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
    addPolygons(data = gEAG, layerId = gEAG$EAGIDENT,
                stroke = T, color= 'grey' , fillColor = NA, opacity=0.8, weight = 1,
                smoothFactor = 0.8,
                fill=F, fillOpacity = 0.6)%>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    addLegend("bottomright", colors=mypal, labels=gKRW$OWMNAAM)
}
eagoverzicht <- function(gEAG, eag_wl){
  
  tabset1 <- merge(gEAG, eag_wl, by = 'EAGIDENT',  all.x = TRUE, duplicateGeoms = T)
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
    addPolygons(layerId = tabset1$EAGIDENT, popup= paste("naam", tabset1$EAGNAAM, "<br>",
                                                         "Ident:", tabset1$EAGIDENT,"<br>",
                                                         "watertype:", tabset1$watertype),
                stroke = T, color= ~pal(watertype) , fillColor = ~pal(watertype), opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillOpacity = 0.6) %>%
    addProviderTiles("Esri.WorldGrayCanvas")%>%
    addLegend("bottomright", pal=pal, values=tabset1$watertype)
}

# Additional visualisations stand van zaken report ------------------

# ekr per eag en mp
ppr_mapPpercWatb <- function(pvskp, gEAG,param = 'p_uitspoel'){
  
  if(param == 'p_retentie'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(-40,-30,-20,-10,-5,-1,0,1,2,5,10,20,30,40)))}else{
    if(param == 'A_P_AL'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0,10,15,20,25,30,35,40,45,50,60)))}else{
      if(param == 'a_tot'){pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0,50,150,200,300,400,500,1000,2000,3000,4000)))}else{
        pvskp[[param]] <- as.factor(cut(pvskp[[param]], breaks=c(0.1,0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.7,10)))}}}
  pal <- colorFactor(palette = topo.colors(14),  domain = pvskp[[param]])
  gEAG$gaf <- substr(gEAG$EAGIDENT,1,4)
  pvskp <- pvskp[!is.na(pvskp[[param]]),]
  
  # merge met EAG
  map <- sp::merge(gEAG, pvskp, by.x = 'gaf', by.y ='GAF', all.x = FALSE, duplicateGeoms = F)
  # merge met GAF
  map2 <- sp::merge(gEAG, pvskp, by.x = 'EAGIDENT', by.y ='EAG', all.x = FALSE, duplicateGeoms = F)
  
  leaflet(map) %>%
    addPolygons(data = map, layerId = map$EAGIDENT, popup= paste("GAF naam", map$GAFNAAM, "<br>", param, map[[param]]), 
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map[[param]]), fillOpacity = 0.6)    %>%
    addPolygons(data = map2, layerId = map2$EAGIDENT, popup= paste("EAG naam", map2$GAFNAAM, "<br>", param, map2[[param]]), 
                stroke = T, color= 'green', opacity=0.8, weight = 1, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map2[[param]]), fillOpacity = 0.6)    %>%
    addLegend("bottomright", pal, values=~map[[param]], title = param) %>%
    addTiles()
}

# waterdiepte per eag
ppr_dieptekaart<- function (hybi, gebieden = gEAG, background = gbrpAGV, kansrijk = TRUE, handelperspectief = TRUE){ # kaart mediane diepte per eag + slibdikte is handelingsperspectief
  
  b <- dcast(hybi,locatie+EAGIDENT+jaar ~ parameter, value.var = "meetwaarde", fun.aggregate = median, na.rm =TRUE, fill = NaN)
  b <- b[!is.na(b$WATDTE)&!is.na(b$SUBMSPTN),]
  b$kansloc <- 0
  b$SUBMSPTN <- b$SUBMSPTN - b$FLAB
  b$kansloc[(b$SUBMSPTN < 35|b$SUBMSPTN >75) & b$WATDTE < 0.35] <- 1 # alleen locaties waar de bedekking niet voldoet en waterdiepte te gering is
  b$handel <- 0
  b$handel[b$SLIBDTE + b$WATDTE > 0.35 & b$SLIBDTE > 0.05] <- 1
  c <- b[,c('EAGIDENT','locatie','jaar','handel','kansloc','WATDTE','SUBMSPTN','SLIBDTE')]
  
  if(handelperspectief){
    0 -> c$kansloc[c$handel == 0]
  }
  
  if(kansrijk){
    c <- c[,lapply(.SD, mean),.SDcols = c('handel','kansloc','WATDTE','SUBMSPTN','SLIBDTE'), by= c('EAGIDENT','jaar')]
    c <- c[order(c$kansloc),]
    c$fact <- cut(c$kansloc,  breaks = c('0','0.1','0.25','0.5','0.75','1'))
    c <- c[!is.na(c$fact),]
    col <- c('1'= 'lightyellow','2'="yellow", '3'="deepskyblue",'4'="blue",'5'="darkblue")
    labels <- c('1'="0-10%",'2'="10-25%" ,'3'="25-50%",'4'="50-75%",'5'="75-100%")
  }
  
  if(!kansrijk){
    c <- c[,lapply(.SD, median),.SDcols = c('kansloc','WATDTE','SUBMSPTN','SLIBDTE'), by= c('EAGIDENT','jaar')]
    c$fact <- cut(c$WATDTE,  breaks = c('0','0.1','0.2','0.3','0.35','0.4','0.5','1'))
    col <- c('1'="darkred",'2'="red", '3'="tomato",'4'="salmon",'5'="lightblue",'6'="deepskyblue", '7'= 'blue')
    labels <- c('1'="0-0.1",'2'="0.1-0.2" ,'3'="0.2-0.3",'4'="0.3-0.35",'5'="0.35-0.4",'6'="0.4-0.5",'7'="0.5-1")
  }
  
  
  
  # kaart waterdiepte
  map <- sp::merge(gebieden, c, by.x = 'EAGIDENT', by.y =
                     'EAGIDENT', all.x = FALSE, duplicateGeoms = T)
  pal <- colorFactor(palette = col,  domain = map$fact)
  
  map <- map[order(map$jaar),]
  
  leaflet() %>%
    addPolygons(data = gbrpAGV,
                stroke = T, color= 'green', opacity=0.05, weight = 0.2, smoothFactor = 0.8,
                fill=T, fillColor = 'green', fillOpacity = 0.3) %>%
    addPolygons(data = map, layerId = map$EAGIDENT, popup= paste("EAG naam", map$EAGNAAM, "<br>",
                                                                 "Diepte:", map$WATDTE, "<br>",
                                                                 "Slibdikte:", map$SLIBDTE, "<br>",
                                                                 "Percentage kansrijke locaties:", round(map$kansloc,2)*100, "<br>",
                                                                 "jaar:", map$jaar),
                stroke = FALSE, color= NULL, opacity=0.5, weight = 0.5, smoothFactor = 0.8,
                fill=T, fillColor = ~pal(map$fact), fillOpacity = 0.4) %>%
    
    addLegend("bottomright", colors=col, labels=labels, title = "")%>%
    addProviderTiles("Esri.WorldGrayCanvas")
  
}

# species count & indicator species
fractie_score_taxa_nsoort <- function(krw, pars = "OEVPTN", titel = "Aantal en kwaliteit van verschillende soorten waterplanten"){
  # select only non aggregated scores per sample point
  dt <- krw[aggregated == 'nee',] 
  # select relevant parameters (waarnemingssoort = grootheid, parameter, typering)
  # aanwezigheid = score per taxa based on abundance per taxa
  dt <- dt[GHPR %in% c("Aanwezigheid", "Soortenrijkdom Oeverplanten", "Soortenrijkdom Waterplanten", "Soortenrijkdom Macrofyten",
                       "Soortensamenstelling macrofyten Waterplanten","Soortensamenstelling macrofyten Oeverplanten", 'Soortensamenstelling macrofyten') ,] # score per taxa/bedekking
  # convert scores per taxa into quality judgement
  dt$klasse <- cut(dt$Numeriekewaarde, breaks = c(-9,0,3,6,9), labels = c('ongewenst','minder gewenst','gewenst','zeer gewenst'))
  # calculate richness taxa dived by count
  dt <- dt[,soortenrijkdom_tot := mean(Numeriekewaarde[Grootheid.code == "SOORTRDM" & Parameter.code %in% pars])/ length(Numeriekewaarde[GHPR == "Aanwezigheid" & Somparameter.code %in% pars]), by = jaar]
  # when no species are present
  if(!"Aanwezigheid" %in% unique(dt$GHPR)){
    dt[,soortenrijkdom_tot := 0]
    dt <- dt[Parameter.code %in% pars]}else{
      # filter data
      dt <- dt[GHPR %in% c("Aanwezigheid"), ]
      dt <- dt[Somparameter.code %in% pars,]
    }
  # format data for plot
  dt[,jaar_char := as.character(jaar)] 
  
  # plot
  ggplot(dt, aes(x = jaar_char, y = soortenrijkdom_tot, fill = klasse)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values= c("zeer gewenst"="#00AFBB","gewenst"="#52854C","minder gewenst"="#E7B800", "ongewenst"="#FC4E07"))+
    guides(fill=guide_legend(title='Kwaliteit soorten'))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 7), #waardebepmethode
      strip.text.y = element_text(size = 6), #level
      axis.text.x = element_text(size= 10), #jaar
      axis.text.y = element_text(size= 10),
      axis.ticks =  element_line(colour = "black"),
      panel.background = element_blank(),
      plot.background = element_blank(),
      legend.title = element_text(size = 11),
      legend.text  = element_text(size = 10),
      legend.key.size = unit(0.9, "lines"),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5))+
    ggtitle(paste0(titel)) +
    labs(x= "",y="")
  
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



