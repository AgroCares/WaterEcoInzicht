# postprocess functions to combine output Aquo-kit and aggregate data
# Laura Moria - juni 2023

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
  plot <- ggplot(dt, aes(x = id, y = EKR)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1,
                     y = EKR, yend = EKR, linetype = "Huidige toestand"),
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
  plot <- ggplot(dt, aes(x = id, y = EKR)) +
    geom_rect(data = bg_spr, inherit.aes = FALSE,
              aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax,
                  fill = Oordeel), alpha = 0.3) +
    scale_fill_manual(values = myColors) +
    geom_segment(aes(x = 0, xend = 1,
                     y = EKR, yend = EKR, linetype = "Huidige toestand"),
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

plotFractiePerMaatlatFacetEAG <- function(l){
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
    facet_grid(jaar~facet_wrap_code+EAGIDENT)+
    labs(x="",y="")
}

#fractieplot
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



  

          