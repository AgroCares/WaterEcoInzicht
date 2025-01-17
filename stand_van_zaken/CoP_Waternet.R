# Maak figuur met aantal soorten per waardecategorie obv KRW toetsgegevens
#  Settings-------------------------------------------------------

# Load packages and functions-------------------------------------
library(data.table)
library(plotly)
source('./scripts/visualisations_wqdata.R')

# Load data ------------------------------------------------------
ekrlijst <- readRDS('./data/krw/ekrlijst.rds') %>% as.data.table()

# Make plot ------------------------------------------------------

# extract an EAG
i <- unique(ekrlijst$EAGNAAM)[1]
krw <- ekrlijst[EAGNAAM %in% i,]

# extract an waterbody
i <- unique(ekrlijst$KRW_SGBP3)[1]
krw <- ekrlijst[KRW_SGBP3 %in% i,]

# plots for lakes (watertypes M27, M14, M20) where oeverplanten are not part of the biodiversity macrophytes metric ('soortensamenstelling macrofyten')
if(!"OEVPTN" %in% unique(krw$Somparameter.code)){
  plot <- fractie_score_taxa_nsoort(krw, pars = c("WATPTN","MFT_srtscore", "MACFT"), titel = 'water- en oeverplanten')
  plot <- annotate_figure(plot, top = text_grob(paste0("Aantal en kwaliteit van verschillende soorten in ",i), face = "bold", size = 14))
  annotate_figure(plot, left = text_grob("gemiddeld aantal soorten per meetlocatie", rot = 90, size = 11))
}

if("OEVPTN" %in% unique(krw$Somparameter.code)){
  p1<- fractie_score_taxa_nsoort(krw, pars = "OEVPTN", titel = '(natte) oeverplanten')
  p2<- fractie_score_taxa_nsoort(krw, pars = c("WATPTN","MFT_srtscore", "MACFT"), titel = '(onder)waterplanten')
  plot<- ggarrange(p1, p2, ncol=2, common.legend = TRUE, legend="bottom")
  plot<- annotate_figure(plot, top = text_grob("Aantal en kwaliteit van verschillende soorten", face = "bold", size = 14))
  annotate_figure(plot, left = text_grob("gemiddeld aantal soorten per meetlocatie", rot = 90, size = 11))
  ggplot2::ggsave(plot,file=paste0('output/',i,'soortenWaterOeverplanten.png'),
                  width = 14, height = 10,units='cm',dpi=1000)
} 


 