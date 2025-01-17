# Note: CalculateIR ratio werkt met data in format met iig headers:
# "Datum", "Meetpunt.ID", "EAGIDENT", "EAGNAAM", "fewsparameternaam", "meetwaarde", "Eenheid"
# Parametercodes zijn: paramCa="Ca_mg/l_nf", paramCl="Cl_mg/l", param="GELDHD_mS/m_25oC"

convertDatumFEWS <- function (myData){
  myData$datum <- base::as.Date(myData$datum, format = "%Y-%m-%d %H:%M")
  myData$jaar <- base::format(myData$datum, '%Y')
  myData$jaar <- base::as.numeric(myData$jaar)
  return(myData)
}

calculateIRRatio <- function(myData) {
  ### slecteer de correcte parameters uit bestand
  funcData.CA <- myData[myData$fewsparameter=='Ca_mg/l',]
  funcData.CL <- myData[myData$fewsparameter=='Cl_mg/l',]
  ### converteer calcium en chloride naar correcte eenheid
  funcData.CA$meetwaarde <- funcData.CA$meetwaarde/(40.078/2) # Eenheid is nu meq/l
  funcData.CL$meetwaarde <- funcData.CL$meetwaarde/(35.45/1)  # Eenheid is nu meq/l
  funcData.IR <- merge.data.table(funcData.CA, funcData.CL, by = c("locatiecode", "datum"), all = FALSE)# Eenheid is geen mg/l maar meq/l
  ### bereken IR 
  funcData.IR$meetwaarde <- funcData.IR$meetwaarde.x / (funcData.IR$meetwaarde.x + funcData.IR$meetwaarde.y)
  funcData.IR$fewsparameter <- "IR"
  funcData.IR$eenheid <- "DIMSLS"
  funcData.IR$locatie.EAG <- funcData.IR$locatie.EAG.x
  
  return(funcData.IR[,c("datum", "locatiecode", "fewsparameter", "meetwaarde", "eenheid", "locatie.EAG")])
}

createIR_EGV_Graph <- function(theData, LATframework, referencepoints) {
  
  # calculate correct values for IR and EGV met functies hierboven
  IR <- calculateIRRatio(theData)
  EGV <- theData[theData$fewsparameter == 'GELDHD_mS/m_25oC',]
  
  # Combine values on Date, Point and area
  data <- merge.data.table(EGV, IR, by=c("datum", "locatiecode", "locatie.EAG"))
  
  ggplot2::ggplot()+
    geom_point(data = data, aes(x= meetwaarde.x*10, y = meetwaarde.y, col = locatiecode))+
    geom_path(data = LATframework, aes(x= EC25*10, y = IR/100), size = 0.1, linetype = 'dotdash')+
    geom_text(data = referencepoints, aes(x = EC25*10, y = IR/100, label = Name))+
    scale_x_log10()+
    guides(col=guide_legend(title='Locatiecode'))+
    theme_minimal()+
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10), 
      strip.text.y = element_text(size = 10), 
      axis.text.x = element_text(size= 10, angle=90,hjust=1),
      axis.text.y = element_text(size= 10, hjust=2),
      axis.ticks =  element_line(colour = "black"), 
      panel.background = element_blank(), 
      #panel.border =element_blank(), 
      #panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(), 
      #panel.margin = unit(0.20, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(0.25,0.25, 0.5, 0.5), "lines")
    )+
    ggtitle("IR EGV diagram") +
    labs(x="EGV (uS/cm)",y="IR-Ratio (Ca/(Ca+Cl))") 
}