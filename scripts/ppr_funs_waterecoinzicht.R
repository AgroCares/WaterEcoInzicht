# Laura Moria - juni 2023
# locatie
# watertype
# EAGIDENT

#postprocessing data ----------------
ppr_wqdata <- function(fychem, syear = 1980, srow = c("IONEN","NUTRI","ALG","VELD","ALGEN","LICHT","MONSVAR")){
  
  # make local copies
  db <- copy(fychem)
  
  # select relevant data, remove double rows Afronding = “Ja”
  db <- db[afronding == 'Ja',]
  
  # adapt wq database
  db[,datum := as.Date(datum, format = "%Y-%m-%d %H:%M")]
  db[,jaar := year(datum)]
  db[,maand := month(datum)]
  db[limietsymbool == '<',meetwaarde := meetwaarde * 0.5]
  
  # delete years before 2000
  db <- db[jaar>=syear,]
  
  # subset waterquality data
  db <- db[categorie %in% srow,]
  
  # adjust fews parameter names to avoid complicated column names
  db[,parameterid := gsub("/","_",parameterid)]
  db[,eenheid := gsub("/","_",eenheid)]
  
  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]
  
  # return output wq parameters
  return(db)
}

# pre process EKR information by adding names for filtering
ppr_ekr <- function(db, doelen, sompar = sompar){
  
  # combine both EKR from KRW and overig water into one data.table
  db$jaar <- as.numeric(db$jaar) 
  
  # delte rows without information, rijen weg zonder informatie
  db <- db[!is.na(db$Numeriekewaarde),]
  
  # remove columns without information
  cols <- colnames(db)[unlist(db[,lapply(.SD,function(x) sum(is.na(x))==nrow(db))])]
  db[,c(cols):= NULL]
  
  # trim spaces
  db[,GHPR := gsub(' $','',GHPR)]
  
  # mean GEP per id  
  doelgeb <- doelen[,.(GEP_2022 = mean(Doel_2022,na.rm=TRUE)), by =.(GeoObject.code, gebied, brondoel_2022, GHPR, Waardebepalingsmethode)]
  doelgeb <- doelgeb[, waterlichaam_code := as.character(fifelse(!(is.na(GeoObject.code)|GeoObject.code == ""), GeoObject.code, gebied))]
  
  # merge with doelen & db
  db[,waterlichaam_code := as.character(waterlichaam_code)]
  db[,facet_wrap_code := as.character(facet_wrap_code)]
  db <- merge(db, doelgeb[,c('waterlichaam_code','Waardebepalingsmethode','GEP_2022')], by.x = c('waterlichaam_code','facet_wrap_code'), by.y = c('waterlichaam_code','Waardebepalingsmethode'), all.x = TRUE)
  
  # merge with additional information on taxa/ parameters 
  db <- merge(db, sompar, by.x = c('Biotaxon.naam','KRWwatertype.code'), by.y = c('Deelparameter.omschrijving','KRWwatertype.code'), all.x =TRUE)
  db[, somparname := Somparameter.omschrijving]
  db[, somparname := gsub("Macrofyten - scorende soorten","waterplanten",somparname)]
  db[, somparname := gsub("Waterplanten","waterplanten",somparname)]
  db[, somparname := gsub("Oeverplanten","(natte) oeverplanten", somparname)]
  
  return(db)
}

# adapt water balance properties
ppr_wbalfiles <- function(dat, EAG = EAG){
  
  # this function select the relevant file names of the excel waterbalances.
  db <- copy(dat)
 
  # remove all inconsequent named balances, scenario balances and .xls balances
  db[,versie := sapply(strsplit(pol, '_'), `[`, 2)]
  db <- db[grepl('^F', versie),]
  db <- db[,versie := gsub('*.xlsx','',versie)]
  remove.versie <- paste(c('a','b','c','S','P','V','W'), collapse ='|')
  db <- db[!grepl(remove.versie, versie, ignore.case = TRUE),]
  
  # create GAF, EAG, version
  db[,gebied := sapply(strsplit(pol, '_'), `[`, 1)]
  db[grepl('EAG', gebied), EAGIDENT :=  gebied]
  db[grepl('GAF', gebied), GAFIDENT :=  gebied]
  db[, GAFIDENT :=  sapply(strsplit(GAFIDENT, '-'), `[`, 1)]
  db <- db[!is.na(EAGIDENT)|!is.na(GAFIDENT),]
  
  # filter only latest version
  setorder(db,-versie)
  db[, versieid := seq_len(.N), by = c('gebied', 'jaar','maand')]
  db <- db[versieid == 1,]
  
  # create a matching table 4 EAGS
  eags <- data.table::tstrsplit(db$EAGIDENT, split=c('-'), fixed=TRUE)
  eags <- as.data.table(eags)
  eags <- cbind(db$EAGIDENT,eags);colnames(eags)[1:3] <- c('eag_balans','gaf','eag')
  eags <- eags[!is.na(eag_balans)]
  eags <- eags[,c('V3','V4','V5','V6','V7','V8') := list(paste0(gaf,'-EAG-',V3),paste0(gaf,'-EAG-',V4),paste0(gaf,'-EAG-',V5),paste0(gaf,'-EAG-',V6),paste0(gaf,'-EAG-',V7),paste0(gaf,'-EAG-',V8))]
  eags <- unique(eags)
  eags <- melt(eags, id.vars = c('eag_balans','gaf','eag'), measure.vars = c('V3','V4','V5','V6','V7','V8'))
  eags <- eags[!grepl('NA',value),]
  setorder(eags,variable)
  eags[, versieid := seq_len(.N), by = c('value')]
  eags <- eags[versieid == 1,]
  eags <- eags[,c('eag_balans','value')]; setnames(eags, c('eag_balans','value'),c('eag_balans','EAGIDENT'))
  
  # match tabel with data
  db <- merge(db, eags, by.x = 'EAGIDENT', by.y = 'eag_balans', all = TRUE, allow.cartesian = TRUE)
  db[,EAGIDENT := EAGIDENT.y]; NULL -> db$EAGIDENT.y
  
  # check if files missing given the most recent EAG shape (so those EAGs do not have a balance)
  # check missing files in EAG shape
  eag.sf <- as.data.table(EAG)
  bal_mis <- unique(eag.sf[!(EAGIDENT %in% eags$EAGIDENT | GAFIDENT %in% unique(db$GAFIDENT)), EAGIDENT])
  bal_mis <- as.character(bal_mis)
  
  # print warning 2
  if(length(bal_mis)>1){
    print(paste0('warning: the waterbalans is missing for ',length(bal_mis),' eags, as given in shape EAG'))}
  
  return(db)

}
ppr_pmaps <- function(dat, Overzicht_kp, hybi, nomogram, eag_bt, gaf_bt, jaar = 2006:2024, pYEAR = FALSE){
  
  # make local copy of soil and waterbalance data
  d1 <- copy(dat)
  # koppel data soiltype 78923
  setDT(eag_bt);setDT(gaf_bt)
  d1a <- merge(d1, eag_bt[,c('EAGIDENT','bd50.hoofd','watertype')], by = 'EAGIDENT')
  d1b <- merge(d1, gaf_bt[,c('GAFIDENT','bd50.hoofd','watertype')], by = 'GAFIDENT')
  d1 <- rbind(d1a, d1b, use.names=TRUE)
  d1[bd50.hoofd %in% c("veengronden","moerige_gronden"),bodem := "veen"]
  d1[bd50.hoofd %in% c("dikke_eerdgronden","zandgronden","humuspodzolgronden"),bodem := "zand"]
  d1[bd50.hoofd %in% c("zeekleigronden","rivierkleigrond" ),bodem := "klei"]
  d1[is.na(bodem) & watertype %in% c('M8','M10','M27','M25'), bodem := "veen"]
  d1[is.na(bodem) & watertype %in% c("M3", "M1a","M30","M14","M11","M6a","M7b","M6b","M7a"), bodem:='klei']
  d1[is.na(bodem) & watertype %in% c("M20"), bodem:='zand']
 
  # filter only recent years
  dg <- d1[jaar %in% jaar,]
  
  # addgroup and estimate meerjarig mean for numeric values
  colsg <- colnames(dg)[grepl('^a_in|^a_uit|^EAGIDENT|^GAFIDENT|^KRW$|watertype|^bodem$|^pol|^EAGNAAM|GAFNAAM',colnames(dg))]
  if(pYEAR){colsg <- c(colsg,'jaar')}
  colss <- colnames(dg)[grepl('^a_|^wp_|jaar|maand|^w_|^p_|ret|lastyear',colnames(dg))]
  colss <- colss[!colss %in% colsg]
  dg <- dg[,lapply(.SD,mean,na.rm=TRUE),.SDcols=colss,by=colsg]
  
  # add total sum of P load
  dg[,wp_tot_sum := wp_min_sum + wp_inc_sum]
  
  # mean water depth per EAG
  mdPtb <- hybi[jaar > 2018 & parameterid == 'WATDTE_m']
  mdPtb <- mdPtb[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)), by='EAGIDENT']
 
  # mean water depth per GAF
  mdPtbG <- hybi[jaar > 2018 & parameterid == 'WATDTE_m']
  mdPtbG <- mdPtbG[,.(meetwaarde = median(meetwaarde,na.rm = TRUE)),by='GAFIDENT']
  
  # merge met kP ----------------------------------------------------------
  
  # koppel waterdiepte per eag en afvoergebied aan water en stoffenbalans
  dgwatdte  <- merge.data.table(dg[is.na(GAFIDENT),], mdPtb, by = 'EAGIDENT', all.x = T)
  dgwatdteG <- merge.data.table(dg[is.na(EAGIDENT),], mdPtbG, by = 'GAFIDENT', all.x = T)
  dgwatdte <- rbind(dgwatdte,dgwatdteG,fill = TRUE)  # mis 1 balans
  dgwatdte[is.na(meetwaarde), meetwaarde := a_slootdiepte]
  
  # update merged table
  dgwatdte[,watdteF := cut(meetwaarde, breaks = c('0','0.3','0.5','0.7','7.0'))]
  dgwatdte[meetwaarde > 0.7, watdteF := '(0.5,0.7]']
  dgwatdte[,debiet := cut(w_debiet, breaks = c('0','7','17','29','36','50','60','70','80','90','100','200','300','400'))]
  dgwatdte[w_debiet > 400, debiet := '(300,400]']
  
  # retrieve kP from meta-model PCditch ----
  
  # make local copy and simplify 
  dbnomogram <- copy(nomogram)
  setnames(dbnomogram,c("debiet (mm/dag)","bodemtype"),c("debiet","bodem"),skip_absent=TRUE)
  # add depth category, similar to dbhybi dataset
  dbnomogram[,watdteF := cut(watdte_m, breaks = c('0','0.3','0.5','0.7','7.0'))]
  dbnomogram[,debiet := cut(debiet, breaks = c('0','7','17','29','36','50','60','70','80','90','100','200','300','400'))]
  dbnomogram <- dbnomogram[,.(kP = mean(kP,na.rm = TRUE)),by=c('bodem','watdteF','debiet')]
  
  # couple nomogram
  dgwatdte <- merge(dgwatdte, dbnomogram, by = c('bodem','watdteF','debiet'), all.x = TRUE)
  
  # calc critical P-concentration
  dgwatdte[,PvskPDitch := wp_min_sum / kP]
  
  # koppel kp plassen obv invoertabel per EAG ----
  
  # make local copu
  kP_plas <- copy(Overzicht_kP)
  
  # relevant columns to be merged
  cols <- colnames(kP_plas)[grepl('^pc_|^lake|^p_bel|^EAG|^GAF$',colnames(kP_plas))]
  
  # merge per EAG and per GAF, and combine both (assuming its either EAG or GAF)
  pvskp <- merge.data.table(dgwatdte[watertype %in% c('M20','M27','M25',"M14",'M11') & !is.na(EAGIDENT), !c('GAFIDENT')],
                                 kP_plas[,mget(cols)], by.x='EAGIDENT', by.y='EAG',all.y = TRUE, all.x = FALSE)
  
  # merge plas kP with original water balance db
  dgwatdte <- merge.data.table(dgwatdte, pvskp[,c('pol','pc_troebel_helder', 'p_bel_year',
                                                  'pc_helder_troebel', 'lake_ditch_vol')], by = c('pol'), all.x = TRUE)
  
  # calc PvskP for lakes
  dgwatdte[!is.na(p_bel_year),wp_min_sum := p_bel_year]
  dgwatdte[,PvskPlake := wp_min_sum / pc_helder_troebel]
  dgwatdte[is.na(lake_ditch_vol),lake_ditch_vol := 'pc_ditch']
  dgwatdte[lake_ditch_vol %in% c('lake','lake; empirisch'),lake_ditch_vol := 'pc_lake']
  
  
  # remove rows without estimated P-belasting
  dgwatdte <- dgwatdte[!is.na(dgwatdte$pol) & !is.na(dgwatdte$wp_min_sum ),] 
  
  return(dgwatdte)
  
}

# calc/ aggregate data functions -------------------------
# calculate mean ekr score per year, calc mean score last 3 measured years, calculate oordeel (score compared 2 goals) & calculate reference score 
tabelEKRPerWLEnEAGPerJaar <- function (EKRset, detail = "deel", minjaar = 2013, outdir = "../output"){
  
  
  # make local copy (only within this function)
  db <- EKRset[EKRset$jaar > 2005, ]
  
  if(detail == "hoofd"){
    db <-  db[db$level == 1,]}
  
  # columns to group with year
  colgroup <- c('EAGIDENT','KRWwatertype.code','facet_wrap_code','GHPR_level',
                'GHPR','level','jaar','GEP_2022','waterlichaam','KRW_SGBP3','aggregated')
  colg <- colgroup[!colgroup %in% c("jaar")]  
  db <- db[,lastyear := max(jaar), by = c(colg)]
  
  # gemiddelde ekr per jaar voor draaitabel en selectie laatste 3 meetjaren
  d1 <- db[,.(EKRmean = mean(Numeriekewaarde,na.rm=T), EKRmedian = quantile(Numeriekewaarde,probs = c(0.50), na.rm=T), EKRperc90 = quantile(Numeriekewaarde,probs = c(0.90), na.rm=T), EKRperc95 = quantile(Numeriekewaarde,probs = c(0.95), na.rm=T)), by = c(colgroup,"lastyear")]
  d1$hdlprsp <- d1$EKRperc90 -d1$EKRmedian
  d1[ ,hdlprsp.mean.per.year := mean(hdlprsp), by = colg]
  d1[ ,EKRperc90.mean.per.year := mean(EKRperc90), by = colg]
  d1[ ,EKRmedian.mean.per.year := mean(EKRmedian), by = colg]
  setorder(d1,EAGIDENT,KRWwatertype.code,GHPR_level,-jaar)
  
  # add year number (given ordered set), and take only three most recent years
  d1 <- d1[jaar > minjaar, yearid := seq_len(.N), by = colg]
  # calculate mean EKR per group over the three years = krw score formeel die wordt vergeleken met doel
  d1 <- d1[, EKR3jr := mean(EKRmean[yearid < 4], na.rm=T), by = colg]
  # add year number (given ordered set), and take only three first years
  setorder(d1,EAGIDENT,KRWwatertype.code,GHPR_level,jaar)
  d1$yearid <- NULL
  d1 <- d1[jaar < 2014, yearid := seq_len(.N),by = colg]
  # calculate mean EKR per group over the three years = krw score formeel die wordt vergeleken met doel
  d1 <- d1[,EKRref := mean(EKRmean[yearid < 4],na.rm=T),by = colg]
  # draaitabel voor wide format per jaar
  d1 <- dcast(d1, EAGIDENT+waterlichaam+KRW_SGBP3+KRWwatertype.code+GHPR_level+aggregated+EKRperc90.mean.per.year+EKRmedian.mean.per.year+hdlprsp.mean.per.year+EKRref+EKR3jr ~ jaar, value.var = c("EKRmean"), fun.aggregate = mean)
  
  # percentielen van alle meetlocaties (zowel meetpunt als geaggr per eag of waterlichaam) in alle jaren per gebied, best sites
  d2 <- db[,.(EKRmean.all.year = mean(Numeriekewaarde,na.rm=T), EKRmedian.all.year = quantile(Numeriekewaarde,probs = c(0.50), na.rm=T), EKRperc90.all.year = quantile(Numeriekewaarde,probs = c(0.90), na.rm=T), EKRperc95.all.year = quantile(Numeriekewaarde,probs = c(0.95), na.rm=T)), by = colg]
  d2$hdlprsp.mean.all.year <- d2$EKRperc90.all.year -d2$EKRmedian.all.year
  
  # merge per jaar en percentielen over alle jaren (die niet worden gebruikt voor handelingsperspectief)
  d3 <- merge(d1, d2, by = c('EAGIDENT','waterlichaam','KRW_SGBP3','KRWwatertype.code','GHPR_level','aggregated'))
  
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
  d3 <- d3[!is.na(waterlichaam) & level == 1, minscore := oordeelsort==min(oordeelsort,na.rm=T), by = colg]
  # zoek laagste score per (deel)maatlat waarbij oever en drijfblad niet meedoen 
  d3 <- d3[facet_wrap_code == "Ov. waterflora" &
                             level == 3 & !(GHPR %in% c('Bedekking Grote drijfbladplanten','Bedekking Kruidlaag')), minscore := EKR3jr==min(EKR3jr,na.rm=T), by = colg] 
  
  write.table(d3, file = paste(outdir,"/EKROordeelPerGebiedJaarWide",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(d3)
}
# calculate trends EKR scores over time 
trendkrw <- function(EKRset){ 
  
  EKRset$jaarlm <- as.numeric(EKRset$jaar-2006)
  # add last year when measures were executed
  # columns to group alle meetpunten over de jaren
  colg <- c('EAGIDENT','waterlichaam_code','KRW_SGBP3','aggregated','KRWwatertype.code','GHPR','GHPR_level','level','facet_wrap_code', 'GEP_2022', 'SGBP3_NAAM')
   
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
  
  # 2do check4  normality of residuals
  res <- setDT(EKRset)[,residuals(lm(Numeriekewaarde ~ jaarlm, na.action = NULL)), by = colg]
 
  return(tb)
}
# description trend ekr scores
beschrijvingtrend <- function(ekrtrendeag){
  ekrtrendeag <- ekrtrendeag[aggregated == "nee"]
  
  # hier tekst maken beschrijving trend, deze in ppr ESF oordeel toevoegen (maar dan is ekrtrend wel nodig)
  ekrtrendeag$toelichting <-
    ifelse(ekrtrendeag$estimate < 0 & ekrtrendeag$r.squared < 1, paste0("De score op de maatlat ", ekrtrendeag$facet_wrap_code, " neemt af (", ekrtrendeag$estimate, " ekr per planperiode van 6 jaar tussen 2006 en ", ekrtrendeag$lastyear, ")."),
           ifelse(ekrtrendeag$estimate < 0 & ekrtrendeag$r.squared == 1, paste0("De score op de maatlat ", ekrtrendeag$facet_wrap_code, " neemt af (", ekrtrendeag$estimate, " ekr per planperiode tussen 2006 en ", ekrtrendeag$lastyear, "). Deze trend is gebaseerd op twee meetjaren."),
                  ifelse(ekrtrendeag$estimate > 0 & ekrtrendeag$r.squared < 1, paste0("De score op de maatlat ", ekrtrendeag$facet_wrap_code, " neemt toe (", ekrtrendeag$estimate, " ekr per planperiode tussen 2006 en ", ekrtrendeag$lastyear, ")."),
                         ifelse(ekrtrendeag$estimate < 0 & ekrtrendeag$r.squared == 1, paste0("De score op de maatlat ", ekrtrendeag$facet_wrap_code, " neemt toe (", ekrtrendeag$estimate, " ekr per planperiode tussen 2006 en ", ekrtrendeag$lastyear, "). Deze trend is gebaseerd op twee meetjaren."), paste0("De score op de maatlat ", ekrtrendeag$facet_wrap_code, " blijft gelijk.")
                         ))))
  # hier dcast om tabel met header kwaliteitslementen te maken
  ekr_scores_trendbesc <- dcast.data.table(setDT(ekrtrendeag), EAGIDENT+KRW_SGBP3 ~ facet_wrap_code, value.var = c("toelichting"))
  
  # remove NA
  ekr_scores_trendbesc[is.na(ekr_scores_trendbesc)] <- ""
  # merge tekst
  ekr_scores_trendbesc$mergetekst <- paste0("In ", ekr_scores_trendbesc$EAGIDENT, ": ", ekr_scores_trendbesc$Fytoplankton, " ", ekr_scores_trendbesc$`Ov. waterflora`," ",ekr_scores_trendbesc$Macrofauna," ", ekr_scores_trendbesc$Vis )
  # trim character columns from starting and ending space
  cols <- colnames(ekr_scores_trendbesc)[sapply(ekr_scores_trendbesc, is.character)] # which colnames are character
  ekr_scores_trendbesc[,(cols) := lapply(.SD, function(x) gsub("^\\s+|\\s+$", "", x)),.SDcols = cols]
  
  return(ekr_scores_trendbesc)
}
# create data for dashboard schoon water AGV
createDashboarddata <- function(ekrlijst, minjaar = 2006, trendjaar = 2019, outdir = outdir){
  # make local copy (only within this function)
  db <- ekrlijst[ekrlijst$jaar >= minjaar, ]
  # select only top level metrics (hoofdmaatlatten)
  db <-  db[db$level == 1,]
  # select only non-aggregated data (for KRW waterbodies aggregated data is not present per subaerea/ EAG)
  db <- db[db$aggregated == 'nee',]
  #add year index for lineair model
  db$jaarlm <- as.numeric(db$jaar-2006)
 
  # columns to group mean ekr with year
  colgroup <- c('EAGIDENT','KRW_SGBP3','Typering.code','KRWwatertype.code','jaar','GEP_2022')
  # columns to group ekr trend with and select last 3 measured years
  colg <- colgroup[!colgroup %in% c("jaar")]  
  
  # add last year in which an subarea is measured per measuretype/ parameter
  db <- db[,lastyear := max(jaar), by = c(colg)]
  # add n unique measured years
  db[,aantalmeetjaar := uniqueN(jaar), by = colg]
  
  # calculate trend EKR per metric and EAG
  setDT(db)[, c('intc', 'slope') := as.list(coef(lm(Numeriekewaarde ~ jaarlm, na.action = NULL))), by = colg]
  tb <- setDT(db)[aantalmeetjaar>2, as.list(broom::glance(lm(Numeriekewaarde ~ jaarlm, na.action = NULL))), by = c(colg , 'lastyear','intc', 'slope')]
  # adjust slope to calculate trend in 6 years (planperiode)
  tb[,slope := tb$slope*6]
  # filter significance value (<0.1 = weak evidence or a trend) en relevant trends
  tb[p.value > 0.1 &  lastyear <= trendjaar, slope := NaN ]
  tb[,slope := round(slope, digits = 2)]
  
  # 2do check4  normality of residuals
  # res <- setDT(db)[,residuals(lm(Numeriekewaarde ~ jaarlm, na.action = NULL)), by = colg]
  
  # gemiddelde ekr per jaar voor draaitabel en selectie laatste 3 meetjaren
  db <- db[,.(EKRmean = mean(Numeriekewaarde,na.rm=T)), by = c(colgroup,"lastyear","aantalmeetjaar")]
  setorder(db,EAGIDENT,KRWwatertype.code,Typering.code,-jaar)
  # add year number (given ordered set), and take only three most recent years
  db <- db[jaar > minjaar, yearid := seq_len(.N), by = colg]
  # calculate mean EKR per group over the three years = krw score formeel die wordt vergeleken met doel
  db <- db[, .(TOESDNUMERIEKEWAARDE = mean(EKRmean[yearid < 4], na.rm=T)), by = colg]
  
  # add classification oordeel for GEP 2022 (goals WBP3)
  db[TOESDNUMERIEKEWAARDE < GEP_2022/3, c('OORDLALFANUMERIEKEWAARDE','OORDLNUMERIEKEWAARDE') := list('slecht',7)]
  db[TOESDNUMERIEKEWAARDE >= GEP_2022/3 & TOESDNUMERIEKEWAARDE < 2 * GEP_2022 / 3, c('OORDLALFANUMERIEKEWAARDE','OORDLNUMERIEKEWAARDE') := list('ontoereikend',6)]
  db[TOESDNUMERIEKEWAARDE >= 2 * GEP_2022 / 3,c('OORDLALFANUMERIEKEWAARDE','OORDLNUMERIEKEWAARDE') := list('matig',5)]
  db[TOESDNUMERIEKEWAARDE >= GEP_2022, c('OORDLALFANUMERIEKEWAARDE','OORDLNUMERIEKEWAARDE') := list('goed',4)]
  db[,DOELGTNUMERIEKEWAARDE := ifelse(GEP_2022 - TOESDNUMERIEKEWAARDE < 0, 0, GEP_2022 - TOESDNUMERIEKEWAARDE)]
  
  # merge trend and mean ekr scores, oordeel
  db <- merge(db, tb, by = colg, all.x = TRUE)
  setnames(db, c('KRW_SGBP3','Typering.code','slope'),c('GEOOBJECTCODE','PARAMETERTYPERINGCODE','TRENDNUMERIEKEWAARDE'))
  db <- db[,c("EAGIDENT","PARAMETERTYPERINGCODE","GEOOBJECTCODE","TOESDNUMERIEKEWAARDE","OORDLALFANUMERIEKEWAARDE","OORDLNUMERIEKEWAARDE","TRENDNUMERIEKEWAARDE","DOELGTNUMERIEKEWAARDE")]
  
  write.table(db, file = paste(outdir,"/dashboardSchoonWaterData",format(Sys.time(),"%Y%m%d%H%M"),".csv", sep= ""), quote = FALSE, na = "", sep =';', row.names = FALSE)
  return(db)
}

# calculate mean wq data
calcMeanWq <- function(wq.sel = wq.sel, nyears = 3, smonth = 4:9, pEAG = TRUE, pYEAR = TRUE, pSEASON = TRUE){
  
  # make local copy
  b = copy(wq.sel) 
  
  # add dynamic grouping variable depening on function input
  groups <- c('parameterid','compartiment','EAGIDENT','watertype')
  if(pSEASON){
    b[,season := fifelse(maand %in% smonth,'summer','winter')]
    groups <- c(groups,'season')}
  
  # add year number and take only nyears most recent years (selection per EAG)
  b <- b[,yearid := frank(-jaar,ties.method = 'dense'), by = groups][yearid <= nyears]
  
  # calculate median value per loc, EAG, watertype and year
  cols <- colnames(b)[sapply(b, is.numeric)]
  # calculate median value per location and year
  if(!pEAG){
    b <- b[,lapply(.SD, mean),.SDcols = cols[!cols =='jaar'],by= c(groups,'locatie','jaar')]}
  # calculate median value per location nyear average
  if(!pYEAR & !pEAG){b <- b[,lapply(.SD, mean),.SDcols = cols,by= c(groups,'locatie')]}
  # calculate gemiddelde EKR for each EAG en jaar
  if(pYEAR & pEAG){b <- b[,as.list(unlist(lapply(.SD, function(x) list(mean= mean(x, na.rm = TRUE),median=median(x, na.rm = TRUE),perc= quantile(x,0.8, na.rm = TRUE))))),.SDcols = 'meetwaarde',by= c(groups, 'jaar')]}
  if(!pYEAR & pEAG){b <- b[,as.list(unlist(lapply(.SD, function(x) list(mean= mean(x, na.rm = TRUE),median=median(x, na.rm = TRUE),perc= quantile(x,0.8, na.rm = TRUE))))),.SDcols = 'meetwaarde',by= c(groups)]}
  
  # return database
  return(b)
  
}

# calculate trend wq data
trend_fychem <- function(fychem_vast, filter = "Ntot", grens1 =0.25, grens2 =0.15 ){
  dt <- fychem_vast[parameter %in% filter,]
  dt.trend <- NULL
  
  for(i in unique(dt$locatie)){
    ts <- dt[locatie == i,]
    if(nrow(ts) > 3){
      setorder(ts,datum)
      # ts <- ts(data = ts$meetwaarde, 
      #          start = min(ts$jaar),
      #          frequency = 12)
      # plot(ts)
      # decomp <- stl(ts, s.window = 15)
      # decomp <- decompose(ts)
      # plot(decomp)
      trend <- cor.test(ts$meetwaarde,ts$jaar, method="kendall")
      trend$trend[trend$p.value < 0.05 & trend$estimate >grens1] <- paste0(filter, " neemt toe in de tijd op meetlocatie ", i, ".")
      trend$trend[trend$p.value < 0.05 & trend$estimate >grens2 & trend$estimate <=grens1] <- paste0(filter, " neemt licht toe in de tijd op meetlocatie ", i, ".")
      trend$trend[trend$p.value < 0.05 & trend$estimate >-grens2 & trend$estimate <=grens2] <- paste0(filter, " blijft gelijk in de tijd op meetlocatie ", i, ".")
      trend$trend[trend$p.value < 0.05 & trend$estimate >-grens1 & trend$estimate<=-grens2] <- paste0(filter, " neemt licht af in de tijd op meetlocatie ", i, ".")
      trend$trend[trend$p.value < 0.05 & trend$estimate <=-grens1] <- paste0(filter, " neemt af in de tijd op meetlocatie ", i, ".")
      trend$trend[trend$p.value > 0.05 ] <- paste0(filter, " blijft gelijk in de tijd op meetlocatie ", i, ".")
      dt.add.trend <- data.table(i, unique(ts$XCOORD), unique(ts$YCOORD),unique(ts$EAGIDENT), trend$trend, trend$p.value, trend$estimate)
    }else{dt.add.trend <- data.table(i, unique(ts$XCOORD), unique(ts$YCOORD),unique(ts$EAGIDENT), paste0("Voor ",filter, " kan er geen trend bepaald worden op meetlocatie ",i, "."),1,0)}
    
    dt.trend <- rbind(dt.trend, dt.add.trend)
    
  }
  setnames(dt.trend, c("i","V2","V3","V4","V5", "V6","V7"),c("locatie","XCOORD","YCOORD","EAGIDENT","trend_beschr","pvalue","slope"))
  return(dt.trend)
}

# calc nalevering bodchemie
calc_watbod <- function(bodchem){
  
  # dcast slootbodem
  selb <- dcast.data.table(bodchem, EAGIDENT+locatie+datum+jaar ~ parameterid+compartiment, value.var = "meetwaarde", fun.aggregate = mean, fill= "")

  # calculate relevant ratios
  selb[,FeSP_DW := (Fe_mg_kg_dg_SB/55.845 - Stot_mgS_kg_dg_SB/32.065)/(Ptot_gP_kg_dg_SB*1000/30.974)]# deze zit in baggernut
  selb[,FeP_DW := (Fe_mg_kg_dg_SB/55.845)/(Ptot_gP_kg_dg_SB*1000/30.974)]
  selb[,FeS_DW := (Fe_mg_kg_dg_SB/55.845)/(Stot_mgS_kg_dg_SB/32.065)] # deze is meest relevant
  selb[,FeSP_FW := (Fe_mg_l_ng_SB/55.845 - Stot_mgS_l_ng_SB/32.065)/(Ptot_mgP_l_ng_SB/30.974)]# deze zit in baggernut
  selb[,FeP_FW := (Fe_mg_l_ng_SB/55.845)/(Ptot_mgP_l_ng_SB/30.974)]
  selb[,FeS_FW := (Fe_mg_l_ng_SB/55.845)/(Stot_mgS_l_ng_SB/32.065)]
  selb[,FeP_PW := (Fe_mg_l_nf_PW/55.845)/(Ptot_mgP_l_nf_PW/30.974)]# deze zit in baggernut & is meest relevant
  selb[,FeS_PW := (Fe_mg_l_nf_PW/55.845)/(SO4_mg_l_nf_PW/96.06)]
  selb[,FeS_PW := (Fe_mg_l_nf_PW/55.845)/(Stot_mgS_l_nf_PW/96.06)]
  
  # calculate nalevering
  selb[,nlvr_FW := 0.0247 * Ptot_mgP_l_ng_SB - 1.6035]
  selb[,nlvr_PW := 0.8095 * Ptot_mgP_l_nf_PW - 0.2905]
  selb[,nlvr_olson_FW := 5.8 * (Ptot_mgPOlsen_l_ng_BS/30.974) - 1.1361]
 
  #FW
  selb[,classFeSP_FW := cut(FeSP_FW, breaks = c((min(FeSP_FW, na.rm = TRUE)-1), 1.4, 4, max(FeSP_FW, na.rm = TRUE)), labels = c('geen ijzerval', 'beperkte ijzerval', 'functionele ijzerval'))]
  selb[FeSP_FW >= 4, nlvr_FW := 0.1 * nlvr_FW] # BaggerNut Tool zegt 0-1
  selb[FeSP_FW < 4 & FeSP_FW > 1.4, nlvr_FW := 0.5 * nlvr_FW] # BaggerNut zegt < nlvr_FW & > 0-1
  selb[FeSP_FW <= 1.4, nlvr_FW := nlvr_FW]
  selb[nlvr_FW < 0,nlvr_FW := 0]
  #PW
  selb[FeP_PW > 10 & FeS_PW > 1, c('nlvr_PW','classFESPPWratio') := list(0.1*nlvr_PW,'functionele ijzerval') ]  # BaggerNut zegt lage nalevering
  selb[FeP_PW > 1 & FeS_PW > 1, c('nlvr_PW','classFESPPWratio') := list(0.1*nlvr_PW,'functionele ijzerval') ]  # BaggerNut zegt lage nalevering
  selb[FeP_PW > 1 & FeS_PW <= 1, c('nlvr_PW','classFESPPWratio') := list(0.5*nlvr_PW,'beperkte ijzerval')] # BaggerNut zegt < nlvrPW 
  selb[FeP_PW <= 1, c('nlvr_PW','classFESPPWratio') := list(nlvr_PW,'geen ijzerval')]
  selb[nlvr_PW < 0,nlvr_PW := 0]

  return(selb)  

}
