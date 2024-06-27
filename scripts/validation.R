# read in the lastest data from maatregelen
val_ekrlijst <- function(ekrlijst){
  #check all EAGs within a waterbody are present
  # krw_val <- unique(ekrlijst[,c('KRW_SGBP3','EAGIDENT','jaar')])
  # krw_val <- krw_val[!is.na(KRW_SGBP3)&!KRW_SGBP3 == ""&!is.na(EAGIDENT),]
  krw_val <- dcast(ekrlijst,KRW_SGBP3+EAGIDENT+facet_wrap_code+level~jaar, value.var = 'locatie', fun.aggregate = uniqueN)
  krw_val <- krw_val[!is.na(KRW_SGBP3)&!KRW_SGBP3 == ""&!is.na(EAGIDENT)&level==1,]
  
  return(krw_val)
 
}

val_hybi <- function(hybi){
  
  hybi_val <- dcast(hybi,analyse+KRW_SGBP3+EAGIDENT~jaar, value.var = 'locatie', fun.aggregate = uniqueN)
  # hybi_val <- hybi_val[!is.na(KRW_SGBP3)&!KRW_SGBP3 == ""&!is.na(EAGIDENT),]
  
  return(hybi_val)
  
}

val_hybi2 <- function(hybi){
  
  hybi_val <- dcast(hybi,analyse+KRW_SGBP3+watertype+EAGIDENT+compartiment~jaar, value.var = 'locatie', fun.aggregate = uniqueN)
  # hybi_val <- hybi_val[!is.na(KRW_SGBP3)&!KRW_SGBP3 == ""&!is.na(EAGIDENT),]
  
  return(hybi_val)
  
}
