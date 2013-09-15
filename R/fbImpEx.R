######################
#
# Fieldbook import/export facilities: to and from Excel
#
#
#######################
library(RSQLite)
##### data dictionary

dbname = "gtdms.db"

#' Initialize the working database from a preloaded copy
#' 
#' @author Reinhard Simon
#' @export
#' 
initFbDb <- function(){
  if(!file.exists(dbname)){
    fn = system.file("res/gtdms.db", package = "cloneselector")
    file.copy(fn, dbname)
  }
}

#' Retrieve active countries from local database
#' 
#' @return character vector of country names
#' @author Reinhard Simon
#' @export
#' 
getActiveCountries <- function(){
  prefs = get.prefs()
  cnms  = prefs[prefs$pr_name == "acountries","pr_past"]
  cnms  = str_split(cnms,";")
  cnms[[1]]
}

fbCreateDataDictionary <- function(dbname=dbname){
sqlDefDict = "CREATE TABLE datadictionary (
  ID INT PRIMARY KEY NOT NULL,
  AGROUP CHAR(50),
  DISCIPLINE CHAR(50),
  SUBGR CHAR(50),
  SEQ CHAR(5),
  LAYOUT CHAR(1),
  CROP CHAR(25),
  VAR CHAR(50),
  ABBR CHAR(25),
  SELDIR CHAR(1),
  SELWGT REAL,
  FORMULA CHAR(200),
  AFORMULA CHAR(200),
  REP CHAR(50),
  LOGDATE CHAR(10),
  LOGOP CHAR(25),
  LOG CHAR(20),
  PATTERN CHAR(50),
  DESC TEXT,
  TYPE CHAR(40),
  UNIT CHAR(10),
  STATES CHAR(200),
  USAGE CHAR(100),
  LOWER REAL,
  UPPER REAL,
  DIGITS INT(1),
  NOTE CHAR(150),
  REF TEXT
  );"

con = dbConnect(SQLite(), dbname)
dbGetQuery(con, sqlDefDict)
dbDisconnect(con)
}

get.data.dict = function(terms="all",sheetName="any"){
  #TODO Remove calls with paramters 'terms'
  con = dbConnect(SQLite(),dbname)
  sql = "SELECT * FROM datadictionary"
  res = NULL
  try({
    res = dbGetQuery(con, sql)
  })
  dbDisconnect(con)
  return(res)
} 


fbCreateDictionary <- function(dbname=dbname){
  sqlDefDict = "CREATE TABLE dictionary (
  id_label CHAR(50) PRIMARY KEY NOT NULL,
  en_US CHAR(50),
  es CHAR(50),
  es_PE CHAR(50),
  de CHAR(50),
  fr CHAR(50),
  pt CHAR(50),
  sw CHAR(50),
  id CHAR(50)
  );"
  
  con = dbConnect(SQLite(), dbname)
  dbGetQuery(con, sqlDefDict)
  dbDisconnect(con)
}

get.dict <- function(){
  #read.csv(file.path("res","dictionary.csv"),header=T,sep="\t", stringsAs=F)
  con = dbConnect(SQLite(),dbname)
  sql = paste("SELECT * FROM dictionary", sep="")
  res = NULL
  try({
    res = dbGetQuery(con, sql)
  })
  dbDisconnect(con)
  return(res)
  
}

fbCreateTrialSites <- function(dbname=dbname){
  sqlDefDict = "CREATE TABLE trialsites (
  ID INT PRIMARY KEY NOT NULL,
  SHORTN CHAR(25),
  ALTERN CHAR(25),
  FULLN CHAR(25),
  LOCAL CHAR(25),
  LATD REAL,
  LOND REAL,
  ELEV INT,
  CROPS CHAR(200),
  AEZ CHAR(25),
  CONT CHAR(25),
  CREG CHAR(5),
  CNTRY CHAR(50),
  ADM4 CHAR(50),
  ADM3 CHAR(50),
  ADM2 CHAR(50),
  ADM1 CHAR(50)
  );"
  
  con = dbConnect(SQLite(), dbname)
  dbGetQuery(con, sqlDefDict)
  dbDisconnect(con)
}

#' Retrieve registered countries from local database
#' 
#' @return character vector of country names
#' @author Reinhard Simon
#' @export
#' 
getCountryList = function(){
  con = dbConnect(SQLite(),dbname)
  sql = paste("SELECT CNTRY FROM trialsites", sep="")
  res = NULL
  try({
    res = dbGetQuery(con, sql)
    res = sort(unique(res$CNTRY))
  })
  dbDisconnect(con)
  return(res)
}

getSites <- function(){
  con = dbConnect(SQLite(),dbname)
  sql = "SELECT CNTRY, SHORTN, FULLN FROM trialsites"
  res = NULL
  try({
    res = dbGetQuery(con, sql)
  })
  dbDisconnect(con)
  return(res)
  
}

getSiteList = function(countries, full=TRUE, mini=FALSE){
res = NULL
  try({
    res = getSites()
    res = res[res$CNTRY %in% countries, ]
    if(mini) return(res)
    if(full){
      res = res$FULLN
    } else {
      res = res$SHORTN
    }
    res = sort(res)
  })
  return(res)
}

list.master.countries <- function(){
  getCountryList()
}

add.countries <- function(fp) {
# 1. Minimal checks on type of archive: is.excel?
  test = logical(8)
  test[1] = is.Excel(fp)
  assert(test[1], paste("Adding countries: File '",fp,"' ist not an Excel file.", sep=""))
# 2. Has the expected sheet?
  sheetName = "Sites"
  wb = loadWorkbook(fp)
  sheets = getSheets(wb)
  
# 3a. Has expected sheet?
  ns = names(sheets)
  test[2] = (sheetName %in% ns)
  assert(test[2],"Adding countries: File does not have expected sheet 'Sites'." )
  data = read.xlsx2(fp,sheetName, stringsAsFactors = FALSE)
  
#  3b. Has all expected fields?
  con = dbConnect(SQLite(), dbname)
  fnms = names(data)
  dnms = dbListFields(con, "trialsites")
  dnms = dnms[-1]
  
  test[3] = all(dnms %in% fnms)
  assert(test[3],"Adding countries: File does not have expected column names.")
  
  test[4] = !is.integer(data[,'ELEV'])
  assert(test[4], "Adding countries: column ELEV has non integer values.")
  test[5] = !is.numeric(data[,'LATD'])
  assert(test[5], "Adding countries: column LATD has non numeric values.")
  test[6] = !is.numeric(data[,'LOND'])
  assert(test[6], "Adding countries: column LOND has non numeric values.")
  
  data[,"ELEV"] = as.integer(data[,'ELEV'])
  data[,"LATD"] = as.numeric(data[,'LATD'])
  data[,"LOND"] = as.numeric(data[,'LOND'])
  
# 4. Check that the new location names are not duplicated or already in the database
  sts=getSites()
  ddups = data$SHORTN %in% sts$SHORTN
  ddids = paste(data$SHORTN[ddups], collapse=", ")
  test[7] = !any(ddups) 
  assert(test[7], paste(
         "Adding countries: attempting to add records with SHORTN = '",ddids,"' duplicating the database.",
         sep=""))
  ddshn = paste(data$SHORTN[duplicated(data$SHORTN)], collapse=", ")   
  test[8] = !any(duplicated(data$SHORTN))
  assert(test[8], 
         paste("Adding countries: table contains duplicated values in SHORTN: '",ddshn,"'.", 
         sep=""))
  
# 5. Add countries to country table
  if(any(test)){
  # add ID column values
  n = nrow(sts)
  ID= (n+1):(n+nrow(data))
  data = cbind(ID, data)
  
  
  dbWriteTable(con,"trialsites", data, append=TRUE, row.names=F)
  
  
# 6. Update preferences  
  # Check if new countries
  d.cntrs = getCountryList()
  n.cntrs = unique(data$CNTRY)
  
  n.cntrs = n.cntrs[n.cntrs %in% d.cntrs]
  
  # if so, add to preference table corresponding records and list of sites in 'past'
  if(length(n.cntrs) > 0){
    nprefs=prefs[-c(1:nrow(prefs)),]
    for(i in 1:length(n.cntrs)){
      lsts = data[data$CNTRY==n.cntrs[i], "SHORTN"]
      lsts = paste(lsts,collapse=";")
      rec = c(n.cntrs[i], n.cntrs[i],"","", lsts)
      rec = as.data.frame(t(rec))
      names(rec) = names(prefs)
      nprefs = rbind(nprefs,rec)
    }
    
  }
  dbWriteTable(con,"preferences", nprefs, append=TRUE, row.names=F)
  dbDisconnect(con)
  }
}


chooseCountries <- function(){
  fp = choose.files(default = "", caption = "Select country file to add!",
               filters = c("Excel","*.xls") )
  res=try(
    add.countries(fp), silent=TRUE
    )
  if(inherits(res,"try-error")){
    ok = gmessage(res[1], "Adding countries results.", icon="error")
  } else {
    ok =gmessage("New trialsites added.", "Adding countries results.", icon="info")
  }
  
}




##########################

fbCreatePrefs <- function(dbname=dbname){
  sqlDefDict = "CREATE TABLE preferences (
  pr_name CHAR(25) PRIMARY KEY NOT NULL,
  pr_label_en CHAR(50),
  pr_values TEXT,
  pr_default TEXT,
  pr_past TEXT
  );"
  
  con = dbConnect(SQLite(), dbname)
  dbGetQuery(con, sqlDefDict)
  dbDisconnect(con)
}

get.prefs <- function(){
  con = dbConnect(SQLite(),dbname)
  sql = "SELECT * FROM preferences"
  res = NULL
  try({
    res = dbGetQuery(con, sql)
  })
  dbDisconnect(con)
  return(res)
}

write.prefs <-function(prefs){
  con = dbConnect(SQLite(),dbname)
  dbWriteTable(con,"preferences", prefs, overwrite=TRUE, row.names=F)
}

putPrefs <- function(prefs, vals){
  for(i in 1:length(vals)){
    nm = names(vals[i])
    #print(vals[[i]])
    if(!is.na(vals[[i]])){
      if(length(vals[[i]])==1 ){
        prefs[prefs$pr_name==nm,"pr_past"]=vals[[i]]	
      } else{
        prefs[prefs$pr_name==nm,"pr_past"]=paste(vals[[i]], collapse=";")
      }
    }
  }
  #write.table(prefs,fp, sep="\t", row.names=F)
  write.prefs(prefs)
  #prefs
}


###########################
# update/synchronize gtdms.db : 
# how to tackle the situation when a newer version is installed?
# Basic solution: new version comes without a gtdms.db. If no file is found
# a new database is created from a prefilled 'template'.
# An existing db is initially checked (use a flag by current version) if it has all expected
# tables and each table has all expected fields. Missing ones are added.
#
# This replaces partially the old necessary steps in get.prefs():
# 1. auto merge prior prefs
# 2. auto add crop templates: now done explicitly by new function
# 3. auto add countries: now done explicitly by new functon








