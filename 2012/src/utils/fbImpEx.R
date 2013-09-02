######################
#
# Fieldbook import/export facilities: to and from Excel
#
#
#######################
library(RSQLite)
##### data dictionary

dbname = "res/gtdms.db"



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
  sql = paste("SELECT * FROM datadictionary", sep="")
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

getSiteList = function(countries, full=TRUE){
  con = dbConnect(SQLite(),dbname)
  sql = paste("SELECT CNTRY, SHORTN, FULLN FROM trialsites", sep="")
  res = NULL
  try({
    res = dbGetQuery(con, sql)
    res = res[res$CNTRY %in% countries, ]
    if(full){
      res = res$FULLN
    } else {
      res = res$SHORTN
    }
    res = sort(res)
  })
  dbDisconnect(con)
  return(res)
}


