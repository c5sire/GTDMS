######################
#
# Fieldbook import/export facilities: to and from Excel
#
#
#######################
library(RSQLite)
##### data dictionary

dbname = "res/gtdms.db"

fbCreateDictionary <- function(dbname=dbname){
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



