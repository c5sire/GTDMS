# Functions to import, process, store, format and export fieldbooks
#
# fbCreateFieldbookTables
# fbAddVariablesToFieldbook
# fbLoad  load from Excel file
# fbDetect used for importing files from previous datacollector installations
# fbImport scan a directory and import; parameters for type of file: datacollector, 
#           clonselector, IBP, simple table, ...
# fbCheck  QC
# fbPut    put fb into database; save individual fields of fieldbook
# fbGet    get fb from database (a named list of dataframes)
# fbWrite  use XLConnect library to format sheets
# fbList   get a list of all fieldbook names
# fbMinimal get simple list of fb passport data

fbFormatFbVar <- function(tbl, i){
  rec = tbl[i,]
  out = ""
  if(str_detect(rec$TYPE,"Text")){
    out = paste(rec$ABBR,"TEXT")
  }
  if(str_detect(rec$TYPE,"Discrete")){
    out = paste(rec$ABBR,"INT")
  }
  if(str_detect(rec$TYPE,"Continuous")){
    out = paste(rec$ABBR,"REAL")
  }
  if(str_detect(rec$TYPE,"Date")){
    out = paste(rec$ABBR,"CHAR(10)") #yyyy-mm-dd
  }
  out
}

fbCreateFieldbook <- function(con, tbl){
  n = nrow(tbl)
  s = "CREATE TABLE fieldbooks (
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  PLOT INT,
  BLOCK INT,
  SBLOCK INT,
  REP INT(1),
  GENOTYPEID CHAR(50),
  FACTOR2 CHAR(50),"
  
  for(i in 1:n){
    s = paste(s,fbFormatFbVar(tbl, i))
    if(i<n) {
      s= paste(s,",\n",sep="")
    } else {
      s = paste(s,");", sep="")
    }
  }
  #con = gtdmsConnect()
  dbGetQuery(con,"DROP TABLE fieldbooks")
  dbGetQuery(con, s)
}

fbCreateMinimal <- function(con){
  nm="minimal"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  fbFields TEXT,
  VERSION CHAR,
  CROP CHAR,
  TRIALTYPE CHAR,
  COMMENTS CHAR,
  BEGINDATE CHAR(10),
  ENDDATE CHAR(10),
  LEADER CHAR,
  COLLABORATORS CHAR,
  SITESHORT CHAR,
  AEZ CHAR,
  CIPREGION CHAR,
  CONTINENT CHAR,
  COUNTRY CHAR,
  ADMIN1 CHAR,
  ADMIN2 CHAR,
  ADMIN3 CHAR,
  LOCALITY CHAR,
  ELEVATION INT(4),
  LATD REAL,
  LOND REAL,
  OWNER CHAR,
  PUBLISHER CHAR,
  DATATYPE CHAR,
  FORMAT CHAR,
  IDENTIFIER CHAR,
  LANGUAGE CHAR(6),
  RELATION CHAR,
  LICENSE CHAR,
  AUDIENCE CHAR,
  PROVENANCE CHAR,
  EMBARGO CHAR(10),
  QUALITY REAL,
  STATUS CHAR,
  DONOR CHAR,
  PROJECTNAME CHAR,
  PROJECTSTART CHAR(10),
  PROJECTEND CHAR(10),
  fbVariables TEXT
  );")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  
  dbGetQuery(con, s)
}

fbCreateInstallation<- function(con){
  nm="installation"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  EXPDESIGN CHAR,
  GENDESIGN CHAR,
  FACLABELS CHAR,
  NUMREPS INT(1),
  BSIZE INT,
  BNUMBER INT,
  EXPENV CHAR,
  NPPPLOT INT,
  NPPSPLOT INT,
  NROWPLOT INT,
  NROWSPLOT INT,
  NPLANTROW INT,
  PSIZE REAL,
  DPLANTS REAL,
  DROWS REAL,
  PDENS REAL,
  ROWDIR REAL,
  PLANTMODE CHAR,
  AREA REAL,
  FACTOR2 CHAR,
  LEVEL1 CHAR,
  LEVEL2 CHAR,
  LEVEL3 CHAR,
  LEVEL4 CHAR,
  LEVEL5 CHAR,
  LATD1 REAL,
  LOND1 REAL,
  LATD2 REAL,
  LOND2 REAL,
  LATD3 REAL,
  LOND3 REAL,
  LATD4 REAL,
  LOND4 REAL,
  ISPROUTLENG REAL,
  CROT1 CHAR,
  CROT2 CHAR,
  CROT3 CHAR,
  CROT4 CHAR,
  CROT5 CHAR,
  SENSELEV INT
  );")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  
  dbGetQuery(con, s)
  
}
fbCreateMaterialList <- function(con){
  nm="materiallist"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  CONTROL CHAR(1),
  GENOTYPEID CHAR,
  CLONENAME CHAR,
  CLONEID CHAR,
  FAMILYID CHAR,
  FEMALEID CHAR,
  FEMALECODE CHAR,
  MALEID CHAR,
  MALECODE CHAR,
  SEEDSOURCE CHAR,
  REFSIMTRIALS CHAR,
  REFPRETRIALS CHAR,
  SAUDPC INT(1));")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  
  dbGetQuery(con, s)
  
}
fbCreateSoilAnalysis <- function(con){
  nm="soilanalysis"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  SAMPDATE CHAR(10),
  RQSTR CHAR,
  OPRTR CHAR,
  SLATD REAL,
  SLOND REAL,
  SLABODE CHAR,
  SSAMPLEID CHAR,  
  SFIELDCODE CHAR,
  SOILPH REAL,
  SOILEC REAL,
  CACO2 REAL,
  ORGM REAL,
  NITR REAL,
  PHOS REAL,
  KALI REAL,
  SAND REAL,
  SILT REAL,
  CLAY REAL,
  STEX REAL,
  SCEC REAL,
  EXCA2 REAL,
  EXMG2 REAL,
  EXK REAL,
  EXNA REAL,
  AL3H REAL,
  TCAT REAL,
  TBAS REAL,
  BSAT REAL,
  EXACI REAL,
  SAEC REAL,
  SOILFE REAL,
  SOILCU REAL,
  SOILZN REAL,
  SOILB REAL,
  SOILMN REAL,
  SOLCA REAL,
  SOLMG REAL,
  SOLK REAL,
  SOLNA REAL,
  SOLCL REAL,
  SOILCARB REAL,
  SOILBICARB REAL,
  SOILNITRATE REAL,
  SOILSULFATE REAL,
  SOILPHOSPH REAL,
  SOILGYPSUM REAL,
  SOILWRC REAL,
  SOILFC REAL,
  SOILWP REAL,
  SOILTEMP REAL
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  
  dbGetQuery(con, s)
  
}
fbCreateHoboData <- function(con){
  nm="hobodata"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  HMONTH INT,
  HDAY INT,
  HYEAR INT,
  HTIME INT,
  HTIMEINTVL REAL,
  HTEMP REAL,
  HRELH REAL
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  dbGetQuery(con, s)
}

fbCreateWeatherData <- function(con){
  nm="weatherdata"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  WMONTH INT,
  WDAY INT,
  WYEAR INT,
  WTIME INT,
  WRAIN REAL,
  WTAVG REAL,
  WTMIN REAL,
  WTMAX REAL,
  WTAMP REAL,
  WRELH REAL,
  WMINRELH REAL,
  WMAXRELH REAL,
  WSOLRAD REAL,
  WBARPRS REAL,
  WDEWPNT REAL,
  WGUSTSP REAL,
  WWNDDIR REAL,
  WTSOIL1 REAL,
  WTSOIL2 REAL,
  WTSOIL3 REAL,
  WTSOIL4 REAL,
  WWCONT1 REAL,
  WWCONT2 REAL,
  WWCONT3 REAL,
  WWCONT4 REAL
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  dbGetQuery(con, s)
}

fbCreateCropManagement <- function(con){
  nm="cropmanagement"
  s = paste("CREATE TABLE",nm,"(
  fbID INT PRIMARY KEY NOT NULL,
  fbName CHAR(25),
  ITRCAT CHAR,
  ITRTYP CHAR,
  CMDATE CHAR(10),
  CMOPRT CHAR,
  CMOBSV CHAR,
  CMACTI CHAR,
  CMPRCN REAL,
  CMDOSE REAL,
  CMUNMS REAL
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  dbGetQuery(con, s)
}

fbCreateMaterialGroup <- function(con){
  nm="materialgroup"
  s = paste("CREATE TABLE",nm,"(
  mgID INT PRIMARY KEY NOT NULL,
  mgName CHAR(25),
  GENOTYPEID CHAR(50)
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  dbGetQuery(con, s) 
}


fbCreateTemplateList <- function(con){
  nm="templatelist"
  s = paste("CREATE TABLE",nm,"(
  tlID INT PRIMARY KEY NOT NULL,
  tlName CHAR(25),
  tlMinimal TEXT,
  tlInstallation TEXT,
  tlMaterialList TEXT,
  tlSoilAnalysis TEXT,
  tlHoboData TEXT,
  tlWeatherData TEXT,
  tlCropManagement TEXT
);")
  if(dbExistsTable(con, nm)){
    dbRemoveTable(con,nm)
  }
  dbGetQuery(con, s)  
}


fbCreateFieldbookTables <- function(dbname=dbname){
  # get datadictionary
  con = gtdmsConnect()
  tbl = dbGetQuery(con,"SELECT * FROM datadictionary WHERE AGROUP != 'Documentation'")
  # create central table: fieldbooks with all possible field names + plus factors
  fbCreateFieldbook(con,tbl)
  # create other supporting tables with id field for fieldbook
  fbCreateMinimal(con)
  fbCreateInstallation(con)
  fbCreateMaterialList(con)
  fbCreateSoilAnalysis(con)
  fbCreateHoboData(con)
  fbCreateWeatherData(con)
  fbCreateCropManagement(con)
  
  fbCreateMaterialGroup(con)
  fbCreateTemplateList(con)
  dbDisconnect(con)
}


fbLoad <- function(filepath){
  # get list of sheets
  # compare against list of known sheets
  # allow old inconsistent names
  # construct list of dataframes
  # return standardized list of known dataframes
}


#fbCreateFieldbookTables()

# Test loading fieldbook

#fb = readWorksheetFromFile("inst/res/PTDS201107_PHAO.xls")








