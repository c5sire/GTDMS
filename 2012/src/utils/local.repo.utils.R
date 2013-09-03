###############################################################################
#
# TODO: Add comment
#
# Sep 24, 2011
# 7:19:51 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################
library(RSQLite)


get.local.db.root <- function(){
	#db.root = "K:/packages/GDET4RT/data"  # change this from a config file in the final app
	#scan("res/datapath.conf",character())
	prefs = get.prefs()
	dp = prefs[prefs$pr_name=="dataPath","pr_past"]
	#special case for local sample database
	res=dp
	if(dp=="data" | !file.exists(dp)) res=file.path(data.dir(),res)
	res
}

fb.status.path <- function(){
	crop = getCurrentCrop()
	db.name = str_replace(fb.status.db.name,"fieldbook", paste("fieldbook_",crop,sep="")) #GTDM-309
	fpath = file.path(get.local.db.root(),db.name)
	fpath
}

exists.fb.status.db <- function(){
	file.exists(fb.status.path())
}

fb.connect <- function(){
	dbConnect("SQLite", fb.status.path())
}

has.status.table <- function(){
	con = fb.connect()
	res = dbListTables(con)
	dbDisconnect(con)
	if(length(res)==0) return(FALSE)
	return(TRUE)
}

check.fb.status.db.table <- function(){
	#c("crop","planting.year","country","trial.type")
	con = fb.connect()
	flds=dbListFields(con,"status")
	dbDisconnect(con)
	flds
}


create.fb.status.db <- function(){
	sql="CREATE TABLE status (fieldbook_id char(20), crop char(15), planting_year integer(4),country char(25), 
			trial_type char(15),  contact char(25), checkin char(19), last_update char(19), registered_on char(19),
			last_shared_on char(19), published_on char(19), file_path char(255))"
	sql=gsub("\n","",sql)
	con <- fb.connect()
	rs <- dbSendQuery(con, sql)
	con
}


sql.fb.status <-function(sql){
	if(!has.status.table()){
		con = create.fb.status.db()
	} else 	con <- fb.connect()
	rs <- dbSendQuery(con, sql)
	dat <- fetch(rs)
	dbClearResult(rs)
	dbDisconnect(con)
	dat
}

empty.db <-function(){
	sql = "DELETE FROM status"
	sql.fb.status(sql)
}

get.fb.status <- function(){
	sql = "SELECT * FROM status ORDER BY crop, planting_year DESC, country ASC, trial_type ASC"
	sql.fb.status(sql)
}

create.dummy.fb.status <- function(n=100){
	#use dbWriteTable for entire frame
	#n = 100
	crop = sample(c("potato","sweetpotato"),n, repl=T, pr=c(0.7, 0.3))
	#crop = sample(c("potato"),n, repl=T)
	planting_year = sample(2000:2011,n,repl=T)
	#country = sample(c("Peru","Kenya"),n,repl=T, prob=c(.8,.2))
	country = sample(c("Peru"),n,repl=T)
	trial_type=sample(c("GxE","yield","late blight"), n, repl=T, prob=c(0.3,0.6,0.1))
	#trial_phase=sample(c("ph1","ph2","ph3"), n, repl=T, prob=c(0.3,0.3,0.4))
	fieldbook_id=paste(
			toupper(abbreviate(crop,2)), 
			toupper(abbreviate(trial_type,2)),
			planting_year,
			#toupper(abbreviate(trial_phase,2)),
			"_",
			toupper(abbreviate(country,3)), sep="")
	
	status = as.data.frame(cbind(fieldbook_id, crop, planting_year, country, trial_type),stringsAsF=F)
	con = fb.connect()
	dbWriteTable(con,"status",status,overwrite=T)
	dbDisconnect(con)
}

get.fieldbook.status.con <- function(){
	#library(RSQLite)
	#con = create.fb.status.db(fpath)
	if(exists.fb.status.db()){
		con=fb.connect()
	} else {
		con=create.fb.status.db() 
	}
	con
}

set.fb.dir <- function(dir, w){
	#write(dir,"res/datapath.conf")
	#print(dir)
	#write to prefs
	prefs = get.prefs()
	fp = file.path(getwd(),"res","prefs.txt")
	prefs[prefs$pr_name=="dataPath","pr_past"]=dir
	write.table(prefs,fp, sep="\t", row.names=F)
	#check that two additional directories exist
	dir.create(file.path(dir,"accepted"), rec=T, show=F)
	dir.create(file.path(dir,"rejected"), rec=T, show=F)
	#refresh GUI
	w=refresh(w)
	#refresh()
}

get.checkin.log <- function(){
	files = list.files(file.path(get.local.db.root(),"rejected"))
	rej = paste("Rejected: ",files,collapse="\n")
	files = list.files(file.path(get.local.db.root(),"accepted"))
	acc = paste("Accepted: ",files,collapse="\n")
	
	
	#clean up
	unlink(file.path(get.local.db.root(),"accepted","*.*"))
	unlink(file.path(get.local.db.root(),"rejected","*.*"))
	
	paste(acc,"\n\n",rej,sep="")
}

is.fieldbook.in.status.db <- function(fieldbook_id){
	sql = paste("SELECT * FROM status WHERE fieldbook_id='",fieldbook_id,"'",sep="")
	nrow(sql.fb.status(sql))>0
}

update.trial.in.status.db <-function(fieldbook_id, crop=NULL, planting_date, country, trial_type,  
		contact, checkin, target_path){
	if(is.na(fieldbook_id)) fieldbook_id=""
	#if(is.na(crop)) crop=""
	if(is.na(planting_date)) planting_date=""
	if(is.na(country)) country=""
	if(is.na(trial_type)) trial_type=""
	if(is.na(contact)) contact=""
	if(is.na(checkin)) checkin=""
	if(is.na(target_path )) target_path=""
	planting_year=substr(planting_date,1,4)
	#TODO something causes warnings still after recreating the whole database
	if(!is.fieldbook.in.status.db(fieldbook_id)){
		sql = paste(
		"INSERT INTO status (fieldbook_id, planting_year, country, trial_type,  
		contact,checkin,last_update,file_path) VALUES('",
		fieldbook_id,"','", planting_year,"','", country,"','", trial_type,
		"','", contact, "','",checkin,"','",checkin,"','",target_path,"')",sep="")
	} else {
		sql = paste(
		"UPDATE status SET planting_year='",planting_year,"', country='",country,
		"', trial_type='",trial_type,"', contact='",contact,
		"',last_update='",checkin,"', file_path='",target_path,"' WHERE fieldbook_id='",fieldbook_id,"'",
		sep="")
	}
	sql=gsub("\n","",sql)
	sql=gsub("\t","",sql)
	#print(sql)
	sql.fb.status(sql)
	#sql
}

	