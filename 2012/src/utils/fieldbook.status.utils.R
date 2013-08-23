###############################################################################
#
# TODO: Add comment
#
# Sep 24, 2011
# 8:06:52 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

get.fieldbooks <- function(){
	afun <- function(){
		data=get.fb.status()
		dsl=c("fieldbook_id","planting_year","country",
				"trial_type","contact","last_update","published_on")
		sel = c("_FIELDBOOK_ID_","_PLANTING_YEAR_","_COUNTRY_",
				"_TRIAL_TYPE_","_CONTACT_","_LAST_UPDATE_","_PUBLISHED_ON_")
		#labels = get.meta.label(sel)	
		labels = translate(sel)
		dat = data[,dsl]
		names(dat)=labels
		dat
	}
	dat=NULL
	try((dat=afun()))
	dat
}

get.crops <- function(){
	dat=get.fb.status()
	res = ""
	if(nrow(dat)<1) res="none"
	res = sort(unique(dat[,"crop"]))	
	res
}

get.years <- function(){
	dat=get.fb.status()
	res = ""
	if(nrow(dat)<1) res="none"
	res = sort(unique(dat[,"planting_year"]))
	res
}

get.trial.types <- function(){
	dat=get.fb.status()
	res = ""
	if(nrow(dat)<1) res="none"
	res = sort(unique(dat[,"trial_type"]))
	res
}

get.countries <- function(){
	dat=get.fb.status()
	res = ""
	if(nrow(dat)<1) res="none"
	res = sort(unique(dat[,"country"]))
	res
}

#get.trial.phases <- function(){
#	dat=get.fb.status()
#	res = ""
#	if(nrow(dat)<1) res="none"
#	res = sort(unique(dat[,"trial_phase"]))
#	res
#}

read.fb <-function(fileName, sheet){
	#print("Hello")
	wb=read.xlsx(fileName,sheetName=sheet)
	#print("Reinhard")
	for(i in 1:ncol(wb)) wb[,i]<-as.character(wb[,i])
	row.names(wb)=wb[,1]
	wb
}

