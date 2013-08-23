###############################################################################
#
# This must be called from get.prefs routine.
# It makes sure that for all templates in the correct directory corresponding pref
# entries are present.
#
# Sep 7, 2012
# 4:14:12 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

list.dir.templates <- function(){
	fp = file.path(getwd(),"res")
	fl = list.files(fp, pattern="template_[A-Z]{4}.xls",rec=T)
	fn = gregexpr("[A-Z]{4}",fl)
	tp=character()
	for(i in 1:length(fn)){
		s=fn[[i]][1]
		tp[i] = substr(fl[i],s,(s+3))
	}
	tp
}

list.pref.templates <-function(db){
	nms = db$name[!is.na(str_extract(db$name,"[A-Z]{4}desc"))]
	str_sub(nms,1,4)
}

list.new.templates<-function(db){
	ldt = list.dir.templates()
	lpt = list.pref.templates(db)
	ntp = ldt[!ldt %in% lpt]
	ntp # character(0) if no new templates -> use length(ntp)
}

add.new.templates <- function(db, nt, fp){
	n = length(nt)
	for(i in 1:n){
		rec = c(paste(nt[i],"vars",sep=""),
				paste(nt[i]," variables",sep=""),
				"","","")
		db = rbind(db,rec)
		rec = c(paste(nt[i],"desc",sep=""),
				paste(nt[i]," descriptive summaries",sep=""),
				"","","")
		db = rbind(db,rec)
		rec = c(paste(nt[i],"anal",sep=""),
				paste(nt[i]," analyses",sep=""),
				"","","")
		db = rbind(db,rec)
	}
#	fp = paste(fp,".txt",sep="")
#	print(fp)
	write.table(db, file=fp,sep="\t", row.names=F)
}

auto.add.crop.template <- function(db, fp){
	
	nt = list.new.templates(db)
	if(length(nt)!=0){
		add.new.templates(db,nt,fp)
		db = read.csv(fp, header=T, sep="\t", stringsAs=F)
	}
	return(db)
}
