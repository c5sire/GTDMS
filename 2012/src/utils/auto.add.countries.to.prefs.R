###############################################################################
#
# TODO: Add comment
#
# Sep 8, 2012
# 9:38:25 AM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

# list.master.countries <- function(){
# 	fp = file.path(getwd(),"res","Master-list-trial-sites.xlsx")
# 	db = read.xlsx2(fp,sheetName="Sites",startRow=2)
# 	unique(as.character(db$CNTR))
# }

list.pref.countries <-function(prefs){
	prefs$pr_name
}

list.new.countries <- function(prefs){
	pc = list.pref.countries(prefs)
	mc = list.master.countries()
	mc[!mc %in% pc]
}

add.new.countries <- function(db, nc,fp){
	n = length(nc)
	for(i in 1:n){
		rec = c(nc[i],
				nc[i],
				"","","")
		db = rbind(db,rec)
	}
#	fp = paste(fp,".txt",sep="")
#	print(fp)
	write.table(db, file=fp,sep="\t", row.names=F)
	
}


# REDO!
auto.add.countries.to.prefs <- function(db, fp){
	
	nc = list.new.countries(db)
	if(length(nc)!=0){
		add.new.countries(db,nc,fp)
		db = read.csv(fp, header=T, sep="\t", stringsAs=F)
	}
	return(db)	
}

