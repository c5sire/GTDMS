###############################################################################
#
# Merges existing preferences with a new version of preferences by
# overwriting 'past' values into the new version; then overwriting the old,
# reloading and removing the former new file.
#
# Sep 7, 2012
# 4:14:46 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

auto.merge.prior.preferences <- function(db, fp){
	old.db = db
	new.db = NULL
	np = file.path(dirname(fp),"prefs-new.txt")
	if(file.exists(np)){
		new.db = try(
			read.csv(np, header=T, sep="\t", stringsAs=F)
		)

	  if(class(new.db)=="data.frame"){
		add.rec = new.db[!new.db$name %in% old.db$name,]
		old.db = rbind(old.db, add.rec)
		#new.db[new.db$name %in% old.db$name,"past"] = old.db$past
		write.table(old.db, file=fp,sep="\t", row.names=F)
		unlink(np)
	  } 
  	}
	return(old.db)
}
