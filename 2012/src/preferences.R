###############################################################################
#
# TODO: Add comment
#
# Sep 27, 2011
# 8:01:16 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


get.prefs <- function(){
	#read from Pref store: soon use sqlite
	#print("Reading prefs ...")
	fp = file.path(getwd(),"res","prefs.txt") 
	db = read.csv(fp, header=T, sep="\t", stringsAs=F)
	db = auto.merge.prior.preferences(db, fp)
	db = auto.add.crop.template(db, fp)
	db = auto.add.countries.to.prefs(db,fp)
	# add auto merge
	db
}

#
getCurrentCrop = function(){
	db = get.prefs()
	db[db$name=="crop","past"]
}

get.list.of.registered.crops = function(prefs){
	db = prefs
	cl = db[db$name=="crop",'values']
	cl = str_split(cl,";")[[1]]
	n = length(cl)
	re = list()
	for(i in 1:n){
		x = str_split(cl[i],"=")[[1]]
		re[i] = x[2]
		names(re)[i]=x[1]
	}
	re	
}



getCurrentFB = function(){
	db = get.prefs()
	db[db$name=="afieldbook","past"]
}

write.prefs <-function(prefs){
	fp = file.path(getwd(),"res","prefs.txt")
	write.table(prefs,fp, sep="\t", row.names=F)
}


putPrefs <- function(prefs, vals){
	fp = file.path(getwd(),"res","prefs.txt")
	for(i in 1:length(vals)){
		nm = names(vals[i])
		#print(vals[[i]])
		if(!is.na(vals[[i]])){
		if(length(vals[[i]])==1 ){
			prefs[prefs$name==nm,"past"]=vals[[i]]	
		} else{
			prefs[prefs$name==nm,"past"]=paste(vals[[i]], collapse=";")
		}
	    }
	}
	write.table(prefs,fp, sep="\t", row.names=F)
	prefs
}
#
toVector = function(string){
	res=string
	if(length(grep(":",string))>0){
		res = as.integer(as.vector(strsplit(string, ":"))[[1]])
		res = seq(res[1],res[2])
	}
	if(length(grep(";",string))>0){
		res = as.vector(strsplit(string, ";"))[[1]]
	}
	res
}
#
aPref = function(prefs,name){
	prefs[prefs$name==name,"past"]
}

save.prefs <- function(out, prefs){
	vals = c(
	out$logPrefix,
	out$years,
	out$season,
	out$sdesign,
	out$pstart,
	
	out$mbcrd.nreps,
	out$crd.nreps,
	out$f2crd.nreps,
	out$spcrd.nreps,
	
	out$rcbd.nbloc,
	out$f2rcbd.nbloc,
	out$sprcbd.nbloc,
	out$abd.nbloc,
	out$a01d.nbloc,
	
	out$kbloc,
	out$nSeeds,
	out$playout,
	out$bsize,
	
	out$f2crd.adf.name,
	out$f2crd.adf.level,
	out$f2rcbd.adf.name,
	out$f2rcbd.adf.level,
	out$spcrd.adf.name,
	out$spcrd.adf.level,
	out$sprcbd.adf.name,
	out$sprcbd.adf.level,
	out$abd.checks,
	out$msite
	)
	#print(out)
	#print(vals)
	names(vals)=c("logPrefix","years","season","sdesign","pstart",
			"mbcrd.nreps","crd.nreps","f2crd.nreps","spcrd.nreps",
			"rcbd.nbloc","f2rcbd.nbloc","sprcbd.nbloc","abd.nbloc","a01d.nbloc",
			"kbloc","nSeeds",	"playout","bsize",
			"f2crd.adf.name","f2crd.adf.level","f2rcbd.adf.name","f2rcbd.adf.level",
			"spcrd.adf.name","spcrd.adf.level","sprcbd.adf.name","sprcbd.adf.level",
			"abd.checks","msite")
	putPrefs(prefs,vals)
}
