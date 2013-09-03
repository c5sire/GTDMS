###############################################################################
#
#
# May 25, 2011
# 10:15:25 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

#<<<<<<< .mine
#ref.dir = "packages/GDET4RT"
#=======
#ref.dir = "DataCollector\\DC4RT"
#>>>>>>> .r109

get.version <- function(){
	paste("Data Collector version", get.version.nr())
}

data.dir = function(){
	#res="K:"
	res = getwd()
	#print(res)
#	if(!file.exists("D:/")){
#		res="K:/"
#	}
	#prefs = get.prefs()
	
	res
}

getLoG = function(){
	prefs = get.prefs()
	yrs = aPref(prefs,"years")
	ssn = aPref(prefs,"season")
	season = paste(yrs,ssn,sep="" )
	LoG = paste(aPref(prefs,"logPrefix"), aPref(prefs,"trialPhase"), season,sep="")
	LoG
}

getSeason = function(pref=pref.defaults){
	season = paste(
			pref$GermplasmList$years,
			pref$GermplasmList$seasons, 
			sep="")
	season
}

getLGP = function(){
	#file.path(data.dir(),ref.dir,pref.defaults$BreedingProgram$crop,"GermplasmList")	
	#file.path(data.dir(),ref.dir,getCurrentCrop(),"GermplasmList")
	file.path(data.dir(),getCurrentCrop(),"GermplasmList")
}

getLGPF = function(fn){
	path = getLGP()
	seas = substr(fn,nchar(fn)-7,nchar(fn)-5)
	fp   = file.path(path,seas,fn)
	fp
}

getLoGpath = function(){
	prefs=get.prefs()
	#print(prefs)
	yrs = aPref(prefs,"years")
	ssn = aPref(prefs,"season")
	season = paste(yrs,ssn,sep="" )
	#LoG = paste(aPref(prefs,"logPrefix"), aPref(prefs,"trialPhase"), season,sep="")
	
	#season = getSeason(pref)
	path = paste(data.dir(),aPref(prefs,"crop"),"/GermplasmList/",season,
			sep="")	
	
	path
}


show.table = function(ttype, tname, tdata, cont=w){
	#if(!is.null(nb)) dispose(nb)
	#nb = gnotebook(tab.pos = 1,expand=T,cont=w)
	tDlg <- xgwindow(ttype,visible=F)
	gm <- ggroup(horizontal = F, container=tDlg)
	nb = gnotebook(expand=T,cont=gm)
	#if(is.null(tbl)){
	tbl=gtable(tdata,expand=T,cont=nb)
	
	#names(tbl)[1]=title
	names(nb)[1] <- tname
	gbutton("ok", handler = function(h,...)	{dispose(tDlg)},
			container=gm)
	visible(tDlg)=T
}	


filename = function(fpath){
	fn = list.files(fpath, ".xlsx", r=T)
	if(length(fn)==0) fn = list.files(fpath, ".xls", r=T)
	fn = basename(fn)
	#fn = sapply(fn,strsplit(fn,"\\.")[[1]][1])
	ff = function(x) strsplit(x,"\\.")[[1]][1]
	fn = sapply(fn,ff)
	as.character(fn)
}

fn2LoG=function(fn){
	bn = basename(fn)
	bn = strsplit(bn,"\\.")[[1]][1]
	bn
}

expandListName2Path = function(fpath,fn, ext=".xlsx"){
	#file.path(fpath,substr(fn,nchar(fn)-2,nchar(fn)),paste(dget("bin/temp.txt"),".xlsx",sep=""))
	file.path(fpath,substr(fn,nchar(fn)-2,nchar(fn)),paste(fn,ext,sep=""))
}


expandListName2FBPath = function(fpath,fn, ext=".xlsx"){
	#file.path(fpath,substr(fn,nchar(fn)-2,nchar(fn)),paste(dget("bin/temp.txt"),".xlsx",sep=""))
	# is fieldbook part of a trial series? Guess from extension
	n = substr(fn,nchar(fn),nchar(fn))
	r = length(grep("[0-9]{1}",n))
	if(r==1){
		season = substr(fn,nchar(fn)-3,nchar(fn)-1)
	} else {
		season = substr(fn,nchar(fn)-2,nchar(fn))
	}
	file.path(fpath,season,paste(fn,ext,sep=""))
}


has.design <- function(fn){
	res = FALSE
	#fp = getLGPF(fn)
	wb <- loadWorkbook(fn)  
	sheets <- getSheets(wb)
	if(length(sheets)>1) res=TRUE
	res
}

getNoG = function(fp){
	#fp = getLGPF(fn)
	d1 = read.xlsx2(fp,"GermplasmList")
	nrow(d1)
}


getBaseDir <- function(){
	file.path(data.dir())
	
}



getResourcePath <- function(type="sites"){
	#path = getBaseDir()
	ext = "res"
	fil = ""
	if(type=="sites"){
		fil = "Master-list-trial-sites.xlsx" 
	}
	if(type=="dictionary"){
		fil = "data_dictionary.xls" 
	}
	path = file.path(getBaseDir(),ext,fil)
	path
}

getResourceSheet <- function(fpath,sheetName){
	data = read.xlsx2(fpath,sheetName)
	for(i in 1:ncol(data)){
		data[,i]=as.character(data[,i])
	}
	names(data)=data[1,]
	data = data[-1,]
	data
}

getResourceData <- function(type, sheetName){
	fp = getResourcePath(type)
	getResourceSheet(fp, sheetName)
}

getResourceDataSel <- function(type, sheetName, column){
	sts = getResourceData(type, sheetName)
	sts=sts[sts$SELECT=="x",column]
	sts
}

# getCountryList = function(){
# 	sts=getResourcePath("sites")
# 	db=getResourceSheet(sts,"Sites")
# 	sort(unique(db$CNTRY))
# }
# 
# getSiteList = function(countries, full=TRUE){
# 	sts=getResourcePath("sites")
# 	db=getResourceSheet(sts,"Sites")
# 	if(full) return(db[db$CNTRY%in%countries,"FULLN"])
# 	return(db[db$CNTRY%in%countries,"SHORTN"])
# }

# readSites = function(fn){
# 	sites=1
# #	gp = getPath()
# #	sns = strsplit(LoG,"/")[[1]]
# #	LoG = sns[2]
# #	
# 	#fp = paste(gp,"/GermplasmList/",sns[1],"/",LoG,".xlsx" ,sep="")
# 	#print(fp)
# 	data = read.xlsx2(fn,sheetName="Parameters", h=F)
# 	#print(data)
# 	sites=as.character(data[data[,1]=="tsites",2])
# 	#sts = toVector(sites)
# 	
# 	#length(sts)
# 	#gsub(";","\n",sites)
# 	sites
# }

 readTemplates = function(prefs){
     #gp = getPath()
     #fp = paste(gp,"/Templates", sep="")
     #lf = list.files(gp,pattern="template_*.xls")
     #lf
     #prefs = get.prefs()
     nTpl = prefs[prefs$pr_name=="template","pr_past"]
     nTpl = gsub(".xls","",nTpl)
     nTpl
 }
## 
## get.sel.tpl <- function(prefs){
##     nTpl = prefs[prefs$name=="template","past"]
##     nTpl = gsub(".xls","",nTpl)
##     which(nTpl==prefs[prefs$name=="template","past"])
## }


#getBaseDir <- function(){
#	path = file.path(data.dir(),ref.dir)
#	path
#}

getPath = function(){
#	season = getSeason(pref.defaults)
#	path = file.path(getBaseDir(),getCurrentCrop())
	file.path(getwd(),"res",getCurrentCrop())
}


readTplVariables = function(template , shortN=FALSE,ext=".xls"
){
	#print("Read variables")
	#shortN=T
	#ext=".xls"
	gp = getPath()
	#print(gp)
	fp = file.path(gp,paste(template,ext,sep=""))
	
	#dict = get.data.dict()
	#print(fp)
	data = read.xlsx2(fp,"Var List")
	#print(data)
#	dict[str_detect(dict$GROUP,"Yield"),"ABBR"]
	
#	
	for(i in 1:ncol(data)) data[,i]=as.character(data[,i])
	if(!shortN)	{
		#data=data[toupper(data$Select)=="X","Factor.Variables"]
		data=data[,"Factor.Variables"]
	} else {
		#data=data[toupper(data$Select)=="X","Abbreviations"]
		data=data[,"Abbreviations"]
	}
	#print(data)
	data
}


getFieldBooks = function(crop="potato"){
#getFieldBooks = function(){
	#file.path(data.dir(),ref.dir,crop,"Fieldbooks")
	file.path(get.local.db.root(),getCurrentCrop())
}

getFieldBooksPaths = function(){
	fb = getFieldBooks()
	fl = list.files(fb,pattern=".xls",r=T)
	file.path(fb,fl)
}

getFieldBooksNames = function(){
	fp = getFieldBooksPaths()
	fn = basename(fp)
	ft = NULL
	if(length(fn)!=0) {
	fs = strsplit(fn,"\\.")
	ft = fs[[1]][1]
	if(length(fs)>1){
		for(i in 2:length(fs)){
			ft = c(ft,fs[[i]][1])  
		}
	}
	} 
	ft
}

getFieldBookPath = function(fb){
	fbp=getFieldBooks(getCurrentCrop())
	afb=list.files(fbp,pattern=".xls",r=T)
	idx=grep(fb,afb)
	file.path(fbp,afb[idx])
}


getFBSeasons = function(){
	fp = getFieldBooks()
	fs = list.dirs(fp, full.name=F)
	basename(fs[-1])
}


getTrials = function(season="09A"){
	if(season=="cancel"){
		season=getFBSeasons()
	}
	
	n = length(season)
	fl=""
	for(i in 1:n){
	fp = getFieldBooks()
	fp=filename(fp)
	fp=fp[grep(season[i],fp)]
	fl = c(fl, fp)	
	}
	fl[-1]
}

guessVariableType = function(data){
	for(i in 1:ncol(data)){
		data[,i] = as.character(data[,i])
		data[,i] = gsub("ERROR","NA",data[,i])
	}
	write.csv(data,"res/temp/temp-data.csv",row.names=F)	
	read.csv("res/temp/temp-data.csv", stringsAsFactors=F)
}

getFBData = function(fieldbook, crop="potato"){
	#fpath = getFieldBooks(crop)
	#fn = expandListName2FBPath(fpath,fieldbook, ext=".xls")
	#fn = getFieldBookPath(fieldbook)
	# read excel
	fn = getFieldBookPath(fieldbook)
	if(length(fn)==1){
		data = read.xlsx2(fn,sheetName="Fieldbook", h=T)
		data = guessVariableType(data)
	} else {
		data = NULL
	}
	data
}

getGenotypes = function(trials, crop="potato" ){
	geno = ""
	if(trials==""){
		geno="none"
	} else{
		
		for(i in 1:length(trials)){
			#print(trials[i])
			data = getFBData(trials[i])
			if(!is.null(data)){
				geno = c(geno, data[,"INSTN"])	
			} 
		}
		
		geno = sort(unique(geno))
		if(length(geno)>1) geno = geno[-1]
	}
	
	geno
	
}

getVariables = function(trials, crop="potato"){
	vars=""
	for(i in 1:length(trials)){
		data = getFBData(trials[i],crop)
		vars = c(vars, names(data))
	}
	vars = unique(vars)
	vars[-1]
}

countNAvars = function(x,data) nrow(data)-sum(with(data, table(x)))

countNAgeno = function(x) ncol(data)-sum(with(data, table(x)))

hasCommentCol = function(data){
	nms=names(data)
	"Comments"%in% nms
}

filterVars = function(trials, crop, pct = 0.1){
	filt=""
	for(i in 1:length(trials)){
		#print(trials[i])
		data = getFBData(trials[i],crop)
		#environment(getFBData())
		#print(data)
		#str(data)
		thrsh = pct*nrow(data)
		for(i in 1:ncol(data)) {
			if(is.logical(data[,i])) data[,i]=as.integer(data[,i])
		}
		if(hasCommentCol(data)) data = data[,!names(data) %in% "Comments"]
		#str(data)
		#excl = (apply(data,2,countNAvars)<=thrsh)
		#print(excl)
		#nms=names(data)[ apply(data,2,countNAvars)<=thrsh]
		for(i in 1:ncol(data)) {
			if(countNAvars(data[,i], data)<=thrsh){
				filt=c(filt, names(data)[i])
			}
		}
		
		#print(nms)
		#filt = c(filt,nms)
	}
	filt = unique(filt)
	filt=filt[-1]
	#filt=filt[which(filt!="Comments")]
	#print(filt)
	filt
}

filterGeno = function(trials, vars, crop, pct = 0.5){
	i = 1
	filt=""
	for(i in 1:length(trials)){
		data = getFBData(trials[i],crop)[,vars]
		thrsh = pct*ncol(data)
		nms=data[ apply(data,1,countNAgeno)<=thrsh,1]
		filt = c(filt,nms)
	}
	filt = unique(filt)
	filt[-1]
}


combineTrialData = function(trials, vars, geno, crop ){
	n = length(trials)
	#vars = filterVars(trials, crop, pct[1])
	#geno = filterGeno(trials, vars, crop, pct[2])
	db = NULL
	for(i in 1:n){
		data = getFBData(trials[i],crop)
		#print(names(data))
		#print(vars)
		data = data[data[,1]%in%geno, vars]
		#print(data)
		ENV  = rep(trials[i],nrow(data))
		data = cbind(ENV, data)
		if(i==1){
			db = data
		} else {
			db = rbind(db, data)
		}	
	}
	
	if (sum(.packages(all.available = TRUE)=="randomForest")==0) install.packages("randomForest")
	library(randomForest)
		
	#db = db[,c(which(vars!="Comments"))]
	#print(db)
	#print(str(db))
	for(i in 1:ncol(db)){
		if(is.character(db[,i])){
			db[,i] = as.factor(db[,i])
		}
		#db[,2]=as.factor(db[,2])	
	}
	#print(str(db))
	
	
	db = rfImpute(db[,2]~., data=db)
	db[,-1]
}


compile.res = function(res, traits){
	
}


compile.pdf = function(db,report){
	
	tmp = "out/temp"
	rep = "out"
	
	write.csv(db, file.path(tmp,"database.csv"), row.names=F)
	library(pgfSweave)
	#library(R2HTML)
	rf = paste("", report, ".Rnw",sep="")
	#of = file.path(rep,paste(report,".html",sep=""))
	Sweave(rf)
	
	
	#Sweave(rf, driver=RweaveHTML, out=of, syntax="SweaveSyntaxNoweb")
	cmd = paste("pdflatex ",report, sep="")
	system(cmd)
	
	system(cmd)
	
	cmd = file.path(getwd(),paste(report,".pdf",sep=""))
	#cmd = file.path(getwd(),of)
	shell.exec(cmd)
}

get.geography = function(fieldbook, flds = c("Location","Longitude","Latitude", "Country"), crop="potato"){
	fpath = getFieldBooks(crop)
	fn = expandListName2FBPath(fpath,fieldbook, ext=".xls")
	# read excel
	data = read.xlsx2(fn,sheetName="Minimal", h=F)
	data[data[,1]%in%flds,]
}

summarize.geo = function(fieldbooks, flds = c("Location","Longitude","Latitude", "Country"), crop="potato"){
	nms = c("Fieldbook", flds)
	sg = as.data.frame(matrix(NA, ncol=length(nms), nrow=length(fieldbooks) ))
	names(sg)=nms
	for(i in 1:length(fieldbooks)){
		geo = get.geography(fieldbooks[i], flds, crop)
		sg[i,1]=fieldbooks[i]
		sg[i,2:ncol(sg)]=geo[,2]
	}
	sg
}

get.NameByCode = function(code, group){
	rsc = getResourcePath("dictionary")
	data = read.xlsx2(rsc,sheetName=group, h=T)
	data$Name.of.Variable=as.character(data$Name.of.Variable)
	data[data$Abbreviation%in%code,"Name.of.Variable"]
}

get.NameByCode2 = function(code, group, dict){
	dict$Name.of.Variable=as.character(dict$Name.of.Variable)
	dict[dict$Abbreviation%in%code,"Name.of.Variable"]
}


summarize.var.sig = function(traits, res){
	n = length(res)
	r = as.data.frame(matrix(NA, nrow=n, ncol=5))
	names(r)=c("Traits", "Bartlett test", "Sign.","Shapiro-Wilk test","Sign.")
	for(i in 1:n){
		r[i,1]=traits[i]
		r[i,2]=res[[i]]$B_test.pv
		r[i,3]=""
		if(r[i,2]<=0.1){
			r[i,3]="*"	
		}
		if(r[i,2]<0.05){
			r[i,3]="**"	
		}
		if(r[i,2]<0.01){
			r[i,3]="***"	
		}
		r[i,4]=res[[i]]$SW_test.pv
		r[i,5]=""
		if(r[i,4]<=0.1){
			r[i,5]="*"	
		}
		if(r[i,4]<0.05){
			r[i,5]="**"	
		}
		if(r[i,4]<0.01){
			r[i,5]="***"	
		}
		
	}
	r
}
#
#getAllSeasons = function(){
#		
#}

getGenotypesFromFB = function(fb){
	fbp = getFieldBookPath(fb)
	gts=""
	if(length(fbp)==1) { 
		data = read.xlsx2(fbp,sheetName="Material List", h=T)	
		gts = as.character(data[,"Institutional.number"])
	}
	return(gts)
}

get.countries = function(){
	
}


saveSheet = function(fc,fn,sheet){
	wb = loadWorkbook(fn)
	removeSheet(wb, sheet)
	saveWorkbook(wb,fn) 
	write.xlsx2(fc, fn, sheetName=sheet,append=T, row.names=F)
}

has.formula <- function(dict) {
	#has.formula = nchar(dict$FORMULA)>1
  return(!is.na(dict$AFORMULA))
}



# get.data.dict = function(terms="all",sheetName="any"){
# 	#fp = file.path(getwd(),"res","data_dictionary_yield.xls")
# 	dic=getResourceData("dictionary","Variables")
# 	nco=26
# 	if(length(terms)==1){
# 		if(terms=="all"){
# 			return(dic[,1:nco])
# 		}else{
# 			return(dic[dic$ABBR %in% terms,1:nco])
# 		}
# 		
# 	}else {
# 		return(dic[dic$ABBR %in% terms,1:nco])	
# 	}
# 	
# } 

get.templates <- function(){
	crop = getCurrentCrop()
	fp = file.path(getwd(),"res",crop)
	fl = list.files(fp, pattern="template_[A-Z]{4}.xls")
	fn = gregexpr("[A-Z]{4}",fl)
	tp=character()
	for(i in 1:length(fn)){
		s=fn[[i]][1]
		tp[i] = substr(fl[i],s,(s+3))
	}
	tp
}

get.var.fil <- function(fbt, prefs, vss, type) {
	v   = paste(fbt,type, sep="")
	its = prefs[prefs$pr_name==v,"pr_past"]
	its = strsplit(its,";")[[1]]
	its = vss %in% its
	its
}

get.site.fil <- function(cnt, prefs, ssn, type) {
	v   = cnt
	its = prefs[prefs$pr_name==v,"pr_past"]
	if(length(its)!=0){
		its = strsplit(its,";")[[1]]
		its = ssn %in% its
	} else {
		its = rep(FALSE,length(ssn))
	}
	its
}


has.data <-function(acol){
	if(length(acol)<1) return(FALSE)
	res = length(acol)!=length(which(is.na(acol)))
  res
}

get.data.only <- function(data) {
	p = which(names(data)=="INSTN")
	data = data[,(p+1):ncol(data)]
	
	abbr = get.data.dict()$ABBR
	nams = names(data)
	data = data[,nams %in% abbr]
	
	hd = rep(FALSE,ncol(data))
	for(i in 1:ncol(data)) hd[i]=has.data(data[,i])
	data = data[,hd]
	data
}

make.chart.name <- function(data, i) {
	nmd = names(data)[i]
	img = file.path("temp","charts",paste(nmd,".png",sep=""))
}


get.fb.list <-function(){
	fboks = filename(getFieldBooks(getCurrentCrop()))
	fbcur = getCurrentFB()
	fbk=fboks
	if(fbcur %in% fboks){
		fbk = fboks[!fboks %in% fbcur]
		fbk = c(fbcur,fbk)
	}
	fbk
}


create.fb.Dlg = function(w){
	dput("cancel",file=tfl)
	win <- gbasicdialog(title=title, handler = function(h,...) 
				dput(svalue(gl[1,1]),file="bin/temp.txt"), parent=w)
	gl = glayout(cont=win)
	gl[1,1]=gcombobox(get.fb.list(), cont=gl)
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	fn[[1]]
}

get.combined.fb <- function(){
	fpath = file.path(get.local.db.root(),"combined",getCurrentCrop() )
	#fpath
	fn = list.files(fpath)
	fs = strsplit(fn,"\\.")
	ft = fs[[1]][1]
	if(length(fs)>1){
		for(i in 2:length(fs)){
			ft = c(ft,fs[[i]][1])  
		}
	}
	ft
}

getCmbFieldBookPath <-function(res){
	fn = file.path(get.local.db.root(),"combined",getCurrentCrop(),
			paste(res,".xls",sep=""))
	fn
}

create.cfb.Dlg = function(w){
	dput("cancel",file=tfl)
	win <- gbasicdialog(title=title, handler = function(h,...) 
				dput(svalue(gl[1,1]),file="bin/temp.txt"), parent=w)
	gl = glayout(cont=win)
	cmbd = get.combined.fb() 
	gl[1,1]=gcombobox(cmbd, cont=gl)
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	fn[[1]]
}


baseStatus = function(){
	crop = toupper(getCurrentCrop())
	#msg = paste(crop,get.version(),sep=" | ")
	#msg
	crop
}

#
updateStatus= function(){
	msg = paste("Preferred fieldbook: ",getCurrentFB()," | Preferred crop: ",baseStatus(),sep="")
	#svalue(sb) = msg
	msg
}


scale = function(x, to= 10, from=1, precision=0){
	stopifnot(is.double(x))
	stopifnot(is.vector(x))
	r = max(x, na.rm=T)-min(x,na.rm=T)
	xx = round((x-min(x, na.rm=T))/r*(to-1),precision)+from
	xx
}

bcopy <- function(files, udir) {
	n = length(files)
	for(i in 1:n){
		fp = file.path(udir,files[i])
		bn = dirname(fp)
		bn = str_replace(bn,"src","bin") 	# Adjust for src -> bin
		print(bn)
		if(!file.exists(bn)) dir.create(bn, rec=T)
		print(paste(files[i], bn))
		#file.copy(files[i], bn, over=T, rec=T)
		file.copy(files[i], bn, rec=T)
	}
}

close.all.excel = function(){
	gmessage("Please save and close open Excel files - they will be closed now without saving.", icon="warning")
	cmd = "TSKILL excel /A"
	shell(cmd)
}



get.short.msiten <- function(out) {
	msite=NULL
	if(out$sdesign=="Mother & Baby CRD (MBCRD)") {
		msite = strsplit(out$msite," \\(")[[1]][1]
	}
	msite
}


get.short.siten <- function(out) {
	n1=length(out$tsites)
	sitea=character(n1)
	for(i in 1:n1){
		sitea[i] = strsplit(out$tsites[i]," \\(")[[1]][1]
	}
	msite = get.short.msiten(out)
	sitea = unique(c(sitea,msite))
	sitea
}

is.strdate <- function(x){
  res = NA
  if(!is.character(x)) return(FALSE)
  if(!str_length(x)==10) return(FALSE)
  r = str_extract(x,"[0-9]{4}-[0-9]{2}-[0-9]{2}")
  if(!is.na(r)){
  try({
    yy = as.integer(str_sub(r,1,4))
    mm = as.integer(str_sub(r,6,7))
    dd = as.integer(str_sub(r,9,10))
    res = mdy.date(mm,dd,yy)
  })  
  }
  if(is.na(res)) {
    return(FALSE)
  } else return(TRUE)
}

# http://stackoverflow.com/questions/8343509/better-error-message-for-stopifnot
assert <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}



