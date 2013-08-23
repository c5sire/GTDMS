###############################################################################
#
# 
#
# Sep 22, 2011
# 7:59:34 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

add.fieldbook.listing <- function(data, pgc, h){
	n = nrow(data)
	lbl = paste(translate("_FIELDBOOKS_")," (n = ",n,")",sep="") #GTDM-405
	#lbl = translate("_FIELDBOOKS_")
	msg = lbl
	tbl <- gtable(data, multiple=TRUE, height=h,width=700,cont=pgc, expand=T, label=lbl)
	#title(pgc)=lbl
	#names(pgc)[1]=lbl
	tbl
}

add.crop.filter <- function(pgl, gwidth){
	#crops=c("potato")
	crops = get.crops()
	gcr="none"
	if(length(crops)<=1){
		lbl = paste(translate("_CROP_"),": ",toupper(translate(crops)),sep="")
		gcr <- glabel(lbl, cont=pgl)
	} else {
		gcr <- gexpandgroup(translate("_CROPS_"),cont=pgl, width=gwidth)	
		fcr <- gcheckboxgroup(crops,checked=T, cont=fcr)
	}
	gcr
}

add.years.filter <- function(pgl, gwidth, fyr=NULL){
	years=get.years()
	fyr=NULL
	gyr=translate("_NONE_")
	if(length(years)<=1){
		lbl = paste(translate("_PLANTING_YEAR_"),": ",years,sep="")
		gyr <- glabel(lbl, cont=pgl)
	} else {
		fyr <- gexpandgroup(translate("_PLANTING_YEARS_"),cont=pgl, width=gwidth)	
		gyr <- gcheckboxgroup(years,checked=T, cont=fyr)
	}
	#list(gyr=gyr, fyr=fyr)
	gyr
}

add.country.filter <- function(pgl, gwidth){
	countries=get.countries()
	gct=translate("_NONE_")
	if(length(countries)<=1){
		lbl = paste(translate("_COUNTRY_"), ": ",countries,sep="")
		gct <- glabel(lbl, cont=pgl)
	} else {
		gct <- gexpandgroup(translate("_COUNTRIES_") ,cont=pgl, width=gwidth)	
		fct <- gcheckboxgroup(countries,checked=T, cont=gct)
	}
	
}

add.trial.types.filter <- function(pgl, gwidth){
	trial.types=get.trial.types()
	gty=translate("_NONE_")
	if(length(trial.types)<=1){
		lbl = paste(translate("_TRIAL_TYPE_"), ": ",trial.types,sep="")
		gty<- glabel(lbl, cont=pgl)
	} else {
		fty <- gexpandgroup(translate("_TRIAL_TYPES_"),cont=pgl, width=gwidth)	
		gty <- gcheckboxgroup(trial.types,checked=T, cont=fty)
	}
	gty
}

get.left.pane <- function(dkt, lwidth, theight) {
	pgl <- gframe(translate("_FILTER_LBL_"),horizontal=FALSE, cont=dkt, width=lwidth,height=theight)
	#pgl <- ggroup(horizontal=FALSE, use.scroll=T,cont=dkt, width=lwidth,height=theight)
	pgl
}

get.central.pane <- function(lp, dkt) {
	pgc <- gnotebook(horizontal=FALSE, width=900,height=lp$theight, cont=dkt)
	pgc
}


get.layout.params <- function() {
	h = 600
	h2= h/2
	w =300
	b = 12
	wb= w-b
			
lp = list(			
	lwidth = w,
	theight= h,
	height = h2,
	border = b,
	gwidth = wb
)
lp
}

####Global variabls
#define inner functions
lst.excel=list("X"=list(patterns=c(".xls")))
names(lst.excel)[[1]]=translate("_LBL_EXCEL_")

checkin.files <- function(w) gfile(translate("_MSG_SELECT_EXCEL_"),type="open",  
				filter=lst.excel, multi=TRUE,
			action="checkin.form", handler = function(h,...) {
				do.call(h$action,list(h$file,w))
				
			}
			)


#do.layout <-function(w,tbl,fcr,fyr,fct,fty,pgl, gyr){
#g = NULL

#dkt=NULL
lp = NULL
tbl= NULL
pgc = NULL
do.layout <-function(w){	
	data = get.fieldbooks()
	
	g <- ggroup(cont=g2, horizontal=TRUE)
	
	# define the principal layout
#	if(length(find("dkt"))==1 & class(dkt)[[1]]=="gPanedGroup"){
#		#print("Dispose g.")
#		delete(g,dkt)
#	}
	dkt <- gpanedgroup(cont = g)
	#add(g,dkt=gpanedgroup(), expand=T)
	lp = get.layout.params()
	pgl <- get.left.pane(dkt = dkt, lwidth = lp$lwidth, theight = lp$theight)
	pgc <- get.central.pane(lp = lp, dkt = dkt)
	
	#fcr <- add.crop.filter(pgl,lp$gwidth)
	fyr <- add.years.filter(pgl,lp$gwidth)
	fct <- add.country.filter(pgl,lp$gwidth)
	fty <- add.trial.types.filter(pgl,lp$gwidth)
	tbl <- add.fieldbook.listing(data, pgc, lp$height)
	#cl <- glabel("No updates in this session.",label="Checkin log", cont=pgc)
	
	update.table <-function(...){
		#tbl <- update.table.data(fcr = fcr, fyr = fyr, fct = fct, fty = fty, data = data)
		#crops  = svalue(fcr)
		#print(crops)
		#if(length(crops)<1) crops="none" 
		#if(str_detect(crops,":")) crops=tolower(str_split(crops,": ")[[1]][2])
		years = svalue(fyr)
		if(length(years)<1) years="0" 
		if(str_detect(years,":")) years=str_split(years,": ")[[1]][2]
		cntrs = svalue(fct)
		#print(cntrs)
		if(length(cntrs)<1) cntrs="none"
		if(str_detect(cntrs,":")) cntrs=capitalise(str_split(cntrs,": ")[[1]][2])
		#print(cntrs)
		trtps = svalue(fty)
		if(length(trtps)<1) trtps="none" 
		if(str_detect(trtps,":")) trtps=str_split(trtps,": ")[[1]][2]
		#trphs = svalue(ftp)
		#if(length(trphs)<1) trphs="none" 
		#f(str_detect(trphs,":")) trphs=str_split(trphs,": ")[[1]][2]
		
		#lbl = get.meta.label(c("planting_year","country","trial_type","crop"))
		lbl = translate(c("_PLANTING_YEAR_","_COUNTRY_","_TRIAL_TYPE_","_CROP_"))
		#lbl = c("Planting year","Country","Trial type","Crop")	
	
#		ind = with(data, planting_year %in% years 
#						& country %in% cntrs  
#						& trial_type %in% trtps 
#						& trial_phase %in% trphs
#						& crop %in% crops
#		)
		#print(years)
		#print(cntrs)
		
		if(years!="0"){
			data = get.fieldbooks()
			ind = with(data,  data[,lbl[1]] %in% years 
							& data[,lbl[2]] %in% cntrs  
							& data[,lbl[3]] %in% trtps 
							#& data[,lbl[4]] %in% crops
			)
			#print(ind)
			data.cur <<- data[ind,]
		} else {
			data.cur <<-data
		}
		tbl[,]<-data.cur
		
	}
	#set.tbl(tbl)
	#sapply(list(fcr,fyr,fct,fty,ftp), addHandlerClicked, handler=update.table)
	#sapply(list(fcr,fyr,fct,fty), addHandlerClicked, handler=update.table)	
	sapply(list(fyr,fct,fty), addHandlerClicked, handler=update.table)
	
	
	handler.checkin <-function(h, ...){
		#print("Test this")
		checkin.files(w)
	}
	
	handler.checkout <-function(h, ...){
		#print("Hello")
		#print(svalue(tbl))
		fn = svalue(tbl)
		gfile("Checkout",type="save")  
#				filter=list("Excel forms"=list(patterns=c(".xls"))), multi=TRUE,
				
	}
	
	handler.edit <-function(h, ...){
		#gdf(get.fieldbooks(),cont=pgc)  
		gmessage("TODO")
	}
	
	lst = list()
	lst$'Checkin'$handler = handler.checkin
	#lst$'Checkout'$handler = handler.checkout
	#lst$'View single analysis'$handler = function(h,...) {}
	#lst$'Edit'$handler = handler.edit
	add3rdmousepopupmenu(tbl, lst) 
 	
	#list(w=w,tbl=tbl,fcr = fcr,	fyr = fyr,fct = fct, fty = fty, pgl=pgl, gyr=gyr )
	w
}

title= paste("DataCollector",sep="")
title = paste(title," ",get.version.nr()," | ",toupper(translate(getCurrentCrop()))," | ",translate("_REPOSITORY_"),": ",get.local.db.root(),sep="")
w <- xgwindow(title, visible=F)
g2 <- ggroup(cont=w, horizontal=FALSE)


gui <- function(title=title, w=w){
  data = get.fieldbooks()
	synchronizeFilesWithDb(data)
	
	#data = get.fieldbooks()
	tbl <- add.fieldbook.listing(data, pgc, lp$height)
	visible(w) <- FALSE
	#w <- do.layout(w)
	w = do.layout(w)
	get.menu()
	gmenu(mlist, cont = w)
	visible(w) <- TRUE
	w
}

refresh <- function(w=w) {
	visible(w)<-FALSE
	dispose(w)
	source(file.path(app.dir,"gui","gui.R"))
	
}

#synchronizeFilesWithDb()
w=gui(title,w)
#refresh(w)


