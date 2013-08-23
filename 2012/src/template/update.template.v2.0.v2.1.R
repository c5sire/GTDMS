###############################################################################
#
# TODO: Add comment
#
# May 22, 2012
# 1:41:13 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


update.template.v2.0.v2.1 <-function(dp){
	if(!file.info(dp)$isdir) dp = basename(dp)
	fps = list.files(dp,all.files=T,rec=T,full=T,pattern="*.xls")

	getTemplateVersion <- function(fp){
		xl = try(read.excel(fp,"Minimal"))
		if(class(xl)=="character") return("ERROR")
		xl[xl[,1]=="Version",2]
	}

	update.template <- function(fps, i) {
		fp = fps[i]
		tv = getTemplateVersion(fp)
		if(tv>="V.2.0.0"){
			#print("Do something")
			wb = loadWorkbook(fp)
			sheets <- getSheets(wb)
			minimal=sheets[["Minimal"]]
			r = minimal$getLastRowNum()+2
			#print(r)
			if(r==36){
				createRow(minimal,r:(r+3))
			}
			if(r>=40){
				r = 36
			}
			csl= get.cell.styles(wb)
			cs = csl$header
			set.cell.val("Donor",minimal,wb,1,r, cellStyle = cs)
			set.cell.val("Project name",minimal,wb,1,r+1, cellStyle = cs)
			set.cell.val("Project start",minimal,wb,1,r+2, cellStyle = cs)
			set.cell.val("Project end",minimal,wb,1,r+3, cellStyle = cs)
			r = get.row.by.name("Version",minimal)
			cs = csl$form
			set.cell.val("V.2.1.0",minimal,wb,2,r, cellStyle = cs)
		}
		saveWorkbook(wb,fp)
		"OK"
	}
	
	#i = 1
	xmin=1
	i=1
	xmax=length(fps)
	pb <- winProgressBar("Updating template file ...", "File ... %",
			xmin, xmax, xmin)	
	
	
	report = character(length(fps))
	for(i in 1:length(fps)){
		info <- sprintf(paste("%d%%","done"), round(i/xmax*100))
		#setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_LOADING_"),"(%s)"), info), package)
		setWinProgressBar(pb, i, sprintf(paste("Updatinge file ","(%s)"), info), basename(fps[i]))
		
		err=try(update.template(fps = fps, i = i))
		if(str_detect(err,"Error")) {
			report[i]=paste(basename(fps[i]),"ERROR")
		} else {
			report[i]=paste(basename(fps[i]),"OK")
		}
	}
	print(report)
	close(pb)
}

update.template.dlg <- function(w){
	owd = getwd()
	setwd(get.local.db.root())
	dp=try(gfile(type="selectdir"))
	setwd(owd)
	if(!is.na(dp)){
		closeAllExcel()
		update.template.v2.0.v2.1(dp)
	}	
}

#dp = "L:\\packages\\GDET4RT\\new2"
#
#update.template.v2.0.v2.1(dp)
