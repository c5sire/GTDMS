###############################################################################
#
# TODO: Add comment
#
# Oct 14, 2011
# 3:44:39 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

check2txt <-function(check){
	#print(check)
	if(length(check)==0) return("ERROR")
	#if(is.null(check)) return("ERROR")
	#if(is.na(check)) return("ERROR")
	if(check) return("ok")
	return("ERROR")
}

get.check.log.path <- function() {
	fn = file.path(getwd(),"temp","check.log.txt")
}

check.log <- function(comp,rule, check, app=T){
	
	fn = get.check.log.path()
	ln = paste(comp,"\t",rule,":\t",check2txt(check), sep="")
	
	if(!app){
		write("Component\tRule\tResult",fn,app=F)
	}
	write(ln,fn,app=T)
	check
}

get.ref.sheet <-function(rp, asheet){
	#fp = file.path(getwd(),"res","potato","template_PTLB.xls")
	#wb = loadWorkbook(fp)
	#sheets = getSheets(wb)
	#sheets[[asheet]]
	dt = read.xlsx(rp,sheetName=asheet,stringsAsFactors=F)
	#for(i in 1:ncol(dt)) dt[,i]=as.character(dt[,i])
	dt
}


is.Excel <- function(fp) {
	res = try( 	loadWorkbook(fp), silent=T )
	#em=geterrmessage()
	#res = str_detect(res,"Java")
	ref = class(res)[1]
	res = str_detect(ref,"jobjRef")
	check.log("file","Is this an Excel file",res,FALSE)
}

get.tplid <- function(fp) {
	mini = read.xlsx2(fp,sheetName="Minimal")
	aname = mini[mini$Factor=="Short name or Title",2]
	acrop = mini[mini$Factor=="Crop",2]
	tplid = str_sub(aname,1,4)
	#print(tplid)
	tplid = file.path(getwd(),"res",acrop,paste("template_",tplid,".xls",sep=""))
	tplid
}


has.Minimal <-function(fp){
#	mini = sheets[["Minimal"]]
#	res=!is.null(mini)
#	check.log("Sheet","File has sheet called 'Minimal'",res)
	
	#get template indicator
	mini = read.xlsx2(fp,sheetName="Minimal")
	em=geterrmessage()
	res = str_detect(em,"Error")
	x = check.log("Sheet","File has sheet called 'Minimal'",res)
	for(i in 1:ncol(mini)) mini[,i]=as.character(mini[,i])
	
	#mini = read.xlsx2(fp,sheetName="Minimal")
	tp = get.tplid(fp)
	#print(tp)
	res = try( 	loadWorkbook(tp) )
	em=geterrmessage()
	res = str_detect(em,"Error")
	#print(res)
	
	x = check.log("Title","File references known template",res)
	rb = NULL
	if(res) rb = loadWorkbook(tp)
}

compare.cell <- function(anew, aref, element, comp,lbl){
	res = anew %in% aref
	rule = paste("Validated the ",lbl," '",anew,"'",sep="")
	check.log(comp,rule,res)
}

compare.cells <-function(ahdr,rhdr,element, comp, lbl){
	missing = rhdr[!rhdr %in% ahdr]
	#remove optional sheets
	
	#missing = missing[!optional %in% missing]
	#print(missing)
	
	if(length(missing)>0){
	for(i in 1:length(missing)){
		rule = paste("Expected the ",lbl," '",missing[i],"'",sep="")
		x=check.log(comp, rule,FALSE)
	}
	}
	present = rhdr[rhdr %in% ahdr]
	for(i in 1:length(present)){
		rule = paste("Expected the ",lbl," '",present[i],"'",sep="")
		x=check.log(comp, rule,TRUE)
	}
	length(missing)==0
}

check.version<-function(mini){
	avar = "Version"
	anew = mini[mini$Factor==avar,2]
	aref = "V.2.1.0"
	compare.cell(anew, aref,avar,"Minimal","value")
}

check.crop <- function(mini){
	avar = "Crop"
	anew = mini[mini$Factor==avar,2]
	#aref = c("potato", "sweetpotato","ahipa") #TODO replace with more flexible
	crops = get.list.of.registered.crops(get.prefs())
	aref = as.character(crops)
	abbr = names(crops)
	compare.cell(anew, aref,avar,"Minimal","value")
	
	res=FALSE
	cropid = substr(mini[mini$Factor=="Short name or Title",2],1,2)
	
	if(crops[[cropid]]==anew) res = TRUE
	
#	if(cropid=="PT" & anew=="potato") res=TRUE
#	if(cropid=="SP" & anew=="sweetpotato") res=TRUE
#	if(cropid=="AH" & anew=="ahipa") res=TRUE
	
	comp="Minimal"
	element=avar
	lbl="value"
	rule = paste("Crosschecked in '",element,"' the ",lbl," '",anew,"'",sep="")
	check.log(comp,rule,res)
}

check.type <- function(mini){
	avar = "Type of Trial"
	anew = mini[mini$Factor==avar,2]
	aref = c("late blight", "yield", "dormancy") #TODO replace with more flexible 
	compare.cell(anew, aref,avar,"Minimal","value")
	
	res=FALSE
	typeid = substr(mini[mini$Factor=="Short name or Title",2],3,4)
	if(typeid=="YL" & anew=="yield") res=TRUE
	if(typeid=="LB" & anew=="late blight") res=TRUE
	if(typeid=="DS" & anew=="dormancy") res=TRUE
	#if(cropid=="ST" & anew=="stability") res=TRUE
	
	comp="Minimal"
	element=avar
	lbl="value"
	rule = paste("Crosschecked the ",lbl," '",anew,"'",sep="")
	check.log(comp,rule,res)
}

jul.date <- function(y,m,d){
	y = as.integer(y)
	m = as.integer(m)
	d = as.integer(d)
	mdy.date(m,d,y)
}

get.date.format <- function() {
	ptrn="([0-9]{4})-([0-9]){2}-([0-9]){2}"
}

is.valid.date<-function(adate, fmt){
	length(grep(fmt,adate))>0
}

check.dates <- function(mini){
	ptrn=get.date.format()
	comp="Minimal"
	
	lbl="value"

	check.adate <- function(avar, mini, ptrn, comp, lbl) {
		element=avar
		anew = mini[mini$Factor==avar,2]
		res = is.valid.date(anew, ptrn)	#length(grep(ptrn,anew))>0
		rule = paste("Validated for '",element,"' the ",lbl," '",anew,"'",sep="")
		check.log(comp,rule,res)
	}
	
	begd = check.adate("Begin date", mini = mini, ptrn = ptrn, comp = comp, lbl = lbl)
	endd = check.adate("End date", mini = mini, ptrn = ptrn, comp = comp, lbl = lbl)
	embd = check.adate("Embargo till", mini = mini, ptrn = ptrn, comp = comp, lbl = lbl)

	idck = FALSE
	if(begd){
		yearid = substr(mini[mini$Factor=="Short name or Title",2],5,8)
		montid = substr(mini[mini$Factor=="Short name or Title",2],9,10)
		anew = as.character(mini[mini$Factor=="Begin date",2])
		yearnw = substr(anew,1,4)
		montnw = substr(anew,6,7)
		adaynw = substr(anew,9,10)
		res = (yearid==yearnw) & (montid==montnw)
		rule = paste("Cross-checked in ",comp," '","Begin date","' the ",lbl," '",anew,"'",sep="")
		abeg = jul.date(yearnw, montnw, adaynw)
		idck = check.log(comp,rule,res)
	}
	if(endd){
		yeared = substr(mini[mini$Factor=="End date",2],1,4)
		monted = substr(mini[mini$Factor=="End date",2],6,7)
		adayed = substr(mini[mini$Factor=="End date",2],9,10)
		aend = jul.date(yeared, monted, adayed)
	}
	if(embd){
		yearem = substr(mini[mini$Factor=="Embargo till",2],1,4)
		montem = substr(mini[mini$Factor=="Embargo till",2],6,7)
		adayem = substr(mini[mini$Factor=="Embargo till",2],9,10)
		aemb = jul.date(yearem, montem, adayem)
	}
	
	if(begd & endd){
		res = aend>abeg
		rule = paste("Date range validated between ",comp," '","'Begin date' and 'End date'","' the ",lbl," '",anew,"'",sep="")
		check.log(comp,rule,res)
	}
	if(endd & embd){
		res = aemb>=aend
		rule = paste("Date range validated between ",comp," '","'Embargo till' and 'End date'","' the ",lbl," '",anew,"'",sep="")
		check.log(comp,rule,res)
	}
	
	begd & endd & embd & idck
}

check.site <- function(mini){
	ptrn="([A-Z]{3,6})"
	comp="Minimal"
	lbl="value"
	avar="Site short name"
	element=avar
	anew = as.character(mini[mini$Factor==avar,2])
	res = length(grep(ptrn,anew))>0	
	rule = paste("Validated for '",element,"' the ",lbl," '",anew,"'",sep="")
	check.log(comp,rule,res)
	
	#Cross check with ID
	id = as.character(mini[mini$Factor=="Short name or Title",2])
	siteid = substr(id,12,nchar(id))
	res = anew == siteid
	rule = paste("'",avar, "' cross checked for the ",lbl," '",anew,"'",sep="")
	check.log(comp,rule,res)
	
	#Cross check present in site index
	site <- get.site.details(siteid)
	res.site = site$SHORTN == siteid
	rule = paste("'",anew, "' listed in site index.",sep="")
	check.log(comp,rule,res.site)
	
	#Other fields have valid information?
	cipr = as.character(mini[mini$Factor=="CIP Region",2])
	cont = as.character(mini[mini$Factor=="Continent",2])
	cntr = as.character(mini[mini$Factor=="Country",2])
	adm1 = as.character(mini[mini$Factor=="Admin1",2])
	adm2 = as.character(mini[mini$Factor=="Admin2",2])
	adm3 = as.character(mini[mini$Factor=="Admin3",2])
	adm4 = as.character(mini[mini$Factor=="Admin4",2])
	lclt = as.character(mini[mini$Factor=="Locality",2])
	elev = as.character(mini[mini$Factor=="Elevation",2])
	lati = as.character(mini[mini$Factor=="Latitude",2])
	long = as.character(mini[mini$Factor=="Longitude",2])
	
	
	#if(!res.site){
		#If so: register in site index (so next time it will be there
		#TODO: append in archive! (read whole worksheet and save)
		#TODO: separate pref dialog!
	#}
	
	
	
}

get.codes <- function(dict, tn){
	codes = dict[dict$ABBR==tn,"STATES"]
	if(!is.na(codes)){
		codes = str_trim(str_split(codes,";")[[1]])
		cds = NULL
		for(i in 1:length(codes)){
		 cds = c(cds, str_trim(str_split(codes[i],"=")[[1]][1]))	
		}
		codes = cds		
	} 
	as.integer(codes)
}

set.cell <- function(rule, row, cidx, rows, cs) {
#	rows=getRows(sheet)
	ridx=row+1
	cell = getCells(rows[ridx],cidx)[[1]]
	cv = getCellValue(cell)
	#print(cv)
	#cell <- createCell(rows[ridx], colIndex=cidx)[[1,1]]
	#cmt = getCellComment(cell)
	#cmt = paste(cmt,"\n",rule,sep="")
	cmt=paste(rule,": ERROR",sep="")
	#print(paste("RS:",rule,row,cidx))
	createCellComment(cell, string=cmt, author=get.version())
	
	setCellStyle(cell, cs)
}


mark.error <-function(j, cidx, rule, rows, styles){
	cs = styles$error
	set.cell(rule, j, cidx, rows, cs  )
}


test.num.var.rule <- function(col, tn, lwr, upr, rows, cidx, styles){
  col = col[!is.na(col)]
  test.range = !(lwr <= col & col <= upr)
  rule=NULL
  if(any(test.range==TRUE)){
    idx = which(test.range)
    res = col[test.range] 
    
    rule = paste("In column '",tn,"' (row ",idx+1,") the value '",res,
                 "' is NOT between '",lwr,"' and '",upr,"'",sep="")
    if(!is.null(rule)){
      check.log("Fieldbook",rule,NULL)
#       cells = getCells(rows)
#       ind = paste((idx+1),".",cidx, sep="")
#       mapply(createCellComment, cells[ind], rule)
#       sapply(cells[ind], setCellStyle, styles$warn)
    }
  }
  rule
}

test.fac.var.rule <- function(col, tn, cds){
  col = col[!is.na(col)]
  test.range = !(col %in% cds)
  rule=NULL
  if(any(test.range==TRUE)){
    idx = which(test.range)
    res = col[test.range] 
    vls = paste(cds,collapse=", ")
    rule = paste("In column '",tn,"' (row ",idx+1,") the value '",res,
                 "' is NOT one of '",vls,"'",sep="")
    if(!is.null(rule)){
      check.log("Fieldbook",rule,NULL)
    }
  }
  rule
}

test.date.var.rule <- function(col, tn){
  col = col[!is.na(col)]
  test.range = !sapply(col,is.strdate)
  rule=NULL
  if(any(test.range==TRUE)){
    idx = which(test.range)
    res = col[test.range] 
    rule = paste("In column '",tn,"' (row ",idx+1,") the value '",res,
                 "' is NOT a valid date",sep="")
    if(!is.null(rule)){
      check.log("Fieldbook",rule,NULL)
    }
  }
  rule
}


check.variable <- function(col, dict, tn, rows=NULL, styles=NULL, cidx=NULL) {
	if(has.data(col)){
		if(is.numeric(col)){
			lwr = as.numeric(dict[dict$ABBR==tn,"LOWER"])
			upr = as.numeric(dict[dict$ABBR==tn,"UPPER"])
			cds = get.codes(dict,tn)
      
			if(!(is.na(lwr) & is.na(upr))){
			  test.num.var.rule(col, tn, lwr, upr, rows=rows, cidx = cidx, styles = styles)
			}
			if(!(is.na(cds))){
			  test.fac.var.rule(col, tn, cds)
			}
      
		}
    if(is.character(col)){
      typ = dict[dict$ABBR==tn,"TYPE"]
      if(typ=="Date"){
        test.date.var.rule(col, tn)
      }
    }
    # TODO: handle here variables of text: factors and date
	}
}




set.pb <- function(pb, ttl, msg,val){
	if(!is.null(pb)) setWinProgressBar(pb, val, title = ttl, label = msg)
}

process.sheet <- function(asheet, nsi, dict, styles, varl, rp, fp, wb, mini, dfmt, pb) {
	rows = getRows(asheet) #Causes problems with sheet field layout!
	#asht = try(read.xlsx(fp,sheetName=nshets[i],stringsAsFactors=FALSE),TRUE)
  
	asht = try(get.sheet.data(asheet))
	data = asht
	if(class(data)!="try-error"){
		ahdr = names(data)
		
		if(nsi=="Field_layout"){
		}
		if(nsi=="Fieldbook" | nsi=="Fieldbook2"){
			data = guessVariableType(data)
			#print("3")
			xhdr = c("PLOT","REP","CBLOCK","FACTOR","INSTN")
			yhdr = c(xhdr,dict$ABBR)
			test = ahdr[ahdr %in% yhdr]
			vvrs = !test %in% xhdr # valid variables
			for(t in 1:length(test)){
				rule = paste("Validated the column label '",test[t],"'",sep="")
				set.pb(pb,"Checking format",val=t, msg=paste("Checking sheet: Fieldbook", ":", test[t]))
				x=check.log(nsi,rule,TRUE)
				if(vvrs[t]){
					tn = test[t]
					col = data[,tn]
					cidx = which(ahdr %in% tn)
					check.variable(col = col, dict = dict, tn = tn, rows=rows, styles, cidx)
				}
			}
			vl = varl[,2]
			tv = test[vvrs]
			xx = tv %in% vl
			res=all(xx)
			
			rule = paste("Validated that all fieldbook variables are also in the 'Var List' sheet",sep="")
			if(!res){
				rule = paste("Missing in 'Var List' the variable(s) ", paste(tv[!xx],collapse=", "), 
						" from the fieldbook")
			}
			
			x=check.log(nsi,rule,res)
			
			test = ahdr[!ahdr %in% yhdr]
			if(length(test)>0){
				for(t in 1:length(test)){
					rule = paste("Validated the column label '",test[t],"'",sep="")
					x=check.log(nsi,rule,FALSE)
				}
				
			}
			
			
		} else {
			rsht = get.ref.sheet(rp,nsi)
			rhdr = names(rsht)
			#print(rhdr)
		}
		
		
		
		#Optional sheets
		if(	nsi=="Soil_analysis" | 
				nsi=="Weather_data" | nsi=="Hobo_data"  |
				nsi=="Field_layout"){			
		}
		if(nsi=="Fieldbook2"){
			x=try(log.check.fb(fp,wb))
		}
		
		if(!(nsi=="Minimal" | nsi=="Installation" 
					| nsi=="Fieldbook" | nsi=="Fieldbook2" )){
			#print(ahdr)
			x=compare.cells(ahdr, rhdr, nsi,nsi,"column label")
		}
		
		if(nsi=="Minimal"){
			rlbl = rsht[,"Factor"]
			albl = asht[,"Factor"]
			x=compare.cells(albl, rlbl, nsi,nsi,"row label")
			
			#Check special conditions
			#mini = read.xlsx2(fp,sheetName="Minimal")
			#mini = data
			check.version(mini)
			check.crop(mini)
			check.type(mini)
			check.dates(mini)
			check.site(mini)
			
		}
		if(nsi=="Installation"){
			rlbl = rsht[,"Factor"]
			albl = asht[,"Factor"]
			x=compare.cells(albl, rlbl, nsi,nsi,"row label")
		}
		if(nsi=="Crop_management"){
			#mdt = as.character(read.xlsx2(fp,sheetName="Crop_management")$Date)
			mdt = data$Date
			for(j in 1:length(mdt)){
				res = is.valid.date(mdt[j], dfmt)
				rule = paste("Validated in row '",(j+1),"' the 'Date' value '",mdt[j],"'",sep="")
				#print(rule)
				x=check.log("Crop_management",rule,res)
			}
			#print(j)
			#print("point 2")
		}
	}
}


check.format <- function(fp, pb=NULL, value=0){
	# Can you really open the file as an Excel archive?
#tryCatch(
if(is.Excel(fp)){
	pbt = "Checking format"
	dfmt = get.date.format()
	try(set.pb(pb,pbt,val=value, msg="Preparing for format check."))
	dict = get.data.dict()
	wb = loadWorkbook(fp)
	sheets = getSheets(wb)
	styles = get.cell.styles(wb)
	ashets = names(sheets)
	rb = has.Minimal(fp)
	stopifnot(!is.null(rb))
	
	rp = get.tplid(fp)
	#print(rp)
	rshets = c(names(getSheets(rb)),"Fieldbook","Fieldbook2")
	rshets = ashets[ashets%in%rshets]
	x=try(compare.cells(ashets, rshets, basename(fp),basename(fp),"sheet label"))
	
	nshets = rshets[rshets %in% ashets]
	nshets = sort(nshets)
	
	varl = try(get.sheet.data(sheets[["Var List"]], "Var List"))
	mini = try(get.sheet.data(sheets[["Minimal"]], "Minimal"))
	#print(nshets)

	# Remove for the time being the sheet Field_layout
	nshets = nshets[!(nshets %in% "Field_layout")]
	
	for(i in 1:length(nshets)){
		#gc(F)
		set.pb(pb,pbt,val=value, msg=paste("Checking sheet:",nshets[i]))
		asheet = sheets[[nshets[i]]]
		nsi  = nshets[i]
		if(is.null(asheet)) break 
      #print(nshets[i])
      #print(nsi)
   	process.sheet(asheet = asheet, nsi = nsi, dict = dict, styles = styles, 
				varl = varl, rp = rp, fp = fp, wb = wb, mini = mini, dfmt = dfmt, pb= pb)
	}

	clp = get.check.log.path()
	clf = read.csv(clp, sep="\t")
	errs=clf[clf$Result=="ERROR",]
  if(nrow(errs)>0){
	#cld = rbind(errs, clf[clf$Result!="ERROR",])
  cld = errs # report only ERRORS
	#print("point 1")
	#add.sheet(fp = fp, mtrx = cld, sh="Format checks")
	sh = "Format checks"
	clear.sheet( sh, fp)
	wb = loadWorkbook(fp)
	#sheets = getSheets(wb)
	#removeSheet(wb, sh)
	#saveWorkbook(wb,fp)
	#write.xlsx2(cld, fp, sheetName=sh)
	write.xlsx2(cld, fp, sheetName=sh, formatTemplate=NULL,
		col.names=TRUE, row.names=FALSE, append=TRUE)
	#}, return(FALSE))
	#autoSizeColumn(sheet)
	#TODO make reproducible report
	#print("point 1")
  }
	rm(wb,data,pb, varl, mini)
	#gc(verb=FALSE)
	if(nrow(errs)>0) return(FALSE)
	return(TRUE)	
} 
	return(FALSE)
}
