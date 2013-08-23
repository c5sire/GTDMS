###############################################################################
#
# TODO: Add comment
#
# Oct 4, 2011
# 1:31:12 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################



get.site.details <- function(site){
	sts = getResourceData('sites','Sites')
	sts[sts$SHORTN==site,]
}

add.msite.data <- function(is.ms, wb){
	sheets <- getSheets(wb)
	sheet <- sheets[["Minimal"]]
	csl = get.cell.styles(wb)
	cs = csl$number_col1

	siten = "'Baby' site in a 'Mother-Baby' trial"
	if(is.ms) siten = "'Mother' site in a 'Mother-Baby' trial"
	set.cell.value(siten,"Comments",sheet,wb, cellStyle=cs)
}


add.site.data <- function(siten,wb){
	sheets <- getSheets(wb)
	sheet <- sheets[["Minimal"]]
	site <- get.site.details(siten)
	csl = get.cell.styles(wb)
	cs = csl$number_col1
	
	set.cell.value(site["SHORTN"][[1]],"Site short name",sheet,wb, cellStyle=cs)
	set.cell.value(site["CONT"][[1]],"Continent",sheet,wb, cellStyle=cs)
	set.cell.value(site["CREG"][[1]],"CIP Region",sheet,wb, cellStyle=cs)
	set.cell.value(site["CNTRY"][[1]],"Country",sheet,wb, cellStyle=cs)
	set.cell.value(site["ADM1"][[1]],"Admin1",sheet,wb, cellStyle=cs)
	set.cell.value(site["ADM2"][[1]],"Admin2",sheet,wb, cellStyle=cs)
	set.cell.value(site["ADM3"][[1]],"Admin3",sheet,wb, cellStyle=cs)
	set.cell.value(site["LOCAL"][[1]],"Locality",sheet,wb, cellStyle=cs)
	set.cell.value(site["LATD"][[1]],"Latitude",sheet,wb,cellStyle=cs)
	set.cell.value(site["LOND"][[1]],"Longitude",sheet,wb,cellStyle=cs)
	set.cell.value(site["ELEV"][[1]],"Elevation",sheet,wb,cellStyle=cs)
}

add.short.name <-function(fbm, wb){
	sheets <- getSheets(wb)
	sheet <- sheets[["Minimal"]]
	csl = get.cell.styles(wb)
	cs = csl$number_col1
	set.cell.value(fbm,"Short name or Title",sheet,wb, cellStyle=cs)
}

add.refs2sim.trials <- function(siten, sitea, out,wb){
	sheets <- getSheets(wb)
	sheet <- sheets[["Minimal"]]
	csl = get.cell.styles(wb)
	cs = csl$number_col1
	sites = sitea[!(sitea %in% siten)]
	if(length(sites)==0) {
		sites = "none"
	} else {
		season = paste(out$years, out$season, sep="")
		sitex = paste(out$logPrefix, out$trialPhase,season, "_",sites, sep="")
		sites = paste(sitex,collapse=", ")
	}
	set.cell.value(sites,"Relation",sheet,wb, cellStyle=cs)
	
}

add.params <-function(siten, sitea, out, wb){
	#print("check -1")
	
	checkl = str_split(out$abd.checks,"\n")[[1]]
	matl = sort(unique(c(checkl,out$tgenotypesnew)))
	if(!str_detect(out$sdesign,"ABD")){
		matl = matl[!(matl %in% checkl)]
	}
	
	
	sheets <- getSheets(wb)
	sheet <- sheets[["Installation"]]
	csl = get.cell.styles(wb)
	cs = csl$number_col1
	#print(out)
	#print("check 0")
	nreps = 0
	if(	str_detect(out$sdesign,"CRD")){
		set.cell.value(out$crd.nreps,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"RCBD")){
		set.cell.value(out$rcbd.nbloc,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"BIBD")){
		set.cell.value(out$bsize,"Block size (applicable for BIBD only)",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"F2CRD")){
		set.cell.value(out$f2crd.nreps,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"F2RCBD")){
		set.cell.value(out$f2rcbd.nbloc,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"SPCRD")){
		set.cell.value(out$spcrd.nreps,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"SPRCBD")){
		set.cell.value(out$sprcbd.nbloc,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	if(	str_detect(out$sdesign,"ABD")){
		set.cell.value(out$abd.nbloc,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	
	if(	str_detect(out$sdesign,"A01D")){
		set.cell.value(out$a01d.nbloc,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
		#TODO add efficiency factor
		r = out$a01d.nbloc
		ntr = length(matl)
		s = out$bsize
		E <- (ntr - 1) * (r - 1)/((ntr - 1) * (r - 1) + r * (s - 1))
		# add E as comment
	
	
	}
	
	if(	str_detect(out$sdesign,"MBCRD")){
		set.cell.value(out$mbcrd.nreps,"Number of repetitions or blocks",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	}
	
	set.cell.value(out$sdesign,"Experimental design",sheet,wb, cellStyle=cs)
	set.cell.value(as.integer(out$pstart),"Plot start number",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	set.cell.value(out$nSeeds,"Number of plants planted per plot",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=csl$integer_col1)
	
	#TODO remove this duplication: see createDesignExpDlg line 589
	adfn = NULL
	adfl = NULL
	
	if(out$sdesign=="Two-Way Factorial in CRD (F2CRD)") {
		adfn = out$f2crd.adf.name
		adfl = str_split(out$f2crd.adf.level,"\n")[[1]]
	}
	if(out$sdesign=="Two-Way Factorial in RCBD (F2RCBD)") {
		adfn = out$f2rcbd.adf.name
		adfl = str_split(out$f2rcbd.adf.level,"\n")[[1]]
	}
	if(out$sdesign=="Split Plot with Plots in CRD (SPCRD)") {
		adfn = out$spcrd.adf.name
		adfl = str_split(out$spcrd.adf.level,"\n")[[1]]
	}
	if(out$sdesign=="Split Plot with Plots in RCBD (SPRCBD)") {
		adfn = out$sprcbd.adf.name
		adfl = str_split(out$sprcbd.adf.level,"\n")[[1]]
	}
	#print("check 3")
	
	if(!is.null(adfn)){
		set.cell.value(adfn,"Additional factor name",sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=cs)	
		n = length(adfl)
		for(i in 1:n){
			#print(i)
			lbl = paste("Labels for additional factor, level",i)
			set.cell.value(adfl[i],lbl,sheet,wb,hAlign="ALIGN_RIGHT", cellStyle=cs)
		}
	}
	
	
	#set genotypes
	sheets <- getSheets(wb)
	sheet <- sheets[["Material List"]]
	cc = readColumns(sheet, 1,20,1,2,stringsAsFactors=F)
	
	c.instn=which(names(cc)=="Institutional.number") #GTDM-89
	c.contr=which(names(cc)=="Control") 
	n = length(matl)
	
	rows = createRow(sheet, rowIndex=(2:(n+1)))
	cellStyle = get.cell.styles(wb)$number_col1
	
	#n=length(out$tgenotypesnew)
	
	
	for(i in 1:n){
		r = i
		set.cell.val(i, sheet,wb, col=1, r, comm=NULL, cellStyle=cs, rows=rows)
		set.cell.val(matl[i], sheet,wb, col=c.instn, r, comm=NULL, cellStyle=cs, rows=rows)
		if(out$sdesign=="Augmented Block Design (ABD)"){
			if(matl[i] %in% checkl){
				set.cell.val("x", sheet,wb, col=c.contr, r, comm=NULL, cellStyle=cs, rows=rows)
			}
		}
	}
	
	#add.refs2sim.trials(siten, sitea, out,wb) #after add.params!
	
	sites = sitea[!(sitea %in% siten)]
	if(length(sites)==0) {
		sites = "none"
	} else {
		season = paste(out$years, out$season, sep="")
		sitex = paste(out$logPrefix, out$trialPhase,season, "_",sites, sep="")
		sites = paste(sitex,collapse=", ")
	}
	c.refsim=which(names(cc)=="References.to.silmultaneous.trials")
	
	for(i in 1:n){
		r = i
		set.cell.val(sites, sheet,wb, col=c.refsim, r, comm=NULL, cellStyle=cs, rows=rows)
	}
	
	
}

get.vf <- function(fbt, type, prefs) {
  v   = paste(fbt,type, sep="")
  fil.vars = prefs[prefs$name==v,"past"]
  fil.vars = str_split(fil.vars,";")[[1]]
  fil.vars
}


add.vars <- function(vss,wb, season, dict, prefs, fbt){
	sheets <- getSheets(wb)
	sheet <- sheets[["Crop_management"]]
	s = sheet$getLastRowNum()+2
	dic=get.data.dict(vss)
	has.form=has.formula(dic)
	#vsl = dic$VAR[!has.form]
	#vsl = dic$ABBR[!has.form]
	vsl = dic[!has.form,c("ABBR","VAR")]
	
	#fil.vars = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="vars")
	fil.vars = get.vf(fbt = fbt, type = "vars", prefs = prefs)
	#vsl = vsl[fil.vars]
	vsl.abb = fil.vars[fil.vars %in% vsl$ABBR]
  vsl =  vsl[vsl$ABB %in% vsl.abb,"VAR"]
	
	n = length(vsl)
	rows=createRow(sheet,s:(s+n))
	rows=getRows(sheet)
	#tr = sheet$getLastRowNum()
	#print(tr)
	csl = get.cell.styles(wb)
	cs = csl$number_col1
	#rows=createRow(sheet,(1:(nrow(dict)+1)))
	
	for(i in 1:n){
		r = s+i-1
		#r=i
		set.cell.val("Measurement", sheet,wb, col=1, r, comm=NULL, cellStyle=cs, rows=rows)
		set.cell.val(vsl[i], sheet,wb, col=2, r, comm=NULL, cellStyle=cs, rows=rows)
	}
	n = sheet$getLastRowNum()-1
	ed = paste(substr(season,1,4),"-",substr(season,5,6),"-01",sep="")# estimated date
	for(i in 1:n){
		r=i+1
		set.cell.val(ed, sheet,wb, col=3, r, comm=NULL, cellStyle=NULL, rows=rows)
	}
	sheet <- sheets[["Minimal"]]
	rows = getRows(sheet)
	r = get.row.by.name("Begin date",sheet)
	set.cell.val(ed, sheet,wb, col=2, r, comm=NULL, cellStyle=NULL, rows=rows)
	r = get.row.by.name("End date",sheet)
	set.cell.val(ed, sheet,wb, col=2, r, comm=NULL, cellStyle=NULL, rows=rows)
	
	autoSizeColumn(sheet, 1:2)
}

add.var.list <- function(dict,wb, prefs, vss, fbt){
	sheet <- createSheet(wb,"Var List")
	#print(dict)
	rows=createRow(sheet,(1:(nrow(dict)+2)))
	n=ncol(dict)
	dic=get.data.dict(dict[,2])
	#print(dic)
	#has.form=has.formula(dic)
	csl = get.cell.styles(wb)
	cs = csl$header
	
	for(i in 1:n){
		set.cell.val(names(dict)[i], sheet,wb, col=i, r=1, comm=NULL, cellStyle=cs, rows=rows)
	}
	set.cell.val("Factor.Variables", sheet,wb, col=1, r=1, comm=NULL, cellStyle=cs, rows=rows)
	set.cell.val("Abbreviations", sheet,wb, col=2, r=1, comm=NULL, cellStyle=cs, rows=rows)
	
	set.cell.val("Fieldbook", sheet,wb, col=(n+1), r=1, comm=NULL, cellStyle=cs, rows=rows)
	set.cell.val("Summarize", sheet,wb, col=(n+2), r=1, comm=NULL, cellStyle=cs, rows=rows)
	set.cell.val("Analyze", sheet,wb, col=(n+3), r=1, comm=NULL, cellStyle=cs, rows=rows)
	set.cell.val("Selection direction", sheet,wb, col=(n+4), r=1, comm=NULL, cellStyle=cs, rows=rows)
	set.cell.val("Selection weight", sheet,wb, col=(n+5), r=1, comm=NULL, cellStyle=cs, rows=rows)
	
	cs = csl$number_col1
	cw = csl$number_col2
	
	#fil.vars = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="vars")
	
	#GTDM-101
	
	fil.vars = get.vf(fbt = fbt, type = "vars", prefs = prefs)
	fil.desc = get.vf(fbt = fbt, type = "desc", prefs = prefs)
	fil.anal = get.vf(fbt = fbt, type = "anal", prefs = prefs)
	
	
	#fil.desc = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="desc")
	#fil.anal = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="anal")
	#xv = vss == fil.vars
	#xd = vss == fil.desc
	#xa = vss == fil.anal
	#print(fil.vars)

	
	
	for(j in 1:nrow(dic)){
		r=j+1
		#print(r)
		#if(has.form[j]){
			#print("RS")
			set.cell.val(dic$VAR[j], sheet,wb, col=1, r=r, comm=NULL, cellStyle=cs, rows=rows)
			#print("RS 2")
			set.cell.val(dic$ABBR[j], sheet,wb, col=2, r=r, comm=NULL, cellStyle=cs, rows=rows)
	
			#if(fil.vars[j]) set.cell.val("x", sheet,wb, col=3, r=r, comm=NULL, cellStyle=cw, rows=rows)
			#GTDM-101 
	        #print(dic$ABBR[j])
			#print(fil.vars)
			if(dic$ABBR[j] %in% fil.vars) set.cell.val("x", sheet,wb, col=3, r=r, comm=NULL, cellStyle=cw, rows=rows)
			if(dic$ABBR[j] %in% fil.desc) set.cell.val("x", sheet,wb, col=4, r=r, comm=NULL, cellStyle=cw, rows=rows)
			if(dic$ABBR[j] %in% fil.anal) set.cell.val("x", sheet,wb, col=5, r=r, comm=NULL, cellStyle=cw, rows=rows)
		#}
	}
	autoSizeColumn(sheet, 1:7)
}

add.fieldbook <- function(data,wb,prefs, vss, fbt, sheetName="Fieldbook"){
#	print("rs")
#	print(str(data))
#	print(data)
	sheet <- createSheet(wb,sheetName)
	rows=createRow(sheet,1:(nrow(data)+1))
	#n=ncol(data)
	nn=names(data)
	p=which(nn=="INSTN")
#	print("hello")
	#print(names(data))
	#print(data[1,])
	csl = get.cell.styles(wb)
	
	fil.vars = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="vars")
	nms = vss[fil.vars]
	#print(nms)
	
	#dict = get.data.dict(names(data),"yield")
	ft = "yield"
	if(fbt=="PTLB") ft ="late blight"
	dict = get.data.dict(nms,ft)
	#print(dict)
	has.form=has.formula(dict = dict)
	#print(has.form)
	cs = csl$header
	n = p+length(nms)
	#m = 
	#rows = getRows
#	print(vss)
#	print(names(data))
#	print(nn)
#	print(nms)
	for(i in 1:n){
		#print(dict$ABBR)
		#print(names(data)[i])
		sh=NULL
		if(i>p){
			##cm=dict[dict$ABBR==names(data)[i-p],"VAR"]
			#print(cm)
			sh = nms[i-p]
			cm=dict[dict$ABBR==sh,"VAR"]
			#if(length(sh)<1) sh=NULL
		} else {
			##cm=dict[dict$ABBR==names(data)[i],"VAR"]
			xx = c("Plot", "Repetition", "Institutional number")
			cm = xx[i]
			sh = nn[i]
		}
		#print(i)
		#print(sh)
		#cs = NULL # 
		
		set.cell.val(sh, sheet,wb, col=i, r=1, comm=cm, cellStyle=cs, rows=rows)
	}
	
	# Preparing for visual separation of repetitions
	b = length(unique(data$INSTN))
	r = unique(as.integer(data$REP))
	br= b*r
#	print(b)
#	print(r)
#	print(br)
	k=1
	cm = NULL
	cs = csl$number_col1
	
	for(j in 1:nrow(data)){
		r=j+1
		#print(r)
		#bp=NULL
		#cs=NULL #
		cs = csl$number_col1
		if(p==3 & j==br[k]){
			#print(br[k])
			#bp="BOTTOM"
			if(k<length(br)) k=k+1
		}
		for(i in 1:p){
			cs = csl$integer_col1
			set.cell.val(data[j,i], sheet,wb, col=i, r=r, borderPos=bp, comm=NULL,cellStyle=cs, rows=rows)
		}
		for(i in (p+1):n){
			#print(j)
			#color="WHITE"
			cs=csl$number_col2
			if(has.form[i-p]) cs = csl$number_col3#color="GREY_25_PERCENT"
			set.cell.val('', sheet,wb, col=i, r=r, borderPos=bp, comm=NULL, cellStyle=cs, rows=rows)
		}
	}
	autoSizeColumn(sheet, 1:ncol(data))
}


