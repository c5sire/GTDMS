###############################################################################
#
# TODO: Add comment
#
# Oct 9, 2011
# 10:27:04 PM
# Author: Raul Eyzaguirre, Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

themode <-function(x){
	tv=table(x)
	paste(names(tv[tv==max(tv)]),collapse=", ")
}

get.summary.idx <- function(varl){
	idx=as.character(varl$Abbreviations)
	if(length(varl$Summarize)>0){
		varl.ix= as.character(varl$Summarize)
		xx = rep("x",length(varl.ix))
		idx = as.character(varl$Abbreviations[varl.ix %in% xx])
	}
	idx
}


calc.summary <-function(fp){
	#fp = getFieldBookPath(fn)
	digits = 2
	
	datos	= read.excel(fp, sheetName="Fieldbook")
	inst   	= read.xlsx2(fp,sheetName="Installation", stringsAsFactors=F)
	varl   	= read.xlsx2(fp,sheetName="Var List", stringsAsFactors=F)
	mgt		= read.xlsx2(fp,sheetName="Crop_management", stringsAsFactors=F)
	mtl		= read.xlsx2(fp,sheetName="Material List", stringsAsFactors=F)
	mml		= read.xlsx2(fp,sheetName="Minimal", stringsAsFactors=F)
	typ		= as.character(mml[mml$Factor=="Type of Trial","Value"])
	diseno	= as.character(inst[inst$Factor=="Experimental design","Value"])
	iden	= "INSTN"

	cvn=which(names(datos)==iden)	
	ndatos=datos
#TODO check if this 'if' is necessary	
if (diseno=="Two-Way Factorial in RCBD" | diseno=="Two-Way Factorial in CRD"){ 
	
	## calculos: media, desviacion estandar, n
	pp <- cvn
	cvn <- cvn+1
	calcula <- function(datos, idx){
		vvv = datos[,idx]
		nnn = names(datos)[idx]
		#print(nnn)
		subdatos <- subset(datos, vvv!="NA")
		resumen <- as.data.frame(t(table(subdatos[,c(3,cvn)])))
		if(is.numeric(vvv)){
			resumen <- cbind(resumen, round(tapply(vvv, paste(datos[,3],datos[,cvn]), mean, na.rm=T),digits))
			resumen <- cbind(resumen, round(tapply(vvv, paste(datos[,3],datos[,cvn]), sd, na.rm=T),digits))
			resumen <- resumen[,c(1,3,4,5)]
			colnames(resumen)[2:4] <- c(paste(nnn,"n",sep="_"), paste(nnn,"Mean",sep="_"), paste(nnn,"SD",sep="_"))
		}
		if(is.factor(vvv)){
			vvv=as.numeric(as.character(vvv))
			resumen <- cbind(resumen, tapply(vvv, paste(datos[,3],datos[,cvn]), themod))
			resumen <- resumen[,c(1,3,4)]
			#colnames(resumen)[3] <- paste(nnn,"Mode",sep="_")
			colnames(resumen)[2:3] <- c(paste(nnn,"n",sep="_"), paste(nnn,"Mode",sep="_"))
		}
		resumen[,-c(2)]
	} 	
	resu <- as.data.frame(t(table(datos[,c(3,cvn)])))
	resu <- names(datos)[3:cvn]
} else{
	## calculos: media, desviacion estandar, n
	pp <- 2
	calcula <- function(datos, idx){
		vvv = datos[,idx]
		nnn = names(datos)[idx]
		#print(nnn)
		subdatos <- subset(datos, vvv!="NA")
		#print(str(subdatos))
		resumen <- as.data.frame(table(subdatos[,cvn]))
		names(resumen)[1:2] <- c("INSTN",paste(nnn,"n",sep="_"))
		if(is.numeric(vvv)){
			resumen <- cbind(resumen, round(tapply(vvv, datos[,cvn], mean, na.rm=T),digits))
			resumen <- cbind(resumen, round(tapply(vvv, datos[,cvn], sd, na.rm=T),digits))
			colnames(resumen)[3:4] <- c( paste(nnn,"Mean",sep="_"), paste(nnn,"sd",sep="_"))
		}
		if(is.factor(vvv)){
			vvv=as.numeric(as.character(vvv))
			#print(vvv)
			#datos[,cvn]<-as.integer(as.character(datos[,cvn]))
			resumen <- cbind(resumen, tapply(vvv, datos[,cvn], themode))
			colnames(resumen)[3] <- paste(nnn,"Mode",sep="_")
		}
		resumen[,-c(1)]
	} 	
	datos[,cvn] = as.factor(datos[,cvn])
	resu <- as.data.frame(levels(datos[,cvn]))
	colnames(resu) <- iden
}
#print(datos)
nms = get.summary.idx(varl)
#print(nms)
nds = names(datos)[1:3]
#print(nds)
nnn = c(nds,nms)
#print(nnn)
#print(names(datos))
datos= datos[,nnn]

#TODO handle col idx correctly for differing number of columns in factorial designs

#
for (i in 4:ncol(datos)){
	resu = cbind(resu,calcula(datos,i))
} 
#resu = as.data.frame(resu,stringsAsF=F)
shn = "Summary by clone"
#clear.sheet( shn, fp)

  
  
  
wb = loadWorkbook(fp)
sheets <- getSheets(wb)
# create the list of column styles
fbs = getFbStyles(wb) 
cs = list()
cs[[1]] = fbs$RHdr
names(cs)[1] = 1
jj = 0
kk = 0
for(i in 2:ncol(resu)){
  jj = jj+1
  kk = kk+1
  if(jj>3){
    #print(paste(names(resu)[i],"yellow",kk))
    #cs[[i-1]] = fbs$
    if(kk==1) cs[[i]] = fbs$Clc0
    if(kk==2) cs[[i]] = fbs$Clc2
    if(kk==3) cs[[i]] = fbs$Clc2
  } else {
    #print(paste(names(resu)[i],"white",kk))
    if(kk==1) cs[[i]] = fbs$Itgr
    if(kk==2) cs[[i]] = fbs$Nbr2
    if(kk==3) cs[[i]] = fbs$Nbr2
    
  }
  if(jj==6) jj=0
  if(kk==3) kk=0
  names(cs)[i] = i
}
sheet = createSheet(wb,"Summary by clone")
#sht = sh[["Summary by Clone"]]
#resu = for(i in 2:ncol(resu)) resu[,i] = as.numeric(resu[,i])
rs = as.matrix(resu[,c(2:ncol(resu))])
resu[,2:ncol(resu)] =  rs
addDataFrame(resu,sheet, colnamesStyle=fbs$CHdr, rownamesStyle = fbs$RHdr, colStyle = cs, row.names=F)
autoSizeColumn(sheet, 1:ncol(resu))
saveWorkbook(wb, fp)
}



calc.summary.charts <- function(fp){
	data	= read.excel(fp, sheetName="Fieldbook")
	# filter out only columns with data
	data = get.data.only(data = data)
	dict = get.data.dict(names(data))
	fc = file.path(getwd(),"temp","charts")
	if(!file.exists(fc)) dir.create(fc,rec=T)
	unlink(file.path(fc, paste("*.*")))
	#print(str(ncol(data)))
	for (i in 1:ncol(data)){
		#print(names(data)[i])
		img = make.chart.name(data = data, i = i)
		nmd = names(data)[i]
		fulln = paste(dict[dict$ABBR==nmd,"VAR"]," (",nmd,")",sep="")
		units = dict[dict$ABBR==nmd,"UNIT"]
		png(filename=img, width=400, height=400)
		if(!is.factor(data[,i])){	
			boxplot(data[,i], main=paste("Boxplot of"),sub=fulln, ylab=units)
		} else {
		#	x = as.integer(as.character(data[,i]))
		  xx = sort(table(data[,i]))
      cex = 1
      if(length(xx)>30) cex=0.7
# 			fr= table(x)
# 			xx = cbind(as.integer(names(fr)), as.numeric(fr))
# 			rownames(xx) = xx[,1]
# 			xx = xx[,-1]
			
			#desc = dict[dict$ABBR==nmd,"DESC"]
			
			dotchart(xx, main="Dotchart for",sub=fulln, xlab="Frequency [absolute]", ylab=units, cex=cex)
		}
		
		devAskNewPage(ask=FALSE)
		dev.off()
	}
	
}


calc.descriptive <-function(fp){
	#fp = getFieldBookPath(fn)
	data	= read.excel(fp, sheetName="Fieldbook")
#	varl   	= read.xlsx2(fp,sheetName="Var List")

#	nms = get.summary.idx(varl)
#	nds = names(data)[1:3]
#	nnn = c(nds,nms)
#	datos= data[,nnn]
	
	ds = as.data.frame(matrix(NA, nrow=10, ncol=length(data)), stringsAsFactors=F)
	labels=c("Min.", "1st Qu.", "Median", "Mode","3rd Qu.", "Max.",
			"Mean", "St.Dev.", "N Valid", "N Miss.")
	rowlabs = as.data.frame(matrix(labels, nrow=10),
			strings.as.factors=F)
	colnames(ds) <- colnames(data)
	
	cuartil <- function(data, p, mv) quantile(data, p, mv)
	
	for(i in 1:ncol(data)){
		if(is.numeric(data[,i])){
			ds[1,i] <- min(data[,i],na.rm=T)
			ds[2,i] <- cuartil(data[,i],p=.25, mv=T)
			ds[3,i] <- cuartil(data[,i],p=.50, mv=T)
			#ds[4,i] <- themode(round(data[,i],0))
			ds[5,i] <- cuartil(data[,i],p=.75, mv=T)
			ds[6,i] <- max(data[,i],na.rm=T)
			ds[7,i] <- mean(data[,i], na.rm=T)
			ds[8,i] <- sd(data[,i], na.rm=T)
			#ds[,i] = signif(ds[,i],4)
		}
		if(is.factor(data[,i])){
			ds[4,i] <- themode(data[,i])
		}
		if(is.integer(data[,i])){
			ds[1,i] <- min(data[,i],na.rm=T)
			ds[2,i] <- cuartil(data[,i],p=.25, mv=T)
			ds[3,i] <- cuartil(data[,i],p=.50, mv=T)
			#ds[4,i] <- themode(data[,i])
			ds[5,i] <- cuartil(data[,i],p=.75, mv=T)
			ds[6,i] <- max(data[,i],na.rm=T)
			ds[7,i] <- mean(data[,i], na.rm=T)
			ds[8,i] <- sd(data[,i], na.rm=T)
			#ds[,i] = round(ds[,i],0)
		}
		if(!is.character(data[,i])){
			ds[9,i] <- sum(!is.na(data[,i]))
			ds[10,i] <- sum(is.na(data[,i]))
		}
	}
	ds = as.data.frame(t(ds))
	names(ds)=labels
	
	vars = row.names(ds)
	varl = read.xlsx2(fp,sheetName="Var List", stringsAsFactors=F)
	p = ncol(varl)
	varl[,2]=as.character(varl[,2])
	x=ds[vars %in% varl[,2],]
	
	wb = loadWorkbook(fp)
	sheets <- getSheets(wb)
	sheet = sheets[["Var List"]]
	csl = get.cell.styles(wb)
	cs  = csl$header
	csd = csl$number_col1
  csd[["dataFormat"]][[1]] = "###0"
	m 	  = ncol(x)
	#x = as.numeric(as.character(x))
	rows = getRows(sheet)
	
	for(i in 1:ncol(x)) x[,i]=as.numeric(as.character(x[,i]))
	x = round(x,1)
	
	#find the indices of the rows where to put the results
	ridx = which(varl$Abbreviations %in% row.names(x))+1
	#writer header
	cs = csl$header #color="LIGHT_GREEN"
	for(j in 1:m){
		col = j+p
		cell <- createCell(rows[1], colIndex=col)[[1,1]]
		value<- names(x)[j]
		setCellValue(cell, value)
#		cellStyle1 <- createCellStyle(wb, 
#			fillForegroundColor=color, fillPattern="SOLID_FOREGROUND")
		setCellStyle(cell, cs)
	}	
	cs = csl$number_col1 #color="GREY_25_PERCENT"
	for(i in 1:nrow(x)){
	 for(j in 1:m){
		col = j+p
		row = ridx[i]
		cell <- createCell(rows[row], colIndex=col)[[1,1]]
		#dfmt ="###0.0"
		
		if(!is.na(x[i,j]) & (as.character(x[i,j])!="Inf") & (as.character(x[i,j])!="-Inf")){
			value<-x[i,j]
			setCellValue(cell, value)
		} else {
			setCellValue(cell, "")
			cs = csl$number_col1 #dfmt =NULL
		}
		if(j %in% c(4,9,10)) cs = csd #dfmt="###0"
#		cellStyle1 <- createCellStyle(wb, 
#				fillForegroundColor=color, fillPattern="SOLID_FOREGROUND",dataFormat=dfmt)
		setCellStyle(cell, cs)
	}	
	}	
	
	autoSizeColumn(sheet, 1:(ncol(x)+3))
	saveWorkbook(wb, fp)
}

calc.missing <- function(fp, pb=NULL,ii=NULL, info=NULL){
	#fp = getFieldBookPath(fn)
	# get the data with missing data; take advantage of Rs in-built scanning
	# 1. read data
	if(!is.null(pb)) setWinProgressBar(pb, ii, sprintf("Preparing missing data file (%s)", info), info)
	
	data	= read.excel(fp, sheetName="Fieldbook")
	# 2. save them to a temporary file in a text format: not necessary: NA introduced by coercion
	# 4. estimate the missing data using the rfImpute method with 'genoypes' as fixed y
	# only use columns from INSTN onwards
	xdata = data
	ndata = data
	p = which(names(data)=="INSTN")
	data = data[,c(p:ncol(data))]
	# filter out columns with no data at all
	if(!is.null(pb)) setWinProgressBar(pb, ii, sprintf("Estimating missing data (%s)", info), info)
	hd = rep(FALSE, ncol(data))
	for(i in 2:ncol(data)) hd[i]=has.data(data[,i])
	
	data = data[,hd]
	write.csv(data,"check.txt",row.names=F, quote=F)
	#are there any missing data at all?
	any.na = which(is.na(data))
	#print(length(any.na))
	if(length(any.na)!=0){
		#print("calc missing")
	# filter out columns with more than 15% missing values!?
	data[,2:ncol(data)] = rfImpute(data[,-1],data[,1])[,-1]
	
	# copy back columns into the full matrix
	for(i in 2:ncol(data)) xdata[,names(data)[i]] = data[,i]
	
	##############################################
	if(!is.null(pb)) setWinProgressBar(pb, ii, sprintf("Saving missing data (%s)", info), info)
	# 5. then save the results back into the excel table and paint the estimates with a blue background
	wb = loadWorkbook(fp)
	sheets <- getSheets(wb)
	sheet = sheets[["Fieldbook"]]
	rows = getRows(sheet)

	csl = get.cell.styles(wb)
	cs = csl$estimated
	for(i in p:ncol(xdata)){
		hdr = names(xdata)[i]
		#print(hdr)
		if(hdr %in% names(data)){
			#print(i)
			for(r in 1:nrow(xdata)){
				na = ndata[r,i]
				#print(na)
				#print(paste(r,i,"\t"))
				if(is.na(na)){
					#print(paste("NA ",r,i))
					
					cell <- createCell(rows[(r+1)], colIndex=i)[[1,1]]
					cmt = getCellComment(cell)
					txt = ""
					if(length(cmt)>0){
						txt = cmt$getString()$toString()
					} else txt=""
					cmt = paste(txt,"\n", "Estimated value: ",xdata[(r+1),i],sep="")
					#print(txt)
					createCellComment(cell, cmt, author=get.version())					
					
					setCellValue(cell, xdata[(r+1),i])
					setCellStyle(cell, cs)
				} 
				
			}
		}
	}
	saveWorkbook(wb, fp)
	}
}

calc.charts <-function(fp){
	calc.summary.charts(fp)
	data	= read.excel(fp, sheetName="Fieldbook")
	# filter out only columns with data
	data = get.data.only(data = data)
	#insert into Workbook
	#clear.sheet(fp, "Charts")
	
	#sheet = add.sheet(data, fp, sh="Charts",coln=F)
	sheetName="Charts"
	clear.sheet(sheetName, fp)
	wb = loadWorkbook(fp)
	
	#removeSheet(wb, sheetName)
	#saveWorkbook(wb,fp)
	#wb = loadWorkbook(fp)
	
	sheet <- createSheet(wb,sheetName)
	x=1
	y=1
	for(i in 1:ncol(data)){
		# check that there are at least five data points
		
		img = make.chart.name(data = data, i = i)
		r = y
		c = x
		#print(paste(i,r,c))
		addPicture(img, sheet, scale=1, startRow=r, startColumn=c)
		if(i %% 2==0){
			y = y+25
			x = 1
		} else {
			x = x+7
		}
		
		
		
	}
	
	saveWorkbook(wb, fp)
}

get.fb.param <-function(fp,sheet,param){
	params <- read.xlsx2(fp, sheetName = sheet)
	for(i in 1:ncol(params)) params[,i]<-as.character(params[,i])
	params[params$Factor==param,2]
}


add.anova.sheet <- function(mtrx, img1, img2, fp, sh, coln=T) {
	wb = loadWorkbook(fp)
#		sh = "Format checks"
	removeSheet(wb,sh)
	saveWorkbook(wb, fp)
	wb = loadWorkbook(fp)
	write.xlsx2(mtrx, fp, sheetName=sh, app=T, row.names=F, col.names=coln)
	wb = loadWorkbook(fp)
	sheets = getSheets(wb)
	sheet = sheets[[sh]]
	
	
	cs <- CellStyle(wb) + Font(wb, name="Courier")
	#print("check 1")
	rows<-getRows(sheet)
	for(i in 1:length(rows)) setCellStyle(getCells(rows[i],1)[[1]],cs)
	
#	#if(!is.null(ncol(mtrx))) autoSizeColumn(sheet,1:ncol(mtrx))
#	
#	addPicture(img1, sheet, startRow=1, startColumn=10)
#	addPicture(img2, sheet, startRow=29, startColumn=10)
	
	
	
	saveWorkbook(wb,fp)
	
	
	
}



calc.anova <-function(fp){
#	library(XLConnect)
#	library(agricolae)
#	
#	sn = "data/yield-trial-sp-demo-data.xls"
#	tpl = c("Fieldbook data","*.xls")
#	
#	workbook = choose.files(sn, "Choose a fieldbook where you already set parameters!", filters=tpl)
	wb = loadWorkbook(fp)
	design <- get.fb.param(fp,"Installation","Experimental design")	
	val.des <-c("Randomized Complete Block Design (RCBD)")
	if(!design %in% val.des){
		if(length(val.des)==1){
			msg= paste("Currently only analysis for '",val.des[1],"'.",sep="")
		} else msg = paste("Currently only analysis for '",paste(val.des,col=", "),"'.",sep="")
		gmessage(msg,
				icon="info")
		return(FALSE)
	}
	
	data = read.xlsx(fp,sheetName="Fieldbook", h=T)
	datos = guessVariableType(data)

	varl = read.xlsx(fp,sheetName="Var List", h=T)
	for(i in 1:5) varl[,i] <- as.character(varl[,i])
	
	abb = varl[varl[,"Analyze"]=="x"|varl[,"Analyze"]=="X",2]
	abb = abb[!is.na(abb)]
	lab = varl[varl[,"Analyze"]=="x"|varl[,"Analyze"]=="X",1]
	lab = lab[!is.na(lab)]
	
	
#	datos <- readWorksheet(wb, sheet = "datac", startRow=1, endRow=-1,
#			startCol=1, endCol=-1, header=T)
#	
#	varl <- readWorksheet(wb, sheet = "Var List", startRow=2, endRow=-1,
#			startCol=1, endCol=5, header=F)
	
#	abb = varl[varl[,5]=="x"|varl[,5]=="X",2]
#	abb = abb[!is.na(abb)]
#	lab = varl[varl[,5]=="x"|varl[,5]=="X",1]
#	lab = lab[!is.na(lab)]
	
	if (design == "Completely Randomized Design (CRD)"){
		for (i in 1:length(abb)){	
			
			modelo <- aov(datos[abb[i]][[1]] ~ factor(datos[,3]), data=datos)
			tabla <- anova(modelo)
			row.names(tabla)[1] <- colnames(datos)[3]
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			jpeg(filename = paste("dp_", abb[i], "_1.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 1, sub.caption="")
			dev.off()
			jpeg(filename = paste("dp_", abb[i], "_2.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 2, sub.caption="")
			dev.off()
			nn=names(na.omit(rstandard(modelo)))
			btest <- bartlett.test(na.omit(rstandard(modelo)) ~ datos[nn,"INSTN"])
			btest$data.name <- "standardized residuals by treatments"
			ntest <- shapiro.test(rstandard(modelo))
			ntest$data.name <- "standardized residuals"
			compara <- HSD.test(modelo, "factor(datos[, 3])")
			sink(paste(abb[i],".txt",sep=""))
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(btest)
			cat("################################################################################","\n")
			print(ntest)
			cat("################################################################################","\n","\n")
			print(tabla)
			cat("\n")
			cat("################################################################################","\n","\n")
			cat("Tukey's HSD test", "\n", "\n")
			print(compara)
			sink()
		}
	}
	if (design == "Randomized Complete Block Design (RCBD)"){
		for (i in 1:length(abb)){
			#try((
			resp = datos[,abb[i]]
			modelo <- aov(resp ~ factor(INSTN) + factor(REP), data=datos)
			tabla <- anova(modelo)
			row.names(tabla)[1] <- "INSTN" #colnames(datos)["INSTN"]
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			ipth1 = file.path(getwd(),"temp",paste("aov_dp_", abb[i], "_1.png", sep=""))
			png(filename = ipth1, width = 450, height = 450)
			plot(modelo, 1, sub.caption=abb[i])
			dev.off()
			ipth2 = file.path(getwd(),"temp",paste("aov_dp_", abb[i], "_2.png", sep=""))
			png(filename = ipth2, width = 450, height = 450)
			plot(modelo, 2, sub.caption=abb[i])
			dev.off()
			nn=names(na.omit(rstandard(modelo)))
			btest <- bartlett.test(na.omit(rstandard(modelo)) ~ datos[nn,"INSTN"])
			#btest <- bartlett.test(rstandard(modelo) ~ datos[,"INSTN"]) # datos[,3]
			btest$data.name <- "standardized residuals by treatments"
			ntest <- shapiro.test(rstandard(modelo))
			ntest$data.name <- "standardized residuals"
			compara <- HSD.test(modelo, "factor(INSTN)")
			fpth = file.path(getwd(),"temp",paste(abb[i],".txt",sep=""))
			sink(fpth)
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(btest)
			cat("################################################################################","\n")
			print(ntest)
			cat("################################################################################","\n","\n")
			print(tabla)
			cat("\n")
			cat("################################################################################","\n","\n")
			cat("Tukey's HSD test", "\n", "\n")
			print(compara)
			sink()
			data = readLines(fpth)
			add.anova.sheet(data, ipth1,ipth2, fp, sh=abb[i],coln=F)
			#))
			}
	}
#if (diseno == "Balanced Incomplete Block Design (BIBD)"){
#}
#if (diseno == "Latin Square Design"){
#}
	if (design == "Two-Way Factorial in CRD"){
		for (i in 1:length(abb)){
			modelo <- aov(datos[abb[i]][[1]] ~ factor(datos[,4])*factor(datos[,3]), data=datos)
			tabla <- anova(modelo)
			row.names(tabla) <- c(colnames(datos)[4], colnames(datos)[3],
					paste(colnames(datos)[4],"x",colnames(datos)[3]), "Residuals")
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			jpeg(filename = paste("dp_", abb[i], "_1.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 1, sub.caption="")
			dev.off()
			jpeg(filename = paste("dp_", abb[i], "_2.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 2, sub.caption="")
			dev.off()
			btest <- bartlett.test(rstandard(modelo) ~ paste(datos[,3],datos[,4]))
			btest$data.name <- "standardized residuals by treatments"
			ntest <- shapiro.test(rstandard(modelo))
			ntest$data.name <- "standardized residuals"
			sink(paste(abb[i],".txt",sep=""))
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(btest)
			cat("################################################################################","\n")
			print(ntest)
			cat("################################################################################","\n","\n")
			print(tabla)
			cat("\n")
			cat("* It is assumed that both factors are fixed.", "\n")
			sink()
		}	
	}	
	if (design == "Two-Way Factorial in RCBD"){
		for (i in 1:length(abb)){
			modelo <- aov(datos[abb[i]][[1]] ~ factor(datos[,4]) + factor(datos[,3]) + factor(Block) %in%
							factor(datos[,3]) + factor(datos[,4]):factor(datos[,3]), data=datos)
			tabla <- anova(modelo)
			tabla[2,4] <- tabla[2,3]/tabla[3,3]
			tabla[2,5] <- pf(tabla[2,4], tabla[2,1], tabla[3,1], lower.tail=F)
			row.names(tabla) <- c(colnames(datos)[4], colnames(datos)[3],
					paste("Block:",colnames(datos)[3],sep=""),
					paste(colnames(datos)[4],"x",colnames(datos)[3]), "Residuals")
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			jpeg(filename = paste("dp_", abb[i], "_1.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 1, sub.caption="")
			dev.off()
			jpeg(filename = paste("dp_", abb[i], "_2.jpg", sep=""), width = 450, height = 450)
			plot(modelo, 2, sub.caption="")
			dev.off()
			btest <- bartlett.test(rstandard(modelo) ~ paste(datos[,3],datos[,4]))
			btest$data.name <- "standardized residuals by treatments"
			ntest <- shapiro.test(rstandard(modelo))
			ntest$data.name <- "standardized residuals"
			sink(paste(abb[i],".txt",sep=""))
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(btest)
			cat("################################################################################","\n")
			print(ntest)
			cat("################################################################################","\n","\n")
			print(tabla)
			cat("\n")
			cat("* It is assumed that both factors are fixed and that blocks are random.", "\n")
			cat(paste("* It is assumed that blocks are nested into factor",colnames(datos)[3],"."))
			sink()
		}	
	}
	if (design == "Split Plot with Plots in CRD"){
		for (i in 1:length(abb)){
			modelo <- aov(datos[abb[i]][[1]] ~ factor(datos[,2]) + factor(datos[,3]) + 
							factor(datos[,2]):factor(datos[,3]) + factor(datos[,4]) + 
							factor(datos[,3]):factor(datos[,4]), data=datos)
			tabla <- anova(modelo)
			tablabk <- tabla
			tabla[1,] <- tablabk[2,]
			tabla[2,] <- tablabk[1,]+tablabk[4,]
			tabla[3,] <- tabla[1,]+tabla[2,]
			tabla[4,] <- tablabk[3,]
			tabla[7,] <- tabla[3,]+tabla[4,]+tabla[5,]+tabla[6,]
			row.names(tabla) <- c(colnames(datos)[3], "Residual(Plots)", "Total(Plots)",
					colnames(datos)[4], paste(colnames(datos)[3],"x",colnames(datos)[4]),
					"Residual(SubPlots)", "Total(SubPlots)")
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			tabla[1,4] <- tabla[1,3]/tabla[2,3]
			tabla[1,5] <- pf(tabla[1,4], tabla[1,1], tabla[2,1], lower.tail=F)
			tabla[2,c(4,5)] <- ""
			tabla[3,c(3,4,5)] <- ""
			tabla[7,3] <- ""
			sink(paste(abb[i],".txt",sep=""))
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(tabla)
			cat("\n")
			cat("* It is assumed that both factors are fixed.", "\n")
			sink()
		}	
	}
	if (design == "Split Plot with Plots in RCBD"){
		for (i in 1:length(abb)){
			modelo <- aov(datos[abb[i]][[1]] ~ factor(datos[,2]) + factor(datos[,3]) + 
							factor(datos[,2]):factor(datos[,3]) + factor(datos[,4]) + 
							factor(datos[,3]):factor(datos[,4]), data=datos)
			tabla <- anova(modelo)
			tablabk <- tabla
			tabla[3,] <- tablabk[4,]
			tabla[4,] <- tabla[1,]+tabla[2,]+tabla[3,]
			tabla[5,] <- tablabk[3,]
			tabla[6,] <- tablabk[5,]
			tabla[7,] <- tablabk[6,]
			tabla[8,] <- tabla[4,]+tabla[5,]+tabla[6,]+tabla[7,]
			row.names(tabla) <- c(colnames(datos)[2], colnames(datos)[3], "Residual(Plots)",
					"Total(Plots)", colnames(datos)[4],
					paste(colnames(datos)[3],"x",colnames(datos)[4]), "Residual(SubPlots)",
					"Total(SubPlots)")
			attr(tabla,"heading")[2] <- paste("Response:",abb[i])
			tabla[1,4] <- tabla[1,3]/tabla[3,3]
			tabla[1,5] <- pf(tabla[1,4], tabla[1,1], tabla[3,1], lower.tail=F)		
			tabla[2,4] <- tabla[2,3]/tabla[3,3]
			tabla[2,5] <- pf(tabla[2,4], tabla[2,1], tabla[3,1], lower.tail=F)		
			tabla[3,c(4,5)] <- ""
			tabla[4,c(3,4,5)] <- ""
			tabla[8,3] <- ""
			sink(paste(abb[i],".txt",sep=""))
			cat("################################################################################","\n")
			cat(paste("Analysis for", lab[i]), "\n")
			cat("################################################################################","\n")
			print(tabla)
			cat("\n")
			cat("* It is assumed that both factors are fixed.", "\n")
			sink()
		}	
		
	}
	#shell(sn)
	return(TRUE)
	
}
