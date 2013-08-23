###############################################################################
#
#
# Oct 4, 2011
# 1:38:46 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

get.sheet.data <- function(sheet){
  #print(names(sheet))
	rows  <- getRows(sheet)   # get all the rows
	cells <- getCells(rows)   # returns all non empty cells
	values <- lapply(cells, getCellValue) # extract the values
  #print(values)
	nrows = length(rows)
	#ncols = round(length(values)/nrows+0.5,0)
	ncols = max(unique(as.integer(matrix(unlist(strsplit(names(values),"\\.")),ncol=2,byrow=T)[,2])))
	#vv <- getMatrixValues(sheet, 1:nrows, 1:ncols)
	vv <- readColumns(sheet, 1,ncols, 1,nrows, stringsAsFactors=F)
#	vv = as.data.frame(vv, stringsAsFactors=F)
#	names(vv) = as.character(vv[1,])
#	vv = vv[-1,]
	vv
}

closeAllExcel <- function() {
	gmessage(translate("_MSG_PLEASE_SAVE_"), icon="warning")
	cmd = "TSKILL excel /A"
	shell(cmd)
}

get.cell.styles<-function(wb){
	cs = list()
	nmbr1 = "###0.0"
	nmbr2 = "###0.00"
	intgr = "###0"

	font_est <- Font(wb, color="red", heightInPoints=NULL, name="Arial",
			isItalic=TRUE, isStrikeout=FALSE, isBold=TRUE, underline=NULL,
			boldweight=NULL)
	font_err <- Font(wb, color="yellow", heightInPoints=NULL, name="Arial",
			isItalic=TRUE, isStrikeout=FALSE, isBold=TRUE, underline=NULL,
			boldweight=NULL)
	
	cs$header <- CellStyle(wb, border=Border(pos="BOTTOM"),
			fill=Fill(foregroundColor="LIGHT_GREEN", pattern="SOLID_FOREGROUND"))
	cs$form <- CellStyle(wb,
			fill=Fill(foregroundColor="GREY_25_PERCENT", pattern="SOLID_FOREGROUND"))
	
	cs$estimated <- CellStyle(wb, font=font_est,
			fill=Fill(foregroundColor="GREY_25_PERCENT", pattern="SOLID_FOREGROUND"))
	cs$error <- CellStyle(wb, font=font_err,
			fill=Fill(foregroundColor="BLACK", pattern="SOLID_FOREGROUND"))
	cs$warn <- CellStyle(wb, font=font_err,
			fill=Fill(foregroundColor="RED", pattern="SOLID_FOREGROUND"))
	cs$warn2 <- CellStyle(wb, font=font_err,
			fill=Fill(foregroundColor="PINK", pattern="SOLID_FOREGROUND"))
	cs$number_no_col <- CellStyle(wb, ,
			dataFormat=DataFormat(nmbr2))
	cs$number_col1_d1 <- CellStyle(wb, 
			fill=Fill(foregroundColor="GREY_25_PERCENT", pattern="SOLID_FOREGROUND"),
			dataFormat=DataFormat(nmbr1))
	
	cs$number_col1 <- CellStyle(wb, 
			fill=Fill(foregroundColor="GREY_25_PERCENT", pattern="SOLID_FOREGROUND"),
			dataFormat=DataFormat(nmbr2))
	cs$number_col2 <- CellStyle(wb, 
			fill=Fill(foregroundColor="WHITE", pattern="SOLID_FOREGROUND", backgroundColor="yellow"),
			dataFormat=DataFormat(nmbr2))
	cs$number_col3 <- CellStyle(wb, 
			fill=Fill(foregroundColor="LIGHT_YELLOW", pattern="SOLID_FOREGROUND"),
			dataFormat=DataFormat(nmbr2))
	
	cs$integer_col1 <- CellStyle(wb, 
			fill=Fill(foregroundColor="GREY_25_PERCENT", pattern="SOLID_FOREGROUND"),
			dataFormat=DataFormat(intgr))
	
	cs
}


get.row.by.name <- function(aname, sheet){
	n = sheet$getLastRowNum()+1
	m = 10
	#v = getMatrixValues(sheet, 1:n, 1:m)
	v = readColumns(sheet,1,m,1,n)
	df = as.data.frame(v[,1],stringsAsFa=F)
	res=which(str_detect(df[,1],aname))+1 #GTDM-87
	#print(res)
	res
}

set.cell.val <- function(value, sheet,wb, col, r, color="GREY_25_PERCENT", hAlign="ALIGN_LEFT",
		comment="Inserted by program.", dataFormat=NULL,borderPosition="RIGHT", cellStyle=NULL,
		rows=NULL) {
	if(is.null(rows)) rows=getRows(sheet)
	#print(paste("row",r))
	#print(paste("col",col))
	cell <- createCell(rows[r], colIndex=col)[[1,1]]    
	if(!is.null(comment)) createCellComment(cell, string=comment, author=get.version())
	if(!is.null(value)) setCellValue(cell, value)
#	for(i in 1:length(borderPosition)){
#		cellStyle1 <- createCellStyle(wb, borderPosition=borderPosition[i],
#				borderPen="BORDER_THICK", fillBackgroundColor="yellow",
#				fillForegroundColor=color, fillPattern="SOLID_FOREGROUND",
#				dataFormat=dataFormat, hAlign=hAlign)
		if (!is.null(cellStyle)) setCellStyle(cell, cellStyle)
#	}
}


set.cell.value <- function(value, aname, sheet,wb, col=2,color="GREY_25_PERCENT", hAlign="ALIGN_LEFT",
		comment="Inserted by program.",
		dataFormat=NULL, cellStyle=NULL){
	r = get.row.by.name(aname, sheet)
	#print(r)
	set.cell.val(sheet = sheet, r = r, col = col, comment = comment, value = value, wb = wb, color = color, 
			dataFormat = dataFormat, hAlign = hAlign, cellStyle = cellStyle)
}


read.excel = function(fp, sheetName){
	
	data = read.xlsx(fp,sheetName=sheetName,stringsAsFactors=F)
	if(sheetName=="Fieldbook"){
	vars = names(data)
	dict = get.data.dict(vars)
	n = ncol(data)
	for(i in 1:n){
		tp = dict[dict$ABBR==vars[i],"TYPE"]
		ut = dict[dict$ABBR==vars[i],"UNIT"]
		if(length(tp)>0){
      if(str_detect(vars[i],"DATE")){
        data[,i]=as.factor(as.character(data[,i]))
      } else 
      if(vars[i] %in% c("REP","INSTN","PLOT")){
        data[,i]=as.factor(as.character(data[,i]))
      } else 
			if(tp=="Quantitative-Continuous"){
				data[,i]=as.numeric(as.character(data[,i]))
			} else 
			if(tp=="Quantitative-Discrete" & ut=="Count"){
				data[,i]=as.integer(as.character(data[,i]))
			} else 
			if(tp=="Text"){
				data[,i]=as.character(as.character(data[,i]))
			}
		}
	}
	} else {
		for(i in 1:ncol(data)) data[,i] = as.character(data[,i])
	}
	data
}


exists.sheet<-function(sh){
	!is.null(sh[[1]])
}

clear.sheet <- function(sheetName, fp) {
	wb = loadWorkbook(fp)
	removeSheet(wb, sheetName)
	saveWorkbook(wb, fp)
}


write.fb.backup <-function(fp){
	sheetName = "Fieldbook"
	db = read.xlsx(fp,sheetName=sheetName)
	sheetName="Fieldbook_backup"
	clear.sheet(sheetName, fp)
	write.xlsx2(db,fp,sheetName=sheetName,append=T, row.names=F)
}


format.header <- function(to,sheet, cols, rs=1 ){
	wb = loadWorkbook(to)
	sheets <- getSheets(wb)
	sh = sheets[[sheet]]
	# if not exists sheet create it
	if(is.null(sh)){
		createSheet(wb,sheet)
		sheets <- getSheets(wb)
		sh = sheets[[sheet]]
	}
	
	csl = get.cell.styles(wb)
	cs  = csl$header
	rows = getRows(sh,1:rs)
	cells <- getCells(rows)
	values <- lapply(cells, getCellValue)
	
	for(col in 1:cols){
		setCellValue(cells[[col]], values[col])
		setCellStyle(cells[[col]], cs)
	}
	autoSizeColumn(sh, 1:cols)
	saveWorkbook(wb, to)
}
#
#get.sheet <- function(to,sheet){
#	wb = loadWorkbook(to)
#	sheets <- getSheets(wb)
#	sh = sheets[[sheet]]
#	# if not exists sheet create it
#	if(is.null(sh)){
#		createSheet(wb,sheet)
#		sheets <- getSheets(wb)
#		sh = sheets[[sheet]]
#	}
#	list(wb=wb,sheet=sh)
#}
#
#
#format.row <- function(sheet, vals, rs=1, css=c("header","number_no_col")){
#	
##	csl = get.cell.styles(wb)
##	cs = csl$number_no_col
##	if(css=="header") cs = csl$header
#
#	rows = getRows(sheet,1:rs)
#	#cells <- getCells(rows)
#	vs = 1:length(vals)
#	m = length(vals)
#	for(j in 1:m){
#		col = j
#		cell <- createCell(rows[rs], colIndex=col)[[1,1]]
#		value<- vals[j]
#		setCellValue(cell, value)
##		cellStyle1 <- createCellStyle(wb, 
##			fillForegroundColor=color, fillPattern="SOLID_FOREGROUND")
#		#setCellStyle(cell, cs)
#	}
#	
#	for(col in 1:cols){
#		cell <- createCell(rows[rs], colIndex=col)[[1,1]]
#		setCellValue(cell, vs[col])
#	}
#	sheet
#}
#
#save.sheet <- function(sh, wb, to, cols=10){
#	autoSizeColumn(sh, 1:cols)
#	saveWorkbook(wb, to)
#}


write.xls <- function(data,to, sheet, header=T, format=T){
	col.names=T
	if(!header) {
		col.names=F
	}
	write.xlsx2(data,file=to, sheetN=sheet, row.names=F, col.names=col.names, append=T)
	if(format)	format.header(to, sheet, ncol(data))
}

write.xls.section <-function(data, to, sheet, section){
	wb = loadWorkbook(to)
	sheets <- getSheets(wb)
	data = round(data,0)
	Section = rep("",nrow(data))
	INSTN = row.names(data)
	db = cbind(Section, INSTN,data)
	names(db)[1]=paste(section,": ",sep="")
	
	if(!(sheet %in% names(sheets))) {
		write.xls(db, to, sheet)
	} else {
		sh = sheets[[sheet]]
		rs = sh$getLastRowNum()+2
		rl = rs+nrow(db)+1
		#rows = getRows(sh, 1:rl)
		rows = createRow(sh, rowIndex=(rs):rl)
		#print(rs)
		#print(rl)
		#print(sh$getLastRowNum())
		for(col in 1:ncol(db)){
			cell = createCell(rows[2], colIndex=col)[[1,1]]
			setCellValue(cell, names(db)[col])
		}
		#for(r in (rs+1):(rs+nrow(db))){
		for(y in 1:nrow(db)){
			for(col in 1:ncol(db)){
				#print(r)
				#print(col)
				r = y+2
				cell = createCell(rows[r], colIndex=col)[[1,1]]
				#y = r-rs
				setCellValue(cell, db[y,col])
				#print(y)
			}
			  
		}
		saveWorkbook(wb,to)
		format.header(to, sheet, ncol(db), rs=(rs+1))
	}
#	
}

write.xls.snippet <-function(data, to, sheet, section){
	wb = loadWorkbook(to)
	sheets <- getSheets(wb)
	db = data
	if(is.null(data)) data =""
	data = as.data.frame(data)
#	Section = rep("",nrow(data))
#	INSTN = row.names(data)
#	db = cbind(Section, INSTN,data)
#	names(db)[1]=paste(section,": ",sep="")
	sh = sheets[[sheet]]
	if(is.null(sh)){
		sh = createSheet(wb,sheet)
		#rows = createRow(sh, rowIndex=1:100)
		saveWorkbook(wb,to)
		wb = loadWorkbook(to)
		sheets <- getSheets(wb)
		sh = sheets[[sheet]]
	} 
		rs = sh$getLastRowNum()+1
		if(rs>1) {
			rs=rs+1
			rows = createRow(sh, rowIndex=(rs):(rs))
			rs=rs+1
		}
		#rs = sh$getLastRowNum()+2
		rl = rs+nrow(data)
		#rows = getRows(sh, 1:rl)
		#rs=rs+1
		rows = createRow(sh, rowIndex=(rs):(rl))
	    #rows = getRows(sh,(rs+1):rl)
#		print(rs)
#		print(rl)
#		print(sh$getLastRowNum())
		
		cell = createCell(rows[1], colIndex=(1))[[1,1]]
#		print("check 1")
		setCellValue(cell, section)
		csl = get.cell.styles(wb)
		cs  = csl$header
		setCellStyle(cell, cs)
		
		if(!is.null(db)){
		for(col in 1:(ncol(data))){
			cell = createCell(rows[1], colIndex=(col+1))[[1,1]]
			setCellValue(cell, names(data)[col])
			setCellStyle(cell, cs)
		}
		
#		print("check 2")
		#for(r in (rs+1):(rs+nrow(db))){
		for(y in 1:nrow(data)){
			for(col in 1:(ncol(data))){
				#print(r)
				#print(col)
				r = y+1
#				print(y)
				cell = createCell(rows[r], colIndex=(col+1))[[1,1]]
#				print("check 3")
				#y = r-rs
				setCellValue(cell, data[y,col])
				#print(y)
			}
			
		}
		}
		cols = ncol(data)+1
		autoSizeColumn(sh, 1:cols)
		saveWorkbook(wb,to)
		#format.header(to, sheet, ncol(db), rs=(rs+1))
	
#	
}

