###############################################################################
#
# TODO: Add comment
#
# Apr 10, 2012
# 10:10:21 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

calc.missing <-function(fp){
	missRate=10 #10% missing data
	sheetName="Var List"
	db = read.excel(fp, sheetName)
	data=db[!is.na(db$'N.Valid'),c("N.Valid","N.Miss.")]
	data[,1]=as.integer(data[,1])
	data[,2]=as.integer(data[,2])
	th = data[,2]/data[,1]*100
	th>=missRate
	n = length(data[th,1]) # number of missing data

	wb = loadWorkbook(fp)
	sh = getSheets(wb)[[sheetName]]
	csl = get.cell.styles(wb)

	ri = as.integer(row.names(data[th<10,]))+1
	rows = getRows(sh,ri)
	cell = getCells(rows)
	lapply(cell,setCellStyle,csl$number_col1)
	
	if(n>0){
		
		ri = as.integer(row.names(data[th>=10,]))+1
		rows = getRows(sh,ri)
		cell = getCells(rows)
		lapply(cell,setCellStyle,csl$warn)
		
	}
	
	saveWorkbook(wb,fp)
	
}

