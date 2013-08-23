###############################################################################
#
# Compare two matrices or fieldbooks GTDM-404
#
# May 23, 2012
# 4:58:28 PM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

compare.fb <- function(fp){
	data1 = read.xlsx(fp,sheetName="Fieldbook",stringsAsFactors=F)
	data2 = read.xlsx(fp,sheetName="Fieldbook2",stringsAsFactors=F)
	if(nrow(data1)!=nrow(data2) | ncol(data1)!=ncol(data2)){
		compm=FALSE
	} else {
		compm = data1==data2	
	}
	compm
}

fbs.consistent <- function(compm){
	if(class(compm)=="logical") return(FALSE)
	is.na(table(compm)["FALSE"])[[1]]
}

mat2lst <-function(mat){
	stopifnot(class(mat)=="matrix")
	x = NULL
	for(i in 1:nrow(mat)) x = c(x,rep(i,ncol(mat)))
	y = rep(1:ncol(mat),nrow(mat))
	cbind(x,y,as.vector(t(mat)))
}


log.check.fb <- function(fp, wb){
	res = compare.fb(fp)
	
	if(fbs.consistent(res)){
		rule = "Validated consistency of Fieldbook and Fieldbook2"
		x=check.log("Double entry",rule,check=TRUE)
	} else if(class(res)=="logical"){
		rule = "Tested that Fieldbook and Fieldbook2 have the same number of columns and rows"
		x=check.log("Double entry",rule,check=FALSE)
	} else if(class(res)=="matrix"){
		nerr = table(res)["FALSE"][[1]]
		rule = paste("Tested that Fieldbook and Fieldbook2 have",nerr,"difference(s).")
		x=check.log("Double entry",rule,check=FALSE)
		
		# list those errors
		mat = as.data.frame(mat2lst(res))
		mat = mat[!is.na(mat[,3]),]
		mat = mat[mat[,3]==0,]
		#print(mat)
		n = nrow(mat)
		data1 = read.xlsx(fp,sheetName="Fieldbook",stringsAsFactors=F)
		data2 = read.xlsx(fp,sheetName="Fieldbook2",stringsAsFactors=F)
		#wb = loadWorkbook(fp)
		sh = getSheets(wb)
		fb = sh[["Fieldbook"]]
		fb2= sh[["Fieldbook2"]]
		cs = get.cell.styles(wb)
		for(i in 1:n){
			
			cols = names(data1)
			val = data1[mat$x[i],mat$y[i]]
			val2 = data2[mat$x[i],mat$y[i]]
			col = mat$y[i]
			row = mat$x[i]
			rule = paste("Tested that Fieldbook and Fieldbook2 differ in variable",cols[col],"and row",row,"for value",val,".")
			x=check.log("Double entry",rule,check=FALSE)
			# Paint the corresponding cells in both fieldbooks
			set.cell.val(val ,fb ,wb,col,r=(row+1),cellStyle=cs$warn2)
			set.cell.val(val2,fb2,wb,col,r=(row+1),cellStyle=cs$warn2)
		}
		#saveWorkbook(wb,fp)
	}
	fbs.consistent(res)
}

