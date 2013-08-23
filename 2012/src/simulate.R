###############################################################################
#
# TODO: Add comment
#
# Oct 8, 2011
# 12:40:16 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

simulate.var <- function(avec,n){
	sample(avec,n, repl=T)
}

get.codes <-function(dic){
	st=strsplit(dic$DESC,";")[[1]]
	rs=character(length(st))
	for(i in 1:length(st)){
		rs[i]=strsplit(st[i],"\\=")[[1]][1]
		rs[i]=str_trim(rs[i])
	}
	rs
}

simulate.obs <- function(avar,n,dic){
	#var is an abbreviation from the data dictionary
	#dic=get.data.dict(avar)
	dic = dic[dic$ABBR %in% avar,1:25]
	res=NULL
	avec=c("any","words","will","do")
	if(dic$TYPE=="Quantitative-Discrete" | dic$TYPE=="Quantitative-Continuous"){
		avec=as.integer(dic$LOWER):as.integer(dic$UPPER)
	} 
	if(dic$TYPE=="Qualitative-Ordinal"){
		avec=get.codes(dic)
	} 
	simulate.var(avec,n)
}

simulate.fb <- function(fn){
	#get full path
	#fp = getFieldBookPath(fn)
	#get fb sheet
	data=getFBData(fn)
	#find pos of INSTN
	p=which(names(data)=="INSTN")
	s=p+1
	#from there onwards: check if calculated
	vars = names(data)[s:length(names(data))]
	dict = get.data.dict(vars)
	hasf = has.formula(dict)
	#if not: simulate
	
	n=ncol(data)
	m=nrow(data)
	for(i in s:n){
		#print(i)
		if(!hasf[i-p]){
			#print(vars[i-p])
			data[,i]=simulate.obs(vars[i-p],m,dict)
		}
	}
	
	#write cell data
	fp = getFieldBookPath(fn)
	wb = loadWorkbook(fp)
	sh = getSheets(wb)
	db = sh[["Fieldbook"]]
	rs = getRows(db)
	for(j in 1:m){
	  for(i in s:n){
		#print(i)
		if(!hasf[i-p]){
			cell <- createCell(rs[j+1], colIndex=i)[[1,1]]
			setCellValue(cell, data[j,i])
		}
	  }
	}
	saveWorkbook(wb,fp)
	
}

