###############################################################################
#
# TODO: Add comment
#
# Oct 22, 2011
# 11:37:31 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

calc.ranks <- function(fp){

	sheetName="Ranks by clone"
	#clear.sheet(sheetName,fp)
	
	vl = read.excel(fp, "Var List")
	#vl = read.xlsx(fp,sheetName="Var List", h=T, stringsAsFactors=F)
	#an = "x"%in%vl$Analyze | "X"%in%vl$Analyze 
	vl$Selection.weight = as.numeric(vl$Selection.weight)
	an = ("x"%in%vl$Analyze | "X"%in%vl$Analyze) & (which(vl$Selection.weight>0)) 
	#print(an)
	#print(nrow(vl))
	if(length(an)>0){
		vl = vl[which(tolower(vl$Selection.weight)>0),c("Abbreviations","Selection.direction","Selection.weight")]
		#write.xls("Insufficient data for calculating ranks.",fp,sheet="Ranks by clone")
		#print("chau")
		#return()
	#} else {
	#print(vl)
	va  = vl[,"Abbreviations"]
	vn = paste(vl[,"Abbreviations"],"_Mean",sep="")
	
	#TODO handle case when less than 2 are marked
	#better handling of missing values for missing sel.dir and sel.weight
	#print("check 1")
	sc = read.excel(fp, "Summary by clone")
	db = sc[,vn]
	
	#Handle case when count of x=1
	db = as.data.frame(db)	
	names(db)=vn
	
	for(i in 1:ncol(db)) db[,i]=as.numeric(db[,i])
	#print(db)
	
	v = apply(db,2,scale)
	row.names(v) = sc$INSTN
	# remove columns where NaN
	nan = rep(FALSE,ncol(v))
	for(i in 1:ncol(db)) {
		nan[i] = is.nan(v[1,i])
	}
	v = v[,!nan]
	db = db[,!nan]
	
	#Handle case when count of x=1
	db = as.data.frame(db)	
	#print(vn)
	#print(names(db))
	#names(db)=vn
	
	va = rep("",ncol(db))
	if(ncol(db)>1){
		for(i in 1:ncol(db)) va[i] = str_replace(names(db)[i],"_Mean","")
	} else {
		names(db)=vl[1,1]
		va = names(db)
	}
	vl = vl[vl[,1]%in%va,]
	#print("check 2")
	vd = vl[,"Selection.direction"]
	vw = vl[,"Selection.weight"]
	#print(str(vd))
	#print(vw)
	#print(va)
	#print(vl)
	#print(length(vd))
	for(i in 1:length(vd)) {
		if(is.na(vd[i])) vd[i]="1"
		if(is.na(vw[i])) vw[i]="1"
		if(nchar(vd[i])==0) vd[i]="1"
		if(vd[i]=="+") vd[i]="1"
		if(vd[i]=="-") vd[i]="-1"
		if(nchar(vw[i])==0) vw[i]="1"
	}
	vd = as.integer(vd)
	#print("-------")
	#print(vd)
	#print("check 3")
	#filter only those 
	vf = vd!=0 
	vn1 = paste(vl[,1],"_Mean",sep="")
	#print(vn1)
	vn = vn1[vf]
	db = sc[,vn1]
	#print(db)
	#print("==========================")
	#Handle case when count of x=1
	if(length(vn1)==1) db=as.numeric(db)
	db = as.data.frame(db)	
	names(db)=vn
	for(i in 1:ncol(db)) db[,i]=as.numeric(db[,i])
	#print(db)
	#print(str(db))
	#print("==========================")
	vw = as.numeric(vw[vf])
    #print(db)
	#print(str(db))
	#print(str(vd))
	#for(i in 1:ncol(db)) db[,i]=as.numeric(db[,i])
	for(i in 1:ncol(db)) db[,i]=db[,i]*vd[i]
	#print(db)
	#print("==========================")
	
	v = apply(db,2,scale)
	#print(v)
	#print("==========================")
	row.names(v) = sc$INSTN
	nn = ncol(v)
	
	rsi = rowSums(v, na.rm=T)
	rss = scale(rsi)

	# SSD index calculation
	swd <- numeric(nrow(db))
	for (j in 1:nrow(db)) {
	#for all genotypes
		for (i in 1:ncol(db)) {
	# for one genotype add up over variables
		   if(!is.na(v[j,i])){
			swd[j] <- swd[j] + (v[j, i] * vw[i])
		   }
		}
	}
	swd <- scale(swd) #rescale

	eim <- numeric(nrow(db))
	for (i in 1:length(eim)) eim[i] <- 1
	for (j in 1:nrow(db)) {
		for (i in 1:ncol(db)) {
			if(!is.na(v[j,i])){
				eim[j] <- eim[j] * v[j, i]
			}
		}
	}
	eim <- scale(eim)	
	
	vb = db
	for(i in 1:ncol(db)) vb[,i]=as.numeric(db[,i])*vd[i]
	v = cbind(vb, v, rss, swd, eim)
	v = as.data.frame(v)
	names(v)[(ncol(v)-2):ncol(v)]=c("RSS","SWD","EIM")
#	print(v)
	v = v[do.call(order, -v["RSS"]), ]
	
	v = cbind(row.names(v),v)
	names(v)[1]="INSTN"
	names(v)[(nn+2):ncol(v)]=as.character(sapply(names(v)[(nn+2):ncol(v)], str_replace,"_Mean","_Rank"))
	#write.xlsx2(v,file=fp,sheetName="Ranks by clone",append=T, row.names=F)
	#print("write ranks")
	#try(write.xls(v,fp,sheet=sheetName))
    
  try({
    wb = loadWorkbook(fp)
    sheets <- getSheets(wb)
    # create the list of column styles
    fbs = getFbStyles(wb) 

    cs = list()
    cs[[1]] = fbs$RHdr
    names(cs)[1] = 1
    sheet = createSheet(wb,sheetName)
    
    for(i in 2:ncol(v)) {
      if(i<=(1+length(an))){
        cs[[i]] = fbs$Nbr2
      } else {
        cs[[i]] = fbs$Clc0  
      }
      
      names(cs)[i] = i
    }
    
    addDataFrame(v,sheet, colnamesStyle=fbs$CHdr, rownamesStyle = fbs$RHdr, colStyle = cs, row.names=F)
    autoSizeColumn(sheet, 1:ncol(v))
    saveWorkbook(wb, fp)
    
  })  
    
	#print("Write ranks ok")
	}
	"ok"
}
