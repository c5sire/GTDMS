###############################################################################
#
# TODO: Add comment
#
# May 9, 2012
# 11:22:45 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

make.layout <-function(data, nblock=1,layout="4 5 6",design="CRD"){
	ntrt = unique(as.character(data[,3]))
	txt = paste(data[,1],data[,3],sep=":")
	mtr = t(matrix(txt,c(length(ntrt),nblock)))	
	if(str_detect(design,"LSD")){
		ntrt = unique(as.character(data[,4]))
		txt = paste(data[,1],data[,2],sep=":")
		txt = paste(txt,data[,4],sep=" ")
		n = length(unique(as.character(data[,"CBLOCK"])))
		m = length(unique(as.character(data[,"INSTN"])))
		#m = nrow(data)/n
		mtr = t(matrix(txt,c(m,n)))
	} else	
	if(str_detect(design,"A01D")){
		ntrt = unique(as.character(data[,4]))
		txt = paste(data[,2],data[,1],sep=":")
		txt = paste(txt,data[,4],sep=" ")
		n = length(unique(as.character(data[,"BLOCK"])))
		m = nrow(data)/n
		mtr = t(matrix(txt,c(m,n)))
	} else if(str_detect(design,"ABD")){
		n = max(table(data$REP))
		m = length(unique(data$REP))
		mtr = matrix("",m,n)
		data[,2]=as.character(data[,2])
		data[,3]=as.character(data[,3])
		for(i in 1:m){
			db = data[data$REP==i,]
			if(nrow(db)<n){
				for(j in 1:(n-nrow(db))){
					db = rbind(db,rep("",3))
				}
			}
			mtr[i,] = paste(db[,1],db[,3])
		}
	} else if(str_detect(design,"SP")){
		r = length(unique(data[,2]))
		m = length(unique(data[,3]))
		g = length(unique(data[,4]))
		n = m * g
		mtr = matrix("",r,n)
		for(j in 1:r){
			db = data[data[,2]==j,]
			txt=paste(db[,1],db[,3],sep=":")
			mtr[j,]=paste(txt,db[,4],sep=" ")
		}
		
	} else 	
	if(str_detect(design,"F2")){
		txt = paste(txt,data[,4],sep=" ")
	} 
	if(str_detect(design,"BIBD")){
		#print("X")
		txt = paste(data[,1],data[,2],sep=":")
		txt = paste(txt,data[,3])
		m = length(data)/nblock
		mtr = t(matrix(txt,c(nblock,m)))	
		#print(mtr)
	} 
	if(length(ntrt)==nrow(data)){
		q = length(ntrt)/nblock
		#print(data)
		#print(ntrt)
		#print(nblock)
		if(length(ntrt) %% nblock!=0){
			q = round(q,0)+1
			m = q*nblock - length(ntrt)
			x = rep("",m)
			txt = c(txt,x)
		}
		mtr = t(matrix(txt,c(q,nblock)))
	}
	
	if(str_detect(layout,"6 5 4")){
	n = nrow(mtr)
	for(i in 1:n){
		if(i%%2==0) mtr[i,]=rev(mtr[i,])
	}
	}
	mtr
}

add.layout <-function(data, design, nblock, layout, wb, fp){
	#override any prior sheet 'Layout'
	clear.sheet("Field_layout",fp)
#	print(data)
#	print(layout)
#	print(design)
#	print(nblock)
	data = make.layout(data, design = design, layout = layout,nblock=nblock) 
	write.xls(data,fp,"Field_layout", header=F, format=F)
	#wb = loadWorkbook(fp)
	#UDNR
	#CRD
	#RCBD
	#LSD
	#BIBD
	#SPCRD
	#SPRCBD
	#F2CRD
	#F2RCBD
	#ABD
	#MBCRD
	#A01D
	#wb
}