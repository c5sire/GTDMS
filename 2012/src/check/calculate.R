###############################################################################
#
# TODO: Add comment
#
# Oct 9, 2011
# 8:31:13 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

str2days <- function(adate=""){
  res = NA
  try({
    x = as.integer(str_split(adate,"-")[[1]])
    res = mdy.date(x[2],x[3],x[1])
  })
  res
}


get.rel.days <- function(mgt){
	start.date = mgt[mgt$Intervention.type=="Planting","Date"]
	std = as.integer(strsplit(start.date,"-")[[1]])
	std = as.integer(mdy.date(std[2],std[3],std[1]))
	
	lb = paste("Percentage of foliage affected by Late Blight",1:12)
	mgt$Date = as.character(mgt$Date)
	mgt$Intervention.type = as.character(mgt$Intervention.type)
	ds = mgt[mgt$Intervention.type %in% lb,"Date"]
	di = integer(length(ds))
	for(i in 1:length(ds)){
		dx = as.integer(strsplit(ds[i],"-")[[1]])
		if(length(dx)==3){
			di[i] = mdy.date(dx[2],dx[3],dx[1])	
		} else {
			break
		}
	}
	#std = di[1]
	di=di-std
	#di[1] = di[1]+1
	#print(di)
	di #GTDM-43
}

get.lb.control <-function(mtl){
	mtl$Scale.AUDPC.control = as.integer(mtl$Scale.AUDPC.control)
	mtl$Institutional.number= as.character(mtl$Institutional.number)
	mtl[!is.na(mtl$Scale.AUDPC.control),c("Institutional.number","Scale.AUDPC.control")]	
}

saudpc <-function(instn, audpc, reps, lb.ctrl){
	audpc=as.numeric(as.character(audpc))
	sc.ctrl = as.integer(lb.ctrl["Scale.AUDPC.control"])
	saudpc = numeric(length(audpc))
	reps = as.integer(as.character(reps))
	rs=sort(unique(reps))
	
	ref.audpc=NULL
	for(i in 1:length(rs)){
		f = which(reps==rs[i])
		#print(f)
		a = audpc[f]
		#print(a)
		n = instn[f]
		#print(n)
		p = which(n==lb.ctrl["Institutional.number"][[1]])
		if(length(p)==0){
			au.ctrl = ref.audpc
			#print(i)
		} else {
			#print(i)
			au.ctrl = a[p]	
			ref.audpc = au.ctrl
		}
		
		#print(au.ctrl)
		a=a/au.ctrl * sc.ctrl
		#print(sc.ctrl)
		#print(a)
		saudpc[f]=a
	}
	
	round(saudpc,1)
}

xaudpc <- function(eval, dates, type){
	pts = length(dates)
	audpc(eval[,1:pts], dates, type)
}


######################
calculate <-function(fieldbook, inst=NULL, mgt=NULL, mtl=NULL,
		type="yield", digits=2){
digs.old = options("digits")[[1]]
#options(digits=digits)
plot.size = as.numeric(inst[str_detect(inst$Factor,"Plot size"),"Value"])
plant.den = as.numeric(inst[str_detect(inst$Factor,"Planting density"),"Value"])
date.dehaulm = str2days(mgt[str_detect(mgt$Intervention.type,"Date of dehaulming"),"Date"])
date.storage = str2days(mgt[str_detect(mgt$Intervention.type,"Date of storage"),"Date"])
fieldbook$INSTN = as.character(fieldbook$INSTN)
fb = fieldbook

# correct for empty columns that are of type character or logic
# first find where INSTN
fbn = names(fb)
p = which(fbn %in% "INSTN") + 1
for(i in p:length(fbn)){
  if(!str_detect(colnames(fb)[i],"DATE")) {
    fb[,i] = as.numeric(fb[,i])  
  }
	
}

# calculate variable columns from formula
#source("res/formula.R")

if(length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
  PPE   = (NPE*100)/NTP	})			

if(length(fb$NTP)>0 & length(fb$PPH)>0 ) fb=within(fb,{	
  PPH 	= (NPH*100)/NTP	})			
if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{	
  TNTP	= NMTCI + NMTCII + NNoMTP	})			
if(length(fb$NMTP)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{	
  TNTP	= NMTP+ NNoMTP})			


if(length(fb$TNTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  TNTPL	= TNTP/NPH	})			
if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0  ) fb=within(fb,{	
  NMTP	= NMTCI + NMTCII	})			
if(length(fb$NMTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  NMTPL	= NMTP/NPH	})			
if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{	
  TTWP	= MTWCI + MTWCII + NoMTWP	})	
if(length(fb$MTWP)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{	
  TTWP	= MTWP + NoMTWP	})	

if(length(fb$TTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  TTWPL	= TTWP/NPH	})			
if(length(fb$TTWP)>0  ) fb=within(fb,{	
  TTYNA	= TTWP/plot.size*10	})	#GTDM-39		
if(length(fb$TTWPL)>0  ) fb=within(fb,{	
  TTYA	= TTWPL*plant.den/1000}) # GTDM-45			
if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 ) fb=within(fb,{	
  MTWP	= MTWCI + MTWCII})			
if(length(fb$MTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  MTWPL	= MTWP/NPH})			
if(length(fb$MTWP)>0 ) fb=within(fb,{	
  MTYNA	= MTWP/plot.size*10})	#GTDM-39		
if(length(fb$MTWPL)>0 ) fb=within(fb,{	
  MTYA	= MTWPL*plant.den/1000})#GTDM-39			
if(length(fb$TTWP)>0 & length(fb$TNTP)>0) fb=within(fb,{	
  ATW		= TTWP/TNTP*1000})			
if(length(fb$MTWP)>0 & length(fb$NMTP)>0) fb=within(fb,{	
  ATMW	= MTWP/NMTP*1000})			
if(length(fb$DWTS1)>0 & length(fb$FWTS1)>0) fb=within(fb,{	
  DM1		= DWTS1/FWTS1 * 100})			
if(length(fb$DWTS2)>0 & length(fb$FWTS2)>0) fb=within(fb,{	
  DM2		= DWTS2/FWTS2 * 100})			
if(length(fb$DM1)>0 & length(fb$DM2)>0) fb=within(fb,{	
  AVDM	= (DM1 + DM2)/2})			
if(length(fb$TWA)>0 & length(fb$TWA)>0 & length(fb$TWW)>0) fb=within(fb,{	
  SG		= TWA/(TWA-TWW)})			
if(length(fb$IWS1)>0 & length(fb$FWS1)>0 ) fb=within(fb,{	
  OCS1	= 100 - ((IWS1/FWS1)*100)})			
if(length(fb$IWS2)>0 & length(fb$FWS2)>0 ) fb=within(fb,{	
  OCS2	= 100 - ((IWS2/FWS2)*100)})			
if(length(fb$OCS1)>0 & length(fb$OCS2)>0 ) fb=within(fb,{	
  AOCP	= (OCS1 + OCS2)/2})			


# dormancy variables
if(length(fb$LGLATSP1)>0 & length(fb$LGLATSP2)>0 & length(fb$LGLATSP3)>0
   & length(fb$LGLATSP4)>0 & length(fb$LGLATSP5)>0 & length(fb$LGLATSP6)>0) fb=within(fb,{  
     AVLGLATSP = apply(cbind(LGLATSP1,LGLATSP2,LGLATSP3,LGLATSP4,LGLATSP5,LGLATSP6),1,mean,na.rm=T)
   })
if(length(fb$THLSP1)>0 & length(fb$THLSP2)>0) fb=within(fb,{  
  AVTHSP = apply(cbind(THLSP1,THLSP2),1,mean, na.rm=T)
})
if(length(fb$ITW)>0 & length(fb$INTW)>0) fb=within(fb,{  
  PW_USPT = ((ITW-INTW)/ITW)*100
})
if(length(fb$ITW)>0 & length(fb$FTW)>0) fb=within(fb,{  
  PW_SPT = ((ITW-FTW)/ITW)*100
})
if(length(fb$DATESP)>0) fb = within(fb,{
  DORPD = apply(cbind(DATESP),1,str2days) - date.dehaulm
})

############### end dormancy

###############################################################################
# Start related variables for sweetpotato

if(length(fb$CRW)>0 & length(fb$NCRW)>0 ) fb=within(fb,{	
  TRW	= apply(cbind(CRW,NCRW), 1, sum, na.rm=T)})

if(length(fb$CRW)>0) fb=within(fb,{	
  CYTHA	= CRW*10/plot.size})

if(length(fb$CRW)>0 & length(fb$NCRW)>0) fb=within(fb,{	
  RYTHA	= apply(cbind(CRW,NCRW), 1, sum, na.rm=T)*10/plot.size})

if(length(fb$CRW)>0 & length(fb$NOCR)>0) fb=within(fb,{	
  ACRW	= CRW/NOCR})

if(length(fb$NOCR)>0 & length(fb$NONC)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  NRPP	= apply(cbind(NOCR,NONC), 1, sum, na.rm=T)/NOPH})

if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  YPP	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)/NOPH})

if(length(fb$NOCR)>0 & length(fb$NONC)>0) fb=within(fb,{	
  CI	= NOCR/apply(cbind(NOCR,NONC), 1, sum, na.rm=T)*100})

if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  HI	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)/apply(cbind(VW, CRW, NCRW), 1, sum, na.rm=T)*100})

if(length(fb$NOPH)>0 & length(fb$NOPS)>0) fb=within(fb,{	
  SHI	= NOPH/NOPS*100})

if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  BIOM	= apply(cbind(VW, CRW, NCRW), 1, sum, na.rm=T)*10/plot.size})

if(length(fb$VW)>0) fb=within(fb,{	
  FYTHA	= VW*10/plot.size})

if(length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  DM	= DMD/DMF*100})

if(length(fb$VW)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  DMFY	= VW*10/plot.size*DMVD/DMVF})

if(length(fb$DMRY)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  DMRY	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)*10/plot.size*DMD/DMF})

if(length(fb$RFR)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  RFR	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)*(DMD/DMF)/(VW*DMVD/DMVF)})





fieldbook = fb

lbf = c("LB1","LB2","LB3","LB4","LB5","LB6","LB7","LB8","LB9","LB10","LB11","LB12")
yy = names(fieldbook)
if(type=="late blight"){
	rel.days=get.rel.days(mgt)
	lb.control=get.lb.control(mtl)
	#print(str(fieldbook))	
	fieldbook=within(fieldbook,{
		AUDPC	= xaudpc(
					eval = fieldbook[,yy[yy %in% lbf]] ,
					#eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
					dates= rel.days,
					type = "absolute")
		rAUDPC	= xaudpc(
					eval = fieldbook[,yy[yy %in% lbf]] ,
					#eval = cbind(LB1,LB2,LB3,LB4,LB5,LB6,LB7) ,
					dates= rel.days,
					type = "relative")
		SAUDPC	= saudpc(INSTN,AUDPC,REP, lb.control)
					
	})
}


fieldbook[,fbn] # make sure not any variables were attached
}

save.calcs.to.excel <- function(df, fp, nm,sheetN){
	fieldbook	= read.excel(fp, sheetName=sheetN)
	hd = rep(TRUE, ncol(fieldbook))
	for(i in 1:ncol(fieldbook)) hd[i]=has.data(fieldbook[,i])
	hd = which(hd)
	# get the col indices for the calculated vars
	cnms = nm
	dict = get.data.dict(cnms)
  
	hasf = has.formula(dict)
	# correct for the missing initial columns (3 or 4)
	p = length(cnms)-length(hasf)
	hasf = c(rep(FALSE,p),hasf)
	hf = which(hasf)
	n  = length(hf)
	m  = nrow(df)
	# get the worksheet
	wb = loadWorkbook(fp)
	sheets <- getSheets(wb)
	sheet <- sheets[[sheetN]]
	# for each calc column 
	rows=getRows(sheet)
	csl = get.cell.styles(wb)
	for(i in 1:n){
		col = hf[i]
    #print(col)
    #print(cnms[col])
    dgs = as.integer(dict[dict$ABBR ==cnms[col],c("DIGITS")])
    #print(dgs)
    #csf = csd[["dataFormat"]][[1]] = "###0"
#     csf = "###0"
#     if(dgs>0){
#       csf = "###0."
#       for(i in 1:dgs) csf = paste(csf,0, sep="")
#     }
    #print(csf)
		cs=csl$integer_YEL
		#cs[["dataFormat"]][[1]] = csf
		for(j in 1:m){
			r = j+1
			#if(!(col %in% hd)) {
				cell <- createCell(rows[r], colIndex=col)[[1,1]]
				value<- df[j,col]
				setCellValue(cell, value)
				
      #print(cs)
				setCellStyle(cell, cs)
			#}
		}
	}
	# finally save the workbook
	autoSizeColumn(sheet, 1:ncol(df))
	saveWorkbook(wb, fp)
}


calc.vars <- function(wb, sheetName="Fieldbook", dict=NULL, fp=NULL){
	#fp = getFieldBookPath(fn)
	#fieldbook	= read.xlsx(fp,sheetName=sheet, stringsAsFactors=F) # reverted to xlsx so all formulas are read as values
  sn = getSheets(wb)[[sheetName]]
	fieldbook = get.sheet.data(sn,sheetName)
	#GTDM-39, GTDM-44
	inst 	  	= read.xlsx(fp,sheetName="Installation", stringsAsFactors=F) # reverted to xlsx so all formulas are read as values
	mgt			= read.xlsx(fp,sheetName="Crop_management", stringsAsFactors=F)
	mtl			= read.xlsx2(fp,sheetName="Material List", stringsAsFactors=F)
	mml			= read.xlsx(fp,sheetName="Minimal", stringsAsFactors=F) # reverted to xlsx so all formulas are read as values
	typ			= as.character(mml[mml$Factor=="Type of Trial","Value"], stringsAsFactors=F)
	fb = calculate(fieldbook,inst=inst,mgt=mgt, mtl=mtl, type=typ)
	#save.calcs.to.excel(fb, fp, names(fieldbook),sheetN=sheet)
  saveFieldbook(fb, wb, fp, dict)
}

	

