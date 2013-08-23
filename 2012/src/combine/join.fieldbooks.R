###############################################################################
#
# TODO: Add comment
#
# Oct 24, 2011
# 9:19:01 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################
#rbooks=basename(list.files("C:\\DataCollector\\DC4RT\\data\\potato",rec=T))

has.additional.factors <- function(fact){
	length(unique(fact[,3]))>1
}

#GTDM-223
join.df <-function(books){
	bnms = names(books)
	n = length(bnms)
	
	#nms = c("ENV","YEAR","MONTH","LOCATION")
	nms = NULL
	rr  = 0
	#nmsx = character()
	fact = matrix("NA",ncol=4,nrow=n)
	for(i in 1:n) {
		nmsx = names(books[[bnms[i]]])
		nms = c(nms,nmsx)
		fac = nmsx[1:which(nmsx=="INSTN")]
		fact[i,1:length(fac)]= fac
		rr = rr+nrow(books[[bnms[i]]])
	}
	# find out if more than 3 factors
	if(has.additional.factors(fact)){
		#TODO
		add.f = sort(unique(fact[,3]))
		unms = unique(nms)
		strt.p = 4 + length(add.f)
		nms = c("PLOT","REP",add.f,"INSTN", sort(unms[strt.p:length(unms)]))
	} else {
		unms = unique(nms)
		nms = c(unms[1:3], sort(unms[4:length(unms)]))	
	}
	nms = c("ENV","YEAR","MONTH","LOCATION",nms)

	cc = length(nms)
	b3 = matrix(NA,nrow=rr, ncol=cc)
	b3 = as.data.frame(b3,stringsAsFactors=F)
	names(b3) = nms

 	#Filling in
	rs = 1
	re = 0
	for(i in 1:n){
		re = re+nrow(books[[bnms[i]]])
		#print(rs)
		#print(re)
		#print(nrow(books[[bnms[i]]]))
		b3[rs:re,"ENV"] = bnms[i]
		b3[rs:re,"YEAR"] = str_sub(bnms[i],5,8)
		b3[rs:re,"MONTH"] = str_sub(bnms[i],9,10)
		b3[rs:re,"LOCATION"] = str_sub(bnms[i],12,nchar(bnms[i]))
		#b3[rs:re,names(books[[bnms[i]]])] = books[[bnms[i]]]
		nb = names(books[[bnms[i]]])
		for(j in 1:length(nb)) b3[rs:re,nb[j]] = books[[bnms[i]]][,nb[j]]
		rs = re+1
	}
	b3=guessVariableType(b3)
 b3
}

#books = c("PTYL200910_KIBRCH","PTYL200910_KISIMA","PTYL200910_LIMURU",
#		  "PTYL200910_OLJORO","PTYL200910_BARAKA","PTYL200910_NAROK")

#books = c("PTYL200205_CIPHQ","PTYL200208_CIPSRM-1","PTYL200211_CHIARA","PTYL200211_LAVICT")
#books = c("PTLB201103_S_PHAO","PTLB201003_S_PHAO","PTLB200803_S_PHAO")



join.fieldbooks <- 
		# books: a vector of fieldbook names
		# dbname:a character string for the new joint 'database'
		# constructs a new Excel sheet with joint original data
		# saves it under: [data]/joins/[dbname].xls
		function(books, dbname){
	try({
	#for each fieldbook name
	books = sort(unique(books))				
	n = length(books)
	db = list()
	for(i in 1:n){
	#find the full path
		fp = getFieldBookPath(books[i])
		db[[i]] = try(read.excel(fp, sheetName="Fieldbook"))
		nm = str_replace(books[i],".xls","")
		names(db)[i]=nm
		#print(db)
	}
	
	data = join.df(db)
	dr = file.path(get.local.db.root(),"combined",getCurrentCrop())
	if(!file.exists(dr)) dir.create(dr, recursive=T)
	
	to = file.path(dr,paste(dbname,'.xls',sep=""))
	if(file.exists(to)) unlink(to)
	wb = createWorkbook(type="xls")
	sh = createSheet(wb,"test")
	saveWorkbook(wb,to)
	
	write.xls(data, to, "Fieldbook")
	
	wb = loadWorkbook(to)
	removeSheet(wb,"test")
	saveWorkbook(wb,to)

	dict = get.data.dict(names(data))[,c("VAR","ABBR","SELDIR","SELWGT")]
	names(dict) = c("Variable name","Abbreviations","Selection direction","Selection weight")
	Analyze=rep("x",nrow(dict))
	dict=cbind(dict,Analyze)
	dict = dict[do.call(order, dict[c("Abbreviations")]), ]
	
	write.xls(dict, to, "Var List")
	
	try(calc.descriptive(to))
	try(calc.missing(to))
	
	#print("combining")
	nmsx = names(data)
	instn.p = which(nmsx=="INSTN")+1
	# means
	xnms = get.data.dict(names(data))$ABBR
	x = data[,xnms]
	y = data[,c("ENV","INSTN")] #TODO for factorial designs
	rmean = function(x) round(mean(x),2)
	s.means=tapply.stat(x,y,rmean)
	instn.p = which(names(s.means)=="INSTN")+1
	#s.means
	s.means = s.means[,c(2,1,instn.p:ncol(s.means))]
	s.means[,1]=as.character(s.means[,1])
	s.means[,2]=as.character(s.means[,2])
	#s.means
	s.means = s.means[do.call(order, s.means[c("INSTN","ENV")]), ]
	instn.p = which(names(s.means)=="INSTN")+2
	names(s.means)[instn.p:ncol(s.means)] = paste(names(s.means)[instn.p:ncol(s.means)],"_Mean",sep="")
	#print("combining summary")
	write.xls(s.means, to, "Summary")

	y = data[,c("ENV")] #TODO for factorial designs
	rmean = function(x) round(mean(x),2)
	e.means=tapply.stat(x,y,rmean) 
	names(e.means)[1] = "ENV"
	names(e.means)[2:ncol(e.means)] = paste(names(e.means)[2:ncol(e.means)],"_Mean",sep="")
	#print("combining summary env")
	write.xls(e.means, to, "Summary by environment")
	
	
	y = data[,c("INSTN")] #TODO for factorial designs
	rmean = function(x) round(mean(x),2)
	g.means=tapply.stat(x,y,rmean) 
	names(g.means)[1] = "INSTN"
	names(g.means)[2:ncol(g.means)] = paste(names(g.means)[2:ncol(g.means)],"_Mean",sep="")
	#print("combining summary clone")
	write.xls(g.means, to, "Summary by clone")
	
	#print("ok")
	calc.ranks(to)
	
	})
	shell.exec(to)
}

