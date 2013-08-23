###############################################################################
#
# TODO: Add comment
#
# Oct 25, 2011
# 9:06:06 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

add.MET.excel <- function(traits,res,fp){
	
for(i in 1:length(traits)){
	#write out the MET analysis
	fn = file.path("temp",paste(traits[i],".txt",sep=""))
	sink(fn)
	cat("MET for trait:",traits[i],"\n")
	cat("# of genotypes:", res[[traits[i]]]["Geno_num"][[1]],"\n")
	cat("# of environments:", res[[traits[i]]]["Env_num"][[1]],"\n")
	cat("Heritability:", res[[traits[i]]]["Herit"][[1]],"\n")
	
	cat("\nOverall statistics:\n" )
	print(res[[traits[i]]]["Overall_stat"][[1]])
	cat("\n" )
	print(res[[traits[i]]]["B_test"][[1]])
	cat("\n" )
	print(res[[traits[i]]]["SW_test"][[1]])
	cat("\n" )
	cat("\nInter means:\n" )
	print(res[[traits[i]]]["Inter_means"][[1]])
	cat("\n" )
	cat("\nInter effects:\n" )
	print(res[[traits[i]]]["Inter_eff"][[1]])
	cat("\n" )
	cat("\nANOVA 1:\n" )
	print(res[[traits[i]]]["Add_ANOVA1"][[1]])
	cat("\n" )
	cat("\nANOVA 2:\n" )
	print(res[[traits[i]]]["Add_ANOVA2"][[1]])
	cat("\n" )
	cat("\nANOVA 3:\n" )
	print(res[[traits[i]]]["Add_ANOVA3"][[1]])
	cat("\n" )
	cat("\nVariance composition:\n" )
	print(res[[traits[i]]]["Var_Comp"][[1]])
	cat("\n" )
	cat("\nMulti Inter" )
	print(res[[traits[i]]]["Multi_Inter"][[1]])
	cat("\n" )
	cat("\nStability of genotype" )
	print(res[[traits[i]]]["Stab_geno"][[1]])
	cat("\n" )
	cat("\nStability of environment" )
	print(res[[traits[i]]]["Stab_env"][[1]])
	
	sink()
	#create a sheet where to put that output
	sh = traits[i]
	wb = loadWorkbook(fp)
	removeSheet(wb,sh)
	saveWorkbook(wb, fp)
	cnt = readLines(fn)
	write.xlsx2(cnt, fp, sheetName=sh, app=T, row.names=F, col.names=F)
	unlink(fn)
	wb = loadWorkbook(fp)
	sheets = getSheets(wb)
	sh = sheets[[traits[i]]]
	cs <- CellStyle(wb) + Font(wb, name="Courier")
	#print("check 1")
	rows<-getRows(sh)
	for(k in 1:length(rows)) setCellStyle(getCells(rows[k],1)[[1]],cs)
	
	#add the images
	#c=13
	
#	addPicture(file.path(getBaseDir(),"temp",paste(traits[i],"_dp2.png",sep="")),sh, scale=1, startColumn=c, startRow=55)
#	addPicture(file.path(getBaseDir(),"temp",paste(traits[i],"_biplot1.png",sep="")),sh, scale=1, startColumn=c, startRow=110)
#	addPicture(file.path(getBaseDir(),"temp",paste(traits[i],"_biplot2.png",sep="")),sh, scale=1, startColumn=c, startRow=165)
#	addPicture(file.path(getBaseDir(),"temp",paste(traits[i],"_tai.png",sep="")),sh, scale=1, startColumn=c, startRow=220)
	#saveWorkbook(wb,fp)
	saveWorkbook(wb,fp)
}
	
	wb = loadWorkbook(fp)
	sheets = getSheets(wb)
	
	#
	for(i in 1:length(traits)){	
		sh = sheets[[traits[i]]]
		addImage <- function(trait, sh, c=13, r=1, ctg ) {
			#fm=file.path(getBaseDir(),"temp",paste(trait,"_",ctg,".png",sep=""))
			#print(getwd())
			#print(getBaseDir())
			fm=file.path(getBaseDir(),"temp",paste(trait,"_",ctg,".png",sep=""))
			ap = addPicture(fm,sh, scale=1, startColumn=c, startRow=r)
			unlink(fm)
		}
		try(addImage(traits[i], sh, ctg="dp1",r=1)) #;saveWorkbook(wb,fp)
		try(addImage(traits[i], sh, ctg="dp2",r=55))#;saveWorkbook(wb,fp))
		try(addImage(traits[i], sh, ctg="biplot1",r=110))#;saveWorkbook(wb,fp)
		try(addImage(traits[i], sh, ctg="biplot2",r=165))#;saveWorkbook(wb,fp)
		try(addImage(traits[i], sh, ctg="tai",r=220))#;saveWorkbook(wb,fp)
		
	}
	try(saveWorkbook(wb,fp))
	
}

get.traits.for.met<-function(fp){
	data	= read.excel(fp, sheetName="Var List")
	x=data[tolower(data$Analyze)=="x","Abbreviations"]
	x[!(x %in% NA)]
}

check.MET.conditions <- function(data, traits){
	msg = ""
	# Minimum 5 genotypes
	genos = unique(data$INSTN)
	if(length(genos)<5){
		msg = paste(msg, "Expected at least 5 distinct genotypyes. Found",length(genos),".\n")
	}
	# Minimum 3 environments
	envs = unique(data$ENV)
	if(length(envs)<3){
		msg = paste(msg,"Expected at least 3 distinct environments. Found",length(envs),".\n")
	}
	# Minimum 1 trait
	traits = unique(traits)
	if(length(traits)<1){
		msg = paste(msg,"Expected at least 1 distinct trait. Found",length(traits),".\n")
	}
	msg
}


calcMET <-function(fp){
	data	= read.excel(fp, sheetName="Fieldbook")
#	instn.p = which(names(data)=="INSTN")+1
#	traits  = names(data)[instn.p:ncol(data)] #TODO remove magick number (pos of INSTN+1)
#	traits  = traits[!(traits %in% 
#		c("Observations","OBS","Comments","Observation","Comment","Notes","Note"))]
	
	traits = get.traits.for.met(fp)
	dnms = c("ENV","REP","INSTN",traits)
	data = data[,dnms]
	msg = check.MET.conditions(data, traits)
	if(nchar(msg)>0){
		gmessage(msg, title="Assumption check",icon="error")
	} else {
	# exclude all data from further analysis where no variation: min = max
	#novar = rep(FALSE,length(traits))
	#for(i in 1:length(traits)){
	#	novar[i] = min(data[,traits[i]])==max(data[,traits[i]])
	#}
	#traits = traits[!novar]
		try({
			res = doMet(data,traits)
			add.MET.excel(traits, res, fp)
		})
		shell.exec(fp)
	}
}


#######################################################

get.cb.list <-function(){
	ff=file.path(get.local.db.root(),"combined",getCurrentCrop())
	ff=list.files(ff,rec=T)
	ff
}

getMETLayout <-function(){
	lst = get.cb.list()
	its = str_replace(lst,".xls","")
	
	a.list=list(type="gradio",
							name="combs",
							#use.table = T,
							label = "Combined trials",
							items = its#,
							#selected=FALSE
			)
	a.list
}



METDlg <-function(w){
	wDlg <- xgwindow("Select a combined fieldbook.",visible=F, parent=w)
	layout = getMETLayout()

	g <- ggroup(horizontal = FALSE, cont = wDlg)
	wg = ggroup(cont=g)
	#print("check 1")
	wdg = gformlayout(layout, container=wg)
	#print("check 2")
	
	bg <- ggroup(cont = g)
	addSpring(bg)
	
	gbutton("cancel", handler =  function(h,...) {
				out = "cancel"
				#dput(out,file="bin/temp.txt")
				dispose(wDlg)
			},
			, container=bg)
	gbutton("ok", handler =  function(h,...) {
				out <- svalue(wdg)
				#print(out)
				dispose(wDlg)
				xmin=0
				xmax=3
				closeAllExcel()
				
				pb <- winProgressBar("Preparing ...", "MET ... %",
						xmin, xmax, xmin)
				info <- sprintf("%d%% done", round(1/xmax*100))
				setWinProgressBar(pb, 1, sprintf("Preparing (%s)", info), "MET")
				
				fp=file.path(get.local.db.root(),"combined",getCurrentCrop(),paste(out$combs,".xls",sep=""))
				calcMET(fp)
				close(pb)
				
			},
			, container=bg)
	
	visible(wDlg)=T
	#close(pb)
	
}




