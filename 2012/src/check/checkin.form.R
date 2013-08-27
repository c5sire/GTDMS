###############################################################################
#
#
# Sep 26, 2011
# 12:12:20 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

derive.fb.id <- function(wb){
	year = substr(wb["planting_date",2],1,4)
	mnth = substr(wb["planting_date",2],6,7)
	paste(
			toupper(abbreviate(wb["crop",2],2)), 
			toupper(abbreviate(wb["trial_type",2],2)),
			#toupper(abbreviate(wb["trial_phase",2],2)),
			year,
			mnth,
			"_",
			toupper(abbreviate(wb["country",2],3)), sep="")
}

derive.fb.path <-function(wb){
	year = substr(wb["planting_date",2],1,4)
	file.path(
			wb["crop",2], 
			year,
			sep="")
}

do.form.checks <-function(fp, dirname){
	ok=FALSE
	wb <- loadWorkbook(fp)
	if(("Minimal" %in% names(getSheets(wb)))) ok=TRUE
	if(ok){
		wb=read.fb(fp,"Minimal")
	}
	ok
}

tryCatch.W.E <- function(expr)
	 {
		     W <- NULL
		     w.handler <- function(w){ # warning handler
			       W <<- w
			       invokeRestart("muffleWarning")
			     }
		  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
						                                    warning = w.handler),
				        warning = W)
		 }



checkin.form <- function(dirname, w){
	if(is.na(dirname)){
		gmessage(translate("_MSG_NO_FILE_"))
	} else {
	if(!str_detect(dirname,".xls")){
		files = list.files(dirname, rec=T, pattern=".xls")	
	} else 	files = dirname
		closeAllExcel()
		
	n = length(files)
	ok=FALSE
	xmin=0
	xmax=n * 9
	ii=1
	pb <- winProgressBar(translate("_MSG_PREP)"), translate("_MSG_LOAD_MODULE_"),
			xmin, xmax, xmin)
	dict = get.data.dict()
	
	for(i in 1:n){
	   if(length(files)!=1){
		   fp = file.path(dirname,files[i])
		   to = file.path(get.local.db.root(),"accepted",files[i])
	   } else {
		   fp=files
		   to = file.path(get.local.db.root(),"accepted",basename(fp))
	   }
	   #print(to)
	   
	   ii=ii+1
	   info <- sprintf(paste("%d%%",translate("_MSG_DONE_")), round(ii/xmax*100))
	   setWinProgressBar(pb, ii, sprintf("Checking format (%s)", info), info)
	   #ok = do.form.checks(fp, dirname)
	  clearDerivedSheets(fp) 
		file.copy(fp, to, ov=TRUE)
		msg = ""
		ok = TRUE
		ok = try({
      #clearDerivedSheets(to)
      check.format(fp, pb,ii)
      })
		if(str_detect(ok,"Error")) {
			ok = FALSE
		}
		#print(ok)
		#ok = TRUE ##TODO investigate why ok from line before is not ok.
		#print(to)
		if(ok){
			#gc(F)
			wb = loadWorkbook(to)
			sheets = names(getSheets(wb))
			
			canBackup=NULL
			canVars =NULL
			canSummary=NULL
			canDesc = NULL
			canMiss = NULL
			canChart= NULL
			canRank = NULL
			canAnova= NULL
			ii=ii+1
			info <- sprintf("%d%% done", round(ii/xmax*100))
			setWinProgressBar(pb, ii, sprintf("Preparing archive (%s)", info), info)
			canBackup = try(calc.backup(to))	

			ii=ii+1
			info <- sprintf("%d%% done", round(ii/xmax*100))
			setWinProgressBar(pb, ii, sprintf("Calculating variables (%s)", info), info)
			canVars = try(calc.vars(wb,sheetName="Fieldbook", dict, to))
			if(length(str_detect(canVars,"Error"))>0){
				msg = paste(msg,"Calculation of derived variables in sheet 'Fieldbook' had errors. Please see manual.",sep='\n')
			}
			
			
			
# 			if("Fieldbook2" %in% sheets){
# 			canVars = try(calc.vars(to,sheet="Fieldbook2"))
# 			if(length(str_detect(canVars,"Error"))>0){
# 				msg = paste(msg,"Calculation of derived variables in sheet 'Fieldbook2' had errors. Please see manual.",sep='\n')
# 			}
# 			}
			
			# Must be after calculating the variables because it reuses the results in this sheet
			# and just paints them red when mssing values >= 10%
			#print(canVars)
			if(is.null(canVars)){
			ii=ii+1
			info <- sprintf("%d%% done", round(ii/xmax*100))
			setWinProgressBar(pb, ii, sprintf("Summarizing (%s)", info), info)
			canSummary = try(calc.summary(to))
			if(length(str_detect(canSummary,"Error"))>0){
				msg = paste(msg,"Calculation of variable summary had errors. Please see manual.",sep='\n')
			}
			}
			
#			Sys.sleep(2)
			#if(canVars=='ok' & canSummary=='ok'){
			if(is.null(canVars) & is.null(canSummary)){
			ii=ii+1
			info <- sprintf("%d%% done", round(ii/xmax*100))
			setWinProgressBar(pb, ii, sprintf("Descriptive statistics (%s)", info), info)
			canDesc = try(calc.descriptive(to))
			if(length(str_detect(canDesc,"Error"))>0){
				msg = paste(msg,"Calculation of descriptive statistics had errors. Please see manual.",sep='\n')
			}
			}
			
			ii=ii+1
			info <- sprintf("%d%% done", round(ii/xmax*100))
			setWinProgressBar(pb, ii, sprintf("Highlighting missing values (%s)", info), info)
			canMiss = try(calc.missing(to))	
			if(length(str_detect(canMiss,"Error"))>0){
				msg = paste(msg,"Highlighting of missing values had errors. Please see manual.",sep='\n')
			}
			
			setWinProgressBar(pb, ii, sprintf("Descriptive statistics (%s)", info), "Boxplots")
			try(calc.charts(to))
			if(length(str_detect(canDesc,"Error"))>0){
				msg = paste(msg,"Calculation of boxplots had errors. Please see manual.",sep='\n')
			}
			
#			print(canVars)
#			print(canSummary)
#			print(canDesc)
			if(is.null(canVars) & is.null(canSummary) & is.null(canDesc)){
				setWinProgressBar(pb, ii, sprintf("Descriptive statistics (%s)", info), "Ranks")
				canRanks = try(calc.ranks(to))	
				#print(canRanks)
				if(!(str_detect(canRanks,"ok"))){
					msg = paste(msg,"Calculation of ranks had errors. Please see manual.",sep='\n')
				}
			}
			
#TODO: make it optional in Preferences			
#			if(is.null(canVars)){
#			setWinProgressBar(pb, ii, sprintf("Analysis (%s)", info), "ANOVA")
#			# TODO check correct type by type of experiment
#			canAnova=try(calc.anova(to),TRUE)
#			if(!canAnova & !str_detect(canAnova,"Study")){#GTDM-413
#				msg = paste(msg,"ANOVA had errors. Please see manual.",sep='\n')
#			}
#			}
			
			#print("post")
			mml	= read.xlsx2(to,sheetName="Minimal")
			crp = as.character(mml[mml$Factor=="Crop","Value"])
			ttl = as.character(mml[mml$Factor=="Short name or Title","Value"])
			yer = substr(ttl,5,10)
			#ssn = substr(ttl,9,10)
			#print("x")
			out = file.path(get.local.db.root(),crp,yer,paste(ttl,".xls",sep=""))
			#print(out)
			if(!file.exists(file.path(get.local.db.root(),crp,yer))){
				dir.create(file.path(get.local.db.root(),crp,yer), rec=T)
			}
			file.copy(to, out, ov=TRUE)
			if(nchar(msg)>0) gmessage(msg,title="Data processing errors", icon="error")
			#refresh(w)
			
		} else{
			gmessage("File has format errors!",title="Format check", icon="error")
			to = fp
		}
		
	}
	close(pb)
	shell.exec(to)
}

}

