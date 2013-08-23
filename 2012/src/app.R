	###############################################################################
	#
	#
	# June 8th, 2012
    #
	# 7:59:34 AM
	# Author: Reinhard Simon (rsimon)
	# (c) International Potato Center
	#
	###############################################################################
	#setwd("L:/packages/GDET4RT")
	# 

	#setwd("F:/packages/GDET4RT")

	
	#############################
	# setup the reference directory for source code
	# switch to bin in distributed version
	##########################
	library("utils") #Necessary for version upgrade to R 2.14.1; otherwise will stop for not finding progressbar
					 #GTDM-52
	app.dir = "src"
	if(!file.exists(app.dir)) app.dir="bin"
	
	startup = function(){
		
		xmin=1
		i=1
		xmax=100
		pb <- winProgressBar("Loading application ...", "Loading module ... %",
				xmin, xmax, xmin)	
		
		update.prog <- function(i, package) {
			#info <- sprintf(paste("%d%%",translate("_MSG_DONE_")), round(i/xmax*100))
			info <- sprintf(paste("%d%%","done"), round(i/xmax*100))
			#setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_LOADING_"),"(%s)"), info), package)
			setWinProgressBar(pb, i, sprintf(paste("Loading ","(%s)"), info), package)
		}
		
		
		#############################
		# setup helper functions
		#############################
		
		
		import <-function(package,i){
			update.prog(i = i, package = package)
			if(!require(package, character.only=TRUE)) 
				install.packages(package,  depend=TRUE)
			library(package, character.only=TRUE)
		}
		
		
		include <- function(module,i){
			update.prog(i = i, package = module)
			source(file.path(app.dir,module))
		}
		
		
	#############################
	# setup the progress bar
	#############################
#	info <- sprintf("%d%% done", round(i/xmax*100))
#	
#	pb <- winProgressBar("Loading application ...", "Loading module ... %",
#			xmin, xmax, xmin)
#	setWinProgressBar(pb, i, sprintf("Loading (%s)", info), info)
#	
	update.prog(i = i, package = "DataCollector")
	
	#############################
	# external libraries
	#############################
	
	import("gWidgetstcltk",1)
	options(guiToolkit="tcltk")
	import("RSQLite",2)
	import("stringr",3)
	#import("testthat",4)
	import("xlsx",5)
	import("agricolae",6)
	import("date",7)
	import("gWidgetstcltk",8)
	#import("gWidgetsWWW",8)
	#import("randomForest",9)
	import("lme4",10)
	import("abind",11)
	import("MASS",12)
	#import("biomaRt",11)
	#############################
	# modules
	#############################
	include(file.path("utils","translate.R"),18)
	include(file.path("gui","makeGWidgetsIcon.R"),19)
	include(file.path("utils","vars.R"),20)
	include(file.path("utils","excel.utils.R"),21)
	include(file.path("utils","str.utils.R"),22)
	include("metadata.R",23)
	include(file.path("utils", "auto.add.crop.template.R"),24)
	include(file.path("utils", "auto.add.countries.to.prefs.R"),24)
	include(file.path("utils", "auto.merge.prior.preferences.R"),24)
	include("preferences.R",24)
	include(file.path("utils","get.version.nr.R"),25)
	include(file.path("utils","utils.R"),25)
	include("get.calculated.vars.R",26)
	include(file.path("gui","xgwindow.R"),27)
	include(file.path("gui","xgconfirm.R"),28)
	include(file.path("gui","xgmessage.R"),29)
	include("pfd.R",30)
	
	include(file.path("design","randomize.design.R"),31)
	include("dlgLoG.R",32)
	include(file.path("design","get.site.details.R"),33)
	include(file.path("design","createDesignExpDlg.R"),34)
	include(file.path("utils","local.repo.utils.R"),35)
	include(file.path("utils","fieldbook.status.utils.R"),36)
	include(file.path("design","design.fieldbook.R"),37)
	include(file.path("check","calc.backup.R"),38)
	include(file.path("check","calculate.R"),39)
	include(file.path("check","calc.summary.R"),40)
	include(file.path("check","calc.missing.R"),40)
	include(file.path("check","calc.ranks.R"),41)
	include(file.path("check","compare.fb.R"),41)
	
	
	include(file.path("check","checkin.form.R"),42)
	include("create.Pref.Dlg.R",43)
	include(file.path("check","check.format.R"),44)
	include("analysis.ANOVA.R",45)
	include(file.path("combine","join.fieldbooks.R"),46)
	include(file.path("combine","combineDlg.R"),47)
	include("_MET.R",48)
	include("calcMET.R",49)
	include("select1.R",50)
	include("select2.R",51)
	include("select3.R",52)
	include("select4.R",53)
	include(file.path("utils", "synchronizeFilesWithDb.R"),53)
	include(file.path("utils","utils.internet.R"),54)
	include(file.path("utils","utils.confluence.R"),55)
	
	include("metadata.R",56)
	include("add.localities.R",57)
	
	include(file.path("gui","m.actions.R"),59)
	include(file.path("gui","get.menu.R"),60)
	include("design.alpha.check.R",61)
	include(file.path("design","add.layout.R"),62)
	
	include(file.path("template","update.template.v2.0.v2.1.R"),63)
	
	#include(file.path("image","cut.R"),64)
	#include(file.path("image","add.metadata.R"),66)
	#include(file.path("image","annotator.R"),65)
	
	
	#############################
	# load user interface
	#############################
	
	include(file.path("gui","gui.R"),xmax)
	#source(file.path(app.dir,"gui.R"))
	#############################
	# finish progress bar
	#############################
	close(pb)
	
	}
	
	#initialize()
	startup()
	
	
	
