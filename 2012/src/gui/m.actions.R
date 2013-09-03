###############################################################################
#
# May 1, 2012
# 12:31:52 PM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

m.new <-function(w){
	createDesignExpDlg(w)
}

m.check <-function(w){
	checkin.files(w)
}

m.open <-function(w){
	res = create.fb.Dlg(w)
	fr = getFieldBookPath(res)
	shell.exec(fr)
}

m.comb <-function(w){
	combineDlg(w)
}

m.open.comb <-function(w){
	res = create.cfb.Dlg(w)
	fr = getCmbFieldBookPath(res)
	shell.exec(fr)
}

m.add.images <-function(w){
	get.images(w)			
}
m.add.metada.images <-function(w){
	metadata.by.image.Dlg(w)			
}
m.choose.images <-function(w){
	select.image.catalogue(w)			
}
m.change.title.metadata.images <-function(w){
	change.metadata.title(w)	
}	
m.set.repo <-function(w){
	gfile("Select a high level working directory",type="selectdir",  
			action="set.fb.dir", handler = function(h,...) do.call(h$action,list(h$file,w)))	
}

m.quit <-function(w){
	
	cmd = "TSKILL cmd /A"
	shell(cmd)
	cmd = "TSKILL Rterm /A"
	shell(cmd)
	dispose(w)
}

###################
# Edit

m.update.template <-function(w){
	update.template.dlg(w)
}

################### 
# Analysis

m.anova <-function(w){
	do.ANOVA(w)
}

m.met <-function(w){
	METDlg(w)
}


################### 
# Preferences


m.pref.general <-function(w){
	create.Pref.Dlg(w)
}

m.pref.fb.vars <-function(w){
	create.Var.Pref.Dlg(w)
}

m.pref.locs <-function(w){
	create.Loc.Pref.Dlg(w)
}

m.pref.add.locs <-function(w){
	chooseCountries()
}


################### 
# Tools

m.tools.rsse <-function(w){
	responseSelectionSingleExp(w)
}
m.tools.rssl <-function(w){
	responseSelectionSevLoc(w)
}
m.tools.rssly <-function(w){
	responseSelectionSevLocYe(w)
}
m.tools.rssly2 <-function(w){
	responseSelectionSevLocYe2(w)
}


################### 
# Window

m.win.sync <-function(w){
	#w = refresh(w)
	w=refresh(w)
}


################### 
# Help

m.help.man <-function(w){
	fp = file.path(getwd(),"res","manual.doc")
	shell.exec(fp)
}
m.help.prot <-function(w){
	fp = file.path(getwd(),"res","PotatoProtocols.pdf")
	shell.exec(fp)
}
m.help.select <-function(w){
	fp = file.path(getwd(),"res","OnSelectionIndex.pdf")
	shell.exec(fp)
}

m.help.elston <-function(w){
	fp = file.path(getwd(),"res","ElstonSelectionIndex.pdf")
	shell.exec(fp)
}


m.help.ranks <-function(w){
	fp = file.path(getwd(),"docs","ranks.pdf")
	shell.exec(fp)
}

m.help.manon <-function(w){
	shell.exec("https://research.cip.cgiar.org/confluence/display/GDET4RT/Data+Collector")
}


m.help.web <-function(w){
	shell.exec("https://research.cip.cgiar.org/confluence/display/GDET4RT/Home")
}

m.help.feedbk <-function(w){
	shell.exec("https://research.cip.cgiar.org/jira/browse/GTDM")
}

m.help.update <-function(w){
	do.update()
}
m.help.about <-function(w){
	msg = paste(get.version(),"\n\n(c) 2011 International Potato Center\n\nAuthors: ",sep="")
		msg = paste(msg,"
Reinhard Simon, Raul Eyzaguirre, Felipe de Mendiburu, Vilma Hualla, Elisa Salas, 
Stef de Haan, Wolfgang Grueneberg, Merideth Bonierbale",sep="")
	gmessage(msg,ico="info")
}



