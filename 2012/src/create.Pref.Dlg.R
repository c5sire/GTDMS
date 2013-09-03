###############################################################################
#
# TODO: Add comment
#
# Jun 20, 2011
# 9:37:46 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

## TODO update preference file from master file excel for sites
## Solution: check if there are more countries in excel site list than in preferences

check.countries <- function(prefs, cntrs){
	# more checks on content of parameters
	
	# get list of countries from database: parameter cntrs!
	# get list of countries from preference file: parameter prefs!
	p.cntrs = prefs[prefs$pr_name=="acountries","pr_past"]
	# compare both lists
	p.cntrs=str_split(p.cntrs,";")[[1]]
	res=cntrs[!cntrs %in% p.cntrs]
	if(length(res)!=0){
		# and add missing countries to preference file
		prefs[prefs$pr_name=="acountries","pr_past"] =  paste(c(p.cntrs,res),collapse=";")
		prefs[prefs$pr_name=="acountries","pr_values"] =  paste(c(p.cntrs,res),collapse=";")
		prefs[prefs$pr_name=="acountries","pr_default"] =  paste(c(p.cntrs,res),collapse=";")
		for(j in 1:length(res)){
			row = c(res[j],res[j],"","","")
			prefs = rbind(prefs,row)
		}
		#save prefs
		#fn = file.path("res","prefs.txt")
		#write.table(prefs,file = fn, sep="\t", row.names=F)
    write.prefs(prefs)
	}
	prefs
}


make.a.tab <- function(from, to, fbt, items=1:80, prefs){
	if(length(items)<to){
		to=length(items)
	}
	tpl = paste("template_",fbt,sep="")
	vss = readTplVariables(tpl,TRUE)
	
	fil.vars = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="vars")
	fil.desc = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="desc")
	fil.anal = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="anal")
	
	tab=list(type="ggroup",
			label=paste("Variables: ",from,"-",to,sep=""),
			children = list(list(
				type="fieldset",
				columns=3,
				label="",
				label.pos = "top",
				children=list(	
						list(type="gcheckboxgroup",
								label="Fieldbook",
								name=paste(fbt,"sel",from,to,sep="_"),
								items = items[from:to],
								checked=fil.vars[from:to]
						),
						list(type="gcheckboxgroup",
								label="Summarize",
								name=paste(fbt,"dsc",from,to,sep="_"),
								items = from:to,
								checked=fil.desc[from:to]
						),
						list(type="gcheckboxgroup",
								label="Analyze",
								name=paste(fbt,"ana",from,to,sep="_"),
								items = from:to,
								checked=fil.anal[from:to]
						)

				)
		))
	)	
	tab
}

make.a.site.tab <- function(from, to, cnt, items=1:80, prefs){
	if(length(items)<to){
		to=length(items)
	}
	tplts = getSiteList(cnt,F)
	#tpl = paste("template_",fbt,sep="")
	#vss = readTplVariables(tpl,TRUE)
	
#	fil.vars = get.var.fil(fbt = fbt, prefs = prefs, vss = vss, type="vars")
	fil.site = get.site.fil(cnt = cnt, prefs=prefs, ssn = tplts, type="sites")
	tab=list(
			label=paste("Sites: ",from,"-",to,sep=""),
			
				type="gcheckboxgroup",
					#label="Localities",
					name=paste(cnt,"sel",from,to,sep="_"),
					items = items[from:to],
					checked=fil.site[from:to]
			)
	
	tab
}





get.v.per.page <- function(){
	10
}

make.tabs.for.vars <- function(fbt, lvars, prefs, type="vars"){
a.list = list(type = "gnotebook",
	tab.pos = 2,
	children = list(
		#tab
	)
)
v.per.page=get.v.per.page()
n.pages = length(lvars)/v.per.page
if(length(lvars)%%v.per.page >0 ) n.pages=round(n.pages+0.5,0)
for(i in 1:n.pages){
	st = (i-1)*v.per.page+1
	ed = (i*v.per.page)
	#print(st)
	#print(ed)
	if(type=="vars"){
		a.list$children[[i]]=make.a.tab(st,ed, fbt, lvars, prefs)	
		
	}
	if(type=="sites"){
		#print(paste(i,st,ed,fbt, lvars))
		a.list$children[[i]]=make.a.site.tab(st,ed, fbt, lvars, prefs)
		
	}
	
	#print(str(a.list))
}

#print(a.list)
a.list
}

get.tpl.txt <- function(tpl){
	tpl = paste("template_",tpl,sep="")
	vss = readTplVariables(tpl,TRUE)
	vsl = readTplVariables(tpl)
	paste(vsl," (",vss,")",sep="")
}

getPrefLayout = function(prefs){
	
	cntrs  = getCountryList()
	prefs  = check.countries(prefs, cntrs)
	
	fbook  = filename(getFieldBooks(getCurrentCrop()))
	#crops  = strsplit(prefs[prefs$name=="crop","values"],";")[[1]]
	crops = as.character(get.list.of.registered.crops(prefs))
	#stdsg  = strsplit(prefs[prefs$name=="filDesign","values"],";")[[1]]
#	tplts  = get.templates()
#	ptyl.vars = get.tpl.txt("PTYL")
#	ptlb.vars = get.tpl.txt("PTLB")
	
	c.def  = prefs[prefs$pr_name=="crop"   ,"pr_past"]
	#s.def  = strsplit(prefs[prefs$name=="filDesign","past"],";")[[1]]
	s.fbk  = prefs[prefs$pr_name=="afieldbook","pr_past"]
#	s.tpl  = prefs[prefs$name=="template","past"]
	s.cnt  = prefs[prefs$pr_name=="acountries","pr_past"]
	fsel=0
	if(s.fbk!=""){
		fsel=which(fbook%in%s.fbk==TRUE)
		if(length(fsel)==0) {
			fsel=0
		}
	}

#	tpls=1
#	if(s.tpl!=""){
#		tpls=which(tplts%in%s.tpl==TRUE)
#	}
	#print(prefs[,1])
	#print(paste("CHECK",s.cnt))
	csel=1
	if(s.cnt!=""){
		s.cnt= toVector(s.cnt)
		#print(s.cnt)
		csel=cntrs%in%s.cnt==TRUE
		#$rint(csel)
	}
	
	
#	print(fbook)
	#print(fsel)
	a.list=list(type = "gnotebook",
			#horizontal = TRUE,
			children = list(
					
					list(type="gradio",
							name="crop",
							#columns = 1,
							label = "Crops",
							items = crops,
							selected=which(crops%in%c.def==TRUE)
					),
#					list(type="gcheckboxgroup",
#							name="filDesign",
#							#columns = 1,
#							label = "Statistical designs",
#							items = stdsg,
#							checked=stdsg%in%s.def==TRUE
#					),
					list(type="gcombobox",
							name="afieldbook",
							#columns = 1,
							label = "Reference fieldbook",
							items = fbook,
							selected=fsel
					),
					list(type="gcheckboxgroup",
							name="acountries",
							#columns = 1,
							label = "Active countries",
							items = cntrs,
							checked=csel
					)
	)
	)
	a.list
	
}

rejoin.fbt <-function(fbt, out){
	#reconstruct variables in out list
	lvars <- get.tpl.txt(fbt)
	v.per.page=get.v.per.page()
	n.pages = length(lvars)/v.per.page
	if(length(lvars)%%v.per.page >0 ) n.pages=round(n.pages+0.5,0)
	n = round(n.pages)
	#print(str(n.pages))
	rv=character()
	rd=character()
	ra=character()
	for(i in 1:n.pages){
		st = (i-1)*v.per.page+1
		ed = (i*v.per.page)
		if(i==(n.pages)){
			ed=length(lvars)	
		}
		
		#print(paste(i,": ",n.pages,sep=""))
		us = paste(fbt,"sel",st,ed,sep="_")
		ds = paste(fbt,"dsc",st,ed,sep="_")
		an = paste(fbt,"ana",st,ed,sep="_")
		rv = c(rv,out[us][[1]])
		rd = c(rd,out[ds][[1]])
		ra = c(ra,out[an][[1]])
		#print(us)
		#print(ds)
		#print(an)
	}
	#print(rv)
	#print(rd)
	#print(ra)
	# further clean up
	tpl=paste("template_",fbt,sep="")
	vss = readTplVariables(tpl,TRUE)
	fl1 = lvars %in% rv
	fl2 = (1:length(vss)) %in% rd
	fl3 = (1:length(vss)) %in% ra
	#idx = (1:length(vss))[fil]
	rvs = vss[fl1]
	#print(rv)
	#print(fil)
	#print(rvs)
	rds = vss[fl1 & fl2]
	ras = vss[fl1 & fl2 & fl3]
	
	res = list()

	res[paste(fbt,"vars",sep="")][[1]] = rvs
	res[paste(fbt,"desc",sep="")][[1]] = rds
	res[paste(fbt,"anal",sep="")][[1]] = ras
	res
}

rejoin.tpl.vars <- function(out){
	#fbts = c("PTYL","PTLB")
	fbts = get.templates()
	res = list()
	for(i in 1:length(fbts)){
		res[[i]] = rejoin.fbt(fbts[i], out)	
	}
	res
}


create.Pref.Dlg = function(w){
	xmin=0
	xmax=1
	pb <- winProgressBar("Creating Preference dialog ...", "Preparing ... %",
			xmin, xmax, xmin)
	
	
	wDlg <- xgwindow("Preferences",visible=F, parent=w, width=400, height=300)
	prefs  = get.prefs()
	layout = getPrefLayout(prefs)
	#g1 <- ggroup(horizontal = FALSE, cont = wDlg, use.scrollwindow=T)
	g <- ggroup(horizontal = TRUE, cont = wDlg)
	wg = ggroup(cont=g,horizontal = FALSE)
	wdg = gformlayout(layout, container=wg)
	#print("check 1")
	
	bg <- ggroup(cont = g, horizontal = FALSE)
	#addSpring(bg)
	addSpring(bg)
	
	gbutton("cancel", handler =  function(h,...) {
				out = "cancel"
				dput(out,file="bin/temp.txt")
				dispose(wDlg)
			},
			, container=bg)
			#, container=wDlg)
	gbutton("ok", handler =  function(h,...) {
				out <- svalue(wdg)
				#out <- rejoin.tpl.vars(out)
				#print(out)
				#putPrefs(prefs,out)
				prefs[prefs$pr_name=="crop","pr_past"]=out$crop
				#prefs[prefs$name=="filDesign","past"]=paste(out$filDesign,collapse=";")
				if(is.na(out$afieldbook)) out$afieldbook=""
				prefs[prefs$pr_name=="afieldbook","pr_past"]=out$afieldbook
				write.prefs(prefs)
				#w=refresh(w) #GTDM-310
				refresh(w)
				#delete(w,sb)
				#svalue(sb)= updateStatus()
				dispose(wDlg)
				
			},
			, container=bg)
			#, container=wDlg)
	
	visible(wDlg)=T
	close(pb)
}


extract.loc <-function(out){
	#i = regexpr("\\([A-Z\\.\\-]{3,8}\\)",out)[[1]]
	out = substr(out,(nchar(out)-11),nchar(out))
	i = str_locate(out,"\\(")[[1]]
	str_sub(out,(i+1),(nchar(out)-1))
}

rejoin.tpl.ctrs <- function(out){
	cntrs = getCountryList()
	res=list(length(cntrs))
	locs=list()
	for(i in 1:length(cntrs)){
		sites = getSiteList(cntrs[i])
		v.per.page=get.v.per.page()
		n.pages = length(sites)/v.per.page
		if(length(sites)%%v.per.page >0 ) n.pages=round(n.pages+0.5,0)
		
		res=NULL
		for(j in 1:n.pages){
			st = (j-1)*v.per.page+1
			ed = (j*v.per.page)
			if(length(sites)<ed){
				ed=length(sites)
			}
			name=paste(cntrs[i],"sel",st,ed,sep="_")
			#print(name)
			#print(out[[name]])
			res = c(res,out[[name]])
		}
		locs[[cntrs[[i]]]]=res
	}
	for(i in 1:length(cntrs)){
		locs[[cntrs[i]]] = as.character(lapply(locs[[i]],extract.loc))
	}
	locs
}


##################################################
# Variables

getVarPrefLayout = function(prefs){
	tplts  = get.templates()
	crop = getCurrentCrop()

	make.tpl.lst <-function(tpl,vrs,prs){
		list(type="ggroup",
				columns = 1,
				label = tpl,
				children = list(
						make.tabs.for.vars(tpl,vrs, prs)
				)
		)
	}
	a.list=list(type = "gnotebook",
			children = list(
			)
	)
	
	nt = length(tplts)
	for(k in 1:nt){
		#print(tplts[k])
		vrs = get.tpl.txt(tplts[k])
		#print(vrs)
		a.list$children[[k]]=make.tpl.lst(tplts[k],vrs,prefs)
	}
	return(a.list)
}


create.Var.Pref.Dlg = function(win){
	xmin=0
	xmax=1
	pb <- winProgressBar("Creating Preference dialog ...", "Preparing ... %",
			xmin, xmax, xmin)
	
	
	wDlg <- xgwindow("Preferences",visible=F, parent=win)
	prefs  = get.prefs()
	layout = getVarPrefLayout(prefs)
	
	g <- ggroup(horizontal = FALSE, cont = wDlg)
	wg = ggroup(cont=g)
	wdg = gformlayout(layout, container=wg)
	#print("check 1")
	
	bg <- ggroup(cont = g)
	addSpring(bg)
	
	gbutton("cancel", handler =  function(h,...) {
				out = "cancel"
				dput(out,file="bin/temp.txt")
				dispose(wDlg)
			},
			, container=bg)
	gbutton("ok", handler =  function(h,...) {
				out <- svalue(wdg)
				res <- rejoin.tpl.vars(out)
				#nm="PTYLvars"
				dput(res,"temp/vars.txt")
				#print(res[[2]][nm])
				#print(res)
				#putPrefs(prefs,out)
				tplts  = get.templates()
				for(i in 1:length(tplts)){
					nm = paste(tplts[i],"vars",sep="")
					x = paste(res[[i]][[nm]],collapse=";")
					prefs[prefs$pr_name==nm,"pr_past"]=x
					
					nm = paste(tplts[i],"anal",sep="")
					x = paste(res[[i]][[nm]],collapse=";")
					prefs[prefs$pr_name==nm,"pr_past"]=x
					
					nm = paste(tplts[i],"desc",sep="")
					x = paste(res[[i]][[nm]],collapse=";")
					prefs[prefs$pr_name==nm,"pr_past"]=x
					
				}
				write.prefs(prefs)
				dispose(wDlg)
				
			},
			, container=bg)
	
	visible(wDlg)=T
	close(pb)
}


##################################################
# Localities

getLocPrefLayout = function(prefs){
	cntrs = getCountryList()
	clist = list(length(cntrs))
	tplts = list(length(cntrs))
		
	#print(cntrs)
	#print(clist)
	#print(tplts)
	nc = length(cntrs)
	for(i in 1:nc) {
		#names(clist)[[i]]=cntrs[i]
		tplts[[i]] = getSiteList(cntrs[i],F)
		clist[[i]]=paste(getSiteList(cntrs[i])," (",tplts[[i]],")",sep="")
	}
	#print(clist)
	#print(tplts)
#	ptyl.vars = get.tpl.txt("PTYL")
#	ptlb.vars = get.tpl.txt("PTLB")
	s.tpl = list(length(cntrs))
	for(i in 1:nc) {
		s.tpl[[i]]  = prefs[prefs$pr_name==cntrs[i],"pr_past"]
	
		print(s.tpl)
		tpls=list(length(cntrs))
		#print(tplts)
		#print(s.tpl)
		if(s.tpl[i]!=""){
			tplts[[i]]=which(tplts[[i]]%in%s.tpl[[i]]==TRUE)
		#print(tpls)
		#print(s.tpl)
	}
	}
#	if(s.tpl[2]!=""){
#		tpls[[2]]=which(tplts[[1]]%in%s.tpl[[2]]==TRUE)
#	}
	
			assemble.country.sites <- function(cntrs,idx,clist,prefs){
					acs = list(type="ggroup",
							columns = 1,
							label = cntrs[idx],
							children = list(
									make.tabs.for.vars(cntrs[idx],clist[[idx]], prefs, type="sites")
							)
					)
					return(acs)
			}

	
	a.list=list(type = "gnotebook",
			children = list(
			)
	)
	for(i in 1:nc){
		a.list$children[[i]]=assemble.country.sites(cntrs,i,clist,prefs)
	}
	return(a.list)
}


create.Loc.Pref.Dlg = function(win){
	xmin=0
	xmax=1
	pb <- winProgressBar("Creating Preference dialog ...", "Preparing ... %",
			xmin, xmax, xmin)
	
	
	wDlg <- xgwindow("Preferences",visible=F, parent=win)
	prefs  = get.prefs()
	layout = getLocPrefLayout(prefs)
	
	g <- ggroup(horizontal = FALSE, cont = wDlg)
	wg = ggroup(cont=g)
	wdg = gformlayout(layout, container=wg)
	#print("check 1")
	
	bg <- ggroup(cont = g)
	addSpring(bg)
	
	gbutton("cancel", handler =  function(h,...) {
				out = "cancel"
				dput(out,file="bin/temp.txt")
				dispose(wDlg)
			},
			, container=bg)
	gbutton("ok", handler =  function(h,...) {
				out <- svalue(wdg)
				res <- rejoin.tpl.ctrs(out)
				nn=0
				for(i in 1:length(res)){
					nn = nn + length(res[[i]]) 
				}
				if(nn<1){
					gmessage("You must pre-select at least one site!")
				} else{
					for(i in 1:length(res)){
						prefs[prefs$pr_name==names(res)[i],"pr_past"]=paste(res[[i]],collapse=";")
					}
					write.prefs(prefs)
					
				}
				#print(out)
				#dput(out,file="bin/temp.txt")
				#putPrefs(prefs,out)
				#updateStatus()
				dispose(wDlg)
				
			},
			, container=bg)
	
	visible(wDlg)=T
	close(pb)
}

