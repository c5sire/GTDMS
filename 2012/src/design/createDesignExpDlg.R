###############################################################################
#
#
# Jun 21, 2011
# 8:28:07 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

get.last.template <-function(prefs){
	tpl=""	
	tpl.lst = get.templates()
	tpl.pst = prefs[prefs$pr_name=="logPrefix","pr_past"]
	if(tpl.pst %in% tpl.lst){
		tpl= which(get.templates()==
						prefs[prefs$pr_name=="logPrefix","pr_past"])
	} else {
		tpl = tpl.lst[1]
	}
	
	return(tpl)
}

extract.sites <- function(prefs) {
	tsites= toVector(prefs[prefs$pr_name=='tsites',"pr_past"])
	#sts = getResourceData('sites','Sites')
  #sts = getSites()
  sts = prefs[prefs$pr_name=="selCountries", "pr_past"]
  sts = str_split(sts,";")[[1]]
  sts = getSiteList(sts, mini=T)
	countries = sort(unique(sts$CNTRY))
	#countries = sort(sts)
	sites=NULL
	for(i in 1:length(countries)){
		locs = prefs[prefs$pr_name==countries[i],"pr_past"]
		locs = str_split(locs,";")[[1]]
		#print(locs)
		sites = c(sites, locs)
	}
	sites = sts[sts$SHORTN %in% sites,c("FULLN","CNTRY","SHORTN")]
	sites = paste(sites[,"SHORTN"]," (",sites[,2],": ",sites[,1],")",sep="")
	list(tsites=sites, sites=sites)
}

doLayoutExpLoG = function(prefs){
	res = extract.sites(prefs = prefs)
	sites = res$sites
	tsites= res$tsites
	#afb = c(paste("'Genotype list' from fieldbook:",getCurrentFB()),"'New genotypes'")
	
	log.list=list(type = "ggroup",
			horizontal = FALSE,
			label=translate("_LBL_DEF_FB_FORMS_"),
			children = list(
					list(type="fieldset",
							columns = 6,
							label = translate("_LBL_DEF_FB_FORMS_HINT"),
							label.pos = "top",
							#label.font = c(weight="bold"),
							children = list(
									list(name = "logPrefix",
											#label = prefs[prefs$name=="logPrefix","label_en"],#
											label = translate("_LBL_TRIAL_TEMPL_")	,
											type = "gcombobox",
											#items=toVector(prefs[prefs$name=="logPrefix","values"]),
											items=get.templates()
											,
											selected=get.last.template(prefs)	
									)
									,
									list(name = "years",
											#label = prefs[prefs$name=="years","label_en"],
											label = translate("_PLANTING_YEARS_"),
											type = "gcombobox",
											items = toVector(prefs[prefs$pr_name=="years","pr_values"]),
											selected=which(toVector(prefs[prefs$pr_name=="years","pr_values"])==
															as.integer(prefs[prefs$pr_name=="years","pr_past"]))
											,
											handler=check.quality
									),
									list(name = "season",
											#label = prefs[prefs$name=="season","label_en"],
											label = translate("_MONTH_"),
											type = "gcombobox",
											items=toVector(prefs[prefs$pr_name=="season","pr_values"]),
											selected=which(toVector(prefs[prefs$pr_name=="season","pr_values"])==
															prefs[prefs$pr_name=="season","pr_past"])
									)
									,
									list(name = "sep",
											label = translate("_SEP_"),
											type = "glabel",
											text = "_"
									)
									,
									
									
									list(name = "tsites",
											#label = prefs[prefs$name=="tsites","label_en"],
											label = translate("_TRIAL_SITE_"),
											type = "gcheckboxgroup",
											items = sites#,
											#checked=sites%in%tsites
									)
								)
							)
#							,						
				
					)
						
			
	)
	
	log.list
}

doLayoutExpDsg = function(prefs, lg){
ngeno = 30
#print("4")
#sdesign=toVector(prefs[prefs$name=='filDesign',"past"])
sdesign=toVector(prefs[prefs$pr_name=='sdesign',"pr_values"])	
#print(sdesign)
pstart =toVector(prefs[prefs$pr_name=='pstart',"pr_values"])
playout =toVector(prefs[prefs$pr_name=='playout',"pr_values"])
#nreps  =toVector(prefs[prefs$name=='nreps',"values"])
crd.nreps  =toVector(prefs[prefs$pr_name=='crd.nreps',"pr_values"])
f2crd.nreps  =toVector(prefs[prefs$pr_name=='f2crd.nreps',"pr_values"])
spcrd.nreps  =toVector(prefs[prefs$pr_name=='spcrd.nreps',"pr_values"])
mbcrd.nreps  =toVector(prefs[prefs$pr_name=='mbcrd.nreps',"pr_values"])

rcbd.nbloc  =toVector(prefs[prefs$pr_name=='rcbd.nbloc',"pr_values"])
f2rcbd.nbloc  =toVector(prefs[prefs$pr_name=='f2rcbd.nbloc',"pr_values"])
sprcbd.nbloc  =toVector(prefs[prefs$pr_name=='sprcbd.nbloc',"pr_values"])
abd.nbloc  =toVector(prefs[prefs$pr_name=='abd.nbloc',"pr_values"])
a01d.nbloc  =toVector(prefs[prefs$pr_name=='a01d.nbloc',"pr_values"])

kbloc  =toVector(prefs[prefs$pr_name=='kbloc',"pr_values"])
bsize  =toVector(prefs[prefs$pr_name=='bsize',"pr_values"])
#factor2=toVector(prefs[prefs$name=='factor2',"values"])
f2crd.adf.level=paste(toVector(prefs[prefs$pr_name=='f2crd.adf.level',"pr_past"]),collapse="\n")
f2rcbd.adf.level=paste(toVector(prefs[prefs$pr_name=='f2rcbd.adf.level',"pr_past"]),collapse="\n")
spcrd.adf.level=paste(toVector(prefs[prefs$pr_name=='spcrd.adf.level',"pr_past"]),collapse="\n")
sprcbd.adf.level=paste(toVector(prefs[prefs$pr_name=='sprcbd.adf.level',"pr_past"]),collapse="\n")
abd.checks = gsub(";","\n",prefs[prefs$pr_name=='abd.checks',"pr_past"])
idlabel=toVector(prefs[prefs$pr_name=='idlabel',"pr_values"])

rnumber=toVector(prefs[prefs$pr_name=='rnumber',"pr_values"])
rmethod=toVector(prefs[prefs$pr_name=='rmethod',"pr_values"])

res = extract.sites(prefs = prefs)
sites = res$sites
tsites= res$tsites

lmsite = 1
xsite = which(sites==prefs[prefs$pr_name=='msite',"pr_past"])
if(length(xsite)>0) lmsite=xsite

#print(sites)
#print(tsites)
#print('5')
#print("RS")

	gts = getGenotypesFromFB(getCurrentFB())
	#gts=""
	
#print('6')

#gts = c("CIP100001","CIP100002","CIP100003","CIP100004","CIP100005")
#h.design = function(h,...){
#	#print(paste("update n sites",length(svalue(h$obj))))
#	#layout[["nSites"]]=length(svalue(h$obj))
#	#gmessage(svalue(h$obj))
#	if(svalue(h$obj)=="Balanced Incomplete Block Design (BIBD)") {
#		enabled(h$obj, TRUE)
#		#gmessage(svalue(h$obj))
#	} else enabled(h$obj, FALSE)
#	
#}

get.fb.src <- function(){
	fb = getCurrentFB()
	fbp = getFieldBookPath(fb)
	if(length(fbp)==0){
		return(translate("_MSG_NO_SELECTION_"))
	} else {
		return(fb)
	}

}

a.list=list(type = "gnotebook",
#horizontal = TRUE,
children = list(
		lg,
		list(type="ggroup",
				columns = 1,
				label = translate("_LBL_DEF_GENO_"),
				children = list(
						list(type="glabel",
								name="cfieldbook",
								text = paste(translate("_LBL_SRC_GENO_"),get.fb.src(),sep="")
						),
						list(type="gtext",
								name="tgenotypesnew",
								width = 100,
								height = 300,
								label = translate("_LBL_NEW_GENO_"),
								text = paste(gts, collapse="\n")
						)
						
						)
		)
		,
		
		list(type="ggroup",
				columns = 1,
				#label = prefs[prefs$name=="sdesign","label_en"],
				label = translate("_LBL_SDESIGN_"),
				label.pos = "top",
				label.font = c(weight="bold"),
				children = list(
						list(name = "sdesign",
								type = "gradio",
								items = sdesign,
								#handler=h.design,
								selected = 
										which(sdesign==prefs[prefs$pr_name=='sdesign',"pr_past"])
						)
				)
		)
		,
		
		list(type="ggroup",
			columns = 1,
			label=translate("_LBL_SDESIGN_OPT_"),
			tab.pos=2,
			children = list(
			   list(#name="sdesign.crdb",
					type="gnotebook",
					
					children = list(
						list(label=translate("_LBL_GENERAL_"),
								type="fieldset",
								columns=1,
							 children = list(
									 
#									 list(name="nreps",
#											 label=prefs[prefs$name=="nreps","label_en"],
#											 type="gcombobox",
#											 items=nreps,
#											 selected = 
#													 which(nreps==prefs[prefs$name=='nreps',"past"])
#									 ),
									 list(name="pstart",
											 #label=prefs[prefs$name=="pstart","label_en"],
											 label = translate("_LBL_PLOT_START_"),
											 type="gcombobox",
											 items=pstart,
											 selected = 
													 which(pstart==prefs[prefs$pr_name=='pstart',"pr_past"])
									 ),
									 list(name="playout",
											 #label=prefs[prefs$name=="playout","label_en"],
											 #label = prefs[prefs$name=="playout","label_en"],
											 label = translate("_LBL_PLOT_LAYOUT_"),
											 type="gradio",
											 items=playout,
											 selected = 
													 which(playout==prefs[prefs$pr_name=='playout',"pr_past"])
									 ),
									 
									 list(name = "nSeeds",
											 #label = prefs[prefs$name=="nSeeds","label_en"],
											 label = translate("_LBL_NSEEDS_"),
											 label.pos = "top",
											 type = "gcombobox",
											 items=toVector(prefs[prefs$pr_name=="nSeeds","pr_values"]),
											 selected=which(toVector(prefs[prefs$pr_name=="nSeeds","pr_values"])==
															 as.integer(prefs[prefs$pr_name=="nSeeds","pr_past"]))
									 )
								)

						),
						list(label="CRD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="crd.nreps",
												#label=prefs[prefs$name=="nreps","label_en"],
												label=translate("_LBL_N_REPS_"),
												type="gcombobox",
												items=crd.nreps,
												selected = 
														which(crd.nreps==prefs[prefs$pr_name=='crd.nreps',"pr_past"])
										)
								)
						)
						,
						list(label="RCBD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="rcbd.nbloc",
												#label=prefs[prefs$name=="nbloc","label_en"],
												label = translate("_LBL_N_BLOCKS_"),
												type="gcombobox",
												items=rcbd.nbloc,
												selected = 
														which(rcbd.nbloc==prefs[prefs$pr_name=='rcbd.nbloc',"pr_past"])
										)
								)
						)
						,
						list(label="BIBD",
							 type="fieldset",
							 columns=1,
							 children = list(
							 list(name="bsize",
									#label=prefs[prefs$name=="bsize","label_en"],
									label = translate("_LBL_BSIZE_"),
									type="gcombobox",
									items=bsize,
									selected = 
											which(bsize==prefs[prefs$pr_name=='bsize',"pr_past"])
							)
							)
						)
						,
						list(label="F2CRD",
							type="fieldset",
							columns=1,
							children = list(
									 list(name="f2crd.nreps",
											 #label=prefs[prefs$name=="nreps","label_en"],
											 label=translate("_LBL_N_REPS_"),
											 type="gcombobox",
											 items=f2crd.nreps,
											 selected = 
													 which(f2crd.nreps==prefs[prefs$pr_name=='f2crd.nreps',"pr_past"])
									 ),
									list(name="f2crd.adf.name",
										type="gedit",
										#label=prefs[prefs$name=='f2crd.adf.name',"label_en"],
										label = translate("_LBL_F2_"),
										text=prefs[prefs$pr_name=='f2crd.adf.name',"pr_past"]
									),
									list(name="f2crd.adf.level",
										type="gtext",
										#label=prefs[prefs$name=='f2crd.adf.level',"label_en"],
										label = translate("_LBL_F2_LEVEL_"),
										label.pos = "top",
										width=60,
										height=70,
										text=f2crd.adf.level
									)
								)
						
						),
						list(label="F2RCBD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="f2rcbd.nbloc",
												#label=prefs[prefs$name=="nbloc","label_en"],
												label = translate("_LBL_N_BLOCKS_"),
												type="gcombobox",
												items=f2rcbd.nbloc,
												selected = 
														which(f2rcbd.nbloc==prefs[prefs$pr_name=='f2rcbd.nbloc',"pr_past"])
										),
										list(name="f2rcbd.adf.name",
												type="gedit",
												#label=prefs[prefs$name=='f2rcbd.adf.name',"label_en"],
												label = translate("_LBL_F2_"),
												text=prefs[prefs$pr_name=='f2rcbd.adf.name',"pr_past"]
										),
										list(name="f2rcbd.adf.level",
												type="gtext",
												#label=prefs[prefs$name=='f2rcbd.adf.level',"label_en"],
												label = translate("_LBL_F2_LEVEL_"),
												label.pos = "top",
												width=60,
												height=70,
												text=f2rcbd.adf.level
										)
								)
						
						),
						list(label="SPCRD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="spcrd.nreps",
												#label=prefs[prefs$name=="nreps","label_en"],
												label = translate("_LBL_N_REPS_"),
												type="gcombobox",
												items=spcrd.nreps,
												selected = 
														which(spcrd.nreps==prefs[prefs$pr_name=='spcrd.nreps',"pr_past"])
										),
										list(name="spcrd.adf.name",
												type="gedit",
												#label=prefs[prefs$name=='spcrd.adf.name',"label_en"],
												label = translate("_LBL_F2_"),
												text=prefs[prefs$pr_name=='spcrd.adf.name',"pr_past"]
										),
										list(name="spcrd.adf.level",
												type="gtext",
												#label=prefs[prefs$name=='spcrd.adf.level',"label_en"],
												label = translate("_LBL_F2_LEVEL_"),
												label.pos = "top",
												width=60,
												height=70,
												text=spcrd.adf.level
										)
								)
						
						),
						list(label="SPRCBD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="sprcbd.nbloc",
												#label=prefs[prefs$name=="nbloc","label_en"],
												label = translate("_LBL_N_BLOCKS_"),
												type="gcombobox",
												items=sprcbd.nbloc,
												selected = 
														which(sprcbd.nbloc==prefs[prefs$pr_name=='sprcbd.nbloc',"pr_past"])
										),
										list(name="sprcbd.adf.name",
												type="gedit",
												#label=prefs[prefs$name=='sprcbd.adf.name',"label_en"],
												label = translate("_LBL_F2_"),
												text=prefs[prefs$pr_name=='sprcbd.adf.name',"pr_past"]
										),
										list(name="sprcbd.adf.level",
												type="gtext",
												#label=prefs[prefs$name=='sprcbd.adf.level',"label_en"],
												label = translate("_LBL_F2_LEVEL_"),
												label.pos = "top",
												width=60,
												height=70,
												text=sprcbd.adf.level
										)
								)
						
						),
						list(label="ABD",
								type="fieldset",
								columns=1,
								children = list(
										list(name="abd.nbloc",
												#label=prefs[prefs$name=="nbloc","label_en"],
												label = translate("_LBL_N_BLOCKS_"),
												type="gcombobox",
												items=abd.nbloc,
												selected = 
														which(abd.nbloc==prefs[prefs$pr_name=='abd.nbloc',"pr_past"])
										),
										list(name="abd.checks",
												type="gtext",
												#label=prefs[prefs$name=='abd.checks',"label_en"],
												label = translate("_LBL_CHECK_GENO_"),
												label.pos = "top",
												width=60,
												height=70,
												text=abd.checks
										)
								)
						),
						list(label="A01D",
								type="fieldset",
								columns=1,
								children = list(
										list(name="a01d.nbloc",
												#label=prefs[prefs$name=="nbloc","label_en"],
												label = translate("_LBL_N_REPS_"), #GTDM-498
												type="gcombobox",
												items=a01d.nbloc,
												selected = 
														which(a01d.nbloc==prefs[prefs$pr_name=='a01d.nbloc',"pr_past"])
										),
										list(name="kbloc",
												#label=prefs[prefs$name=="kbloc","label_en"],
												label = translate("_LBL_BSIZE_"),
												type="gcombobox",
												items=kbloc,
												selected = 
														which(kbloc==prefs[prefs$pr_name=='kbloc',"pr_past"])
										)
								)
						)							
						,list(label="MBCRD",
								type="fieldset",
								columns=1,
								children=list(
								list(name="mbcrd.nreps",
									#label=prefs[prefs$name=="nreps","label_en"],
									label = translate("_LBL_N_REPS_"),
									type="gcombobox",
									items=mbcrd.nreps,
									selected = 
											which(mbcrd.nreps==prefs[prefs$pr_name=='mbcrd.nreps',"pr_past"])
								),
								list(name = "msite",
									#label = prefs[prefs$name=="msite","label_en"],
									label = translate("_LBL_MSITE_"),
									type = "gradio",
									items = sites,
									selected=lmsite
								  )
								)
						)
						
					)
				)
				)
			)
		)

)
a.list

}

doLayoutExp = function(prefs){
	doLayoutExpDsg(prefs,doLayoutExpLoG(prefs))
}

checkFieldbook = function(afb){
	length(grep("fieldbook",afb))
}

check.duplicate.accessions <-function(accs){
	a=table(accs)
	paste(names(a[a>1]),collapse=", ")
}

checkDesignParam = function(out){
	res = ""
	#print(out$tsites)
	if(length(out$tsites)==0) {
		res=paste(res,"\n",translate("_MSG_ONE_SITE_MIN_"))
	}

	if(str_detect(out$sdesign,"(MBCRD)")) {
		sm = get.short.msiten(out)
		ss = ""
		sx=FALSE
		if(length(out$tsites)==1) {
			ss = get.short.siten(out)
			sx= sm==ss
		}
		if(sx){
			res=paste(res,"\n",translate("_MSG_TWO_SITE_MIN_"))	
		}
	}
	
#	if(str_detect(out$sdesign,"(A01D)")) {
#		trt=out$tgenotypesnew
#		r = as.integer(out$a01d.nbloc)
#		k = as.integer(out$kbloc)
#		if(!design.alpha.check(trt,k,r)){
#			#print("Check works")
#			res=paste(res,"\n",translate("_MSG_INVALID_A01D_"))
#		}
#	}
	
	dups = check.duplicate.accessions(out$tgenotypesnew)
	if(nchar(dups)>0){
		res=paste(res,"\n",translate("_MSG_TWO_SITE_MIN_"),dups)
	}
	if(length(out$tgenotypesnew)<3) {
		res=paste(res,"\n",translate("_MSG_MIN_3_GENO_"))
	}
	
	return(res)
}


get.file.names <- function(out){
	fn = list()
	sfn = get.short.siten(out)
	for(i in 1:length(sfn)){
		siten = strsplit(sfn[i]," \\(")[[1]][1]
		fn[i] = paste(out$logPrefix,out$years,out$season,"_",siten,".xls", sep="")
	}
	fn
}

format.params <- function(res,out) {
	res=paste(res,"\n\n", translate("_LBL_SDESIGN_"),": ", out$sdesign,sep="")
	#res=paste(res,"\n", translate("_LBL_N_REPS_"),": ", out$nreps,sep="")
	res=paste(res,"\n",translate("_LBL_PLOT_START_"),": ", out$pstart,sep="")
	res=paste(res,"\n",translate("_LBL_PLOT_LAYOUT_"),": ", out$playout,sep="")
	#res=paste(res,"\nNumber of sub-blocks: ", out$sblocks,sep="")
	
	if(str_detect(out$sdesign,"(MBCRD)")) {
		res=paste(res,"\n", translate("_LBL_N_REPS_"),": ", out$mbcrd.nreps,sep="")
		res=paste(res,"\n",translate("_LBL_MSITE_"),": ", out$msite,sep="")
	}
	if(str_detect(out$sdesign,"(ABD)")) {
		res=paste(res,"\n\n",translate("_LBL_CHECK_GENO_"),": ", gsub("\n",", ", out$abd.checks),sep="")
		res=paste(res,"\n", translate("_LBL_N_BLOCKS_"),": ", out$abd.nbloc,sep="")
	}
	if(str_detect(out$sdesign,"(BIBD)")) {
		res=paste(res,"\nBlock size: ", out$bsize,sep="")
	}
	if(str_detect(out$sdesign,"(F2CRD)")) {
		adfn = out$f2crd.adf.name
		adfl = str_split(out$f2crd.adf.level,"\n")[[1]]
		res=paste(res,"\n", translate("_LBL_N_REPS_"),": ", out$f2crd.nreps,sep="")
		res=paste(res,"\n",translate("_LBL_F2_"),": ", adfn,sep="")
		res=paste(res,"\n",translate("_LBL_F2_LEVEL_"),": ", paste(adfl,collapse=", "),sep="")
	}
	if(str_detect(out$sdesign,"(F2RCBD)")) {
		adfn = out$f2rcbd.adf.name
		adfl = str_split(out$f2rcbd.adf.level,"\n")[[1]]
		res=paste(res,"\n", translate("_LBL_N_BLOCKS_"),": ", out$f2rcbd.nbloc,sep="")
		res=paste(res,"\n",translate("_LBL_F2_"),": ", adfn,sep="")
		res=paste(res,"\n",translate("_LBL_F2_LEVEL_"),": ", paste(adfl,collapse=", "),sep="")
	}
	if(str_detect(out$sdesign,"(SPCRD)")) {
		adfn = out$spcrd.adf.name
		adfl = str_split(out$spcrd.adf.level,"\n")[[1]]
		res=paste(res,"\n", translate("_LBL_N_BLOCKS_"),": ", out$spcrd.nreps,sep="")
		res=paste(res,"\n",translate("_LBL_F2_"),": ", adfn,sep="")
		res=paste(res,"\n",translate("_LBL_F2_LEVEL_"),": ", paste(adfl,collapse=", "),sep="")
	}
	if(str_detect(out$sdesign,"(SPRCBD)")) {
		adfn = out$sprcbd.adf.name
		adfl = str_split(out$sprcbd.adf.level,"\n")[[1]]
		res=paste(res,"\n", translate("_LBL_N_BLOCKS_"),": ", out$sprcbd.nbloc,sep="")
		res=paste(res,"\n",translate("_LBL_F2_"),": ", adfn,sep="")
		res=paste(res,"\n",translate("_LBL_F2_LEVEL_"),": ", paste(adfl,collapse=", "),sep="")
	}
	
	res=paste(res,"\n\n",translate("_LBL_NSEEDS_"),": ", out$nSeeds,sep="")
	res
}



confirmDesignParam = function(out){
	res = ""
	#files = get.file.names(out)
	fn = get.file.names(out)
	nfb = length(fn)
	#nfb = length(get.short.siten(out))
	res=paste(res,"\n",translate("_FIELDBOOKS_"),": ", paste(paste(nfb,collapse=",\n\t")))
	res=paste(res,"\n\n",translate("_TRIAL_SITES_"),": ", paste(fn, collapse=", "),sep="")
	ngt = length(out$tgenotypesnew)
	gmx=min(ngt,30)
	res=paste(res,"\n\n",translate("_LBL_GENOS_"),": ", paste(out$tgenotypesnew[1:gmx], collapse=", "),
			"\n",translate("_MSG_HINT_30_GENO_"),sep="")
	
	tpg = 0.22 #time per genotypes in secs
	tte = round((tpg * ngt * nfb/60),0)
	cmt = paste("\n\n",translate("_MSG_EST_TIME_")," ",sep="")
	if(tte!=0){
		tim = paste(tte,"min")	
	} else tim="~1 min"
	
	res = paste(res,cmt,tim)
	res=format.params(res, out)
	return(res)
}

create.download.page<-function(out){
	to   = file.path(getwd(),"temp")
	unlink(file.path(to,"*.*"))
	from = file.path(getwd(),"res/html/zen.css")
	file.copy(from, to)
	from = file.path(getwd(),"res/html/download.html")
	file.copy(from, to)
	#print(out)
	fn = get.file.names(out)
	
	#print(fn)
	#convert to hyperlinks
	txt=""
	for(i in 1:length(fn)){
		#print(as.character(names(sfb)[i]))
		#print(fn[i])
		sfb=getFieldBookPath(fn[i])
		#print(sfb)
		txt=paste(txt,"<a href='",sfb,"'>",fn[[i]][1],"</a><br>\n",sep="")
		#print(txt)
	}
	#print(txt)
	to = file.path(getwd(),"temp/download.html")
	html = readChar(to,nchars=10000)
	#print(html)
	html = gsub("\\$content",txt,html)
	html = gsub("\\$stamp",date(),html)
	html = gsub("\\$user",Sys.getenv("USERNAME"),html)
	html = gsub("\\$computer",Sys.getenv("COMPUTERNAME"),html)
	params=confirmDesignParam(out)
	html = gsub("\\$params",params,html)
	#print(html)
	#unlink(to)
	writeChar(html,to)
	shell.exec(to)
}


buildDesign = function(out){
	save(out,file="params.Rdata")
	sitea = get.short.siten(out)
	msite = get.short.msiten(out)
#	print(sitea)
#	print(msite)
	nn=length(sitea)
#	print(nn)
	xmin=0
	xmax=nn+1
	pb <- winProgressBar(translate("_MSG_CREATE_FBS_"), translate("_MSG_PREP_"),
			xmin, xmax, xmin)
	tpl = paste("template",out$logPrefix,sep="_")
	vss = readTplVariables(tpl,TRUE)
	
	vsl = readTplVariables(tpl)
	tvars= paste(vss,": ",vsl,sep="")
	matl = out$tgenotypesnew
	checkl = str_split(out$abd.checks,"\n")[[1]]
	matl = matl[!(matl %in% checkl)]
	
#	adf=out$adf
#	if (nchar(adf)>0){
#		adf = strsplit(adf,"\\n")[[1]]
#	}
	adfn=NULL
	adfl=NULL
	nreps=1 # equal nbloc
	bsize=NULL
	if(str_detect(out$sdesign,"(BIBD)")) {
		bsize= as.integer(out$bsize)
	}
	
	if(str_detect(out$sdesign,"(CRD)")) {
		nreps= as.integer(out$crd.nreps)
	}
	if(str_detect(out$sdesign,"(F2CRD)")) {
		adfn = out$f2crd.adf.name
		nreps= as.integer(out$f2crd.nreps)
		adfl = str_split(out$f2crd.adf.level,"\n")[[1]]
	}
	if(str_detect(out$sdesign,"(RCBD)")) {
		nreps= as.integer(out$rcbd.nbloc)
	}
	if(str_detect(out$sdesign,"(F2RCBD)")) {
		nreps= as.integer(out$f2rcbd.nbloc)
		adfn = out$f2rcbd.adf.name
		adfl = str_split(out$f2rcbd.adf.level,"\n")[[1]]
	}
	if(str_detect(out$sdesign,"(SPCRD)")) {
		adfn = out$spcrd.adf.name
		nreps= as.integer(out$spcrd.nreps)
		adfl = str_split(out$spcrd.adf.level,"\n")[[1]]
	}
	if(str_detect(out$sdesign,"(SPRCBD)")) {
		nreps= as.integer(out$sprcbd.nbloc)
		adfn = out$sprcbd.adf.name
		adfl = str_split(out$sprcbd.adf.level,"\n")[[1]]
	}
	if(str_detect(out$sdesign,"(ABD)")){
		nreps= as.integer(out$abd.nbloc)
	    
	}
	if(str_detect(out$sdesign,"(A01D)")){
		nreps = as.integer(out$a01d.nbloc)
		bsize = as.integer(out$kbloc)
	}
	if(str_detect(out$sdesign,"(MBCRD)")){
		nreps= as.integer(out$mbcrd.nreps)
	}
	
	tp = getPath()
	gp = get.local.db.root()
    
	#fr = paste(gp,out$template,".xls", sep="")
	fr = paste(tp,"/template_",out$logPrefix,".xls", sep="")
	
	#print(fr)
	season = paste(out$years, out$season, sep="")
	dict = as.data.frame(read.xlsx2(fr,"Var List"), stringsAsFactors=F)
  #dict = get.data.dict()
	
#	variab = aa
	prefs = get.prefs()

	crops = get.list.of.registered.crops(prefs)
	prefx = str_sub(out$logPrefix,1,2)
	ccrop = crops[[prefx]]
	
	
#    if(str_detect(out$logPrefix,"PT")) ccrop="potato"
#	if(str_detect(out$logPrefix,"SP")) ccrop="sweetpotato"
#	if(str_detect(out$logPrefix,"AH")) ccrop="ahipa"
	
	#for(i in 1:ncol(dict)) dict[,i]=as.character(dict[,i])
#	abbr = dict$Abbreviations%in%variab
	#dict = dict[tolower(dict$Select)=="x",-c(3,4)]
	
	dict = read.xlsx2(fr, sheetName="Var List")
	dict = dict[,1:2]
	for(i in 1:ncol(dict))  dict[,i]=as.character(dict[,i])
	names(dict)=c("VAR","ABBR")
	
	#print(dict)
	#print(nn)
	msg=""
	
	
	#print(out)

	for(i in 1:nn){
		siten = sitea[i]
		fbm = paste(out$logPrefix, out$trialPhase,season, "_",siten, sep="")
		print(fbm)
		fbn = paste(fbm,".xls", sep="")
#print(fbn)
		fp = file.path(gp,ccrop,season, sep="")
		to = file.path(fp,fbn)
		#print(to)
		#print(file.exists(to))
		if(file.exists(to)){
			msg = paste(msg,"\n",fbn," ",translate("_MSG_FB_EXISTS_"),sep="")
			#gmessage(msg,"Please note!","warning")
			
		} else {
		
		
		is.ms = FALSE
		if(!is.null(msite)) is.ms = siten==msite
		#print(i)
		#print(is.ms)
		fbd=randomize.design(design = out$sdesign,
				matl= matl, 
				#reps=as.integer(out$nreps), 
				reps=nreps,
				msite = is.ms,#GTDM-137
				lbls=out$idlabel,
				bsize = bsize, 
				adfn = adfn,
				adfl = adfl,
				checkl = checkl,
				startn = as.numeric(out$pstart) , 
				seed = as.numeric(out$rnumber), 
				randM= out$rmethod
		)
		#print(fbd)
		aa = array()
		for(ii in 1:length(tvars)) aa[ii]=strsplit(tvars[ii],":")[[1]][1]
		
		if(out$idlabel=="ID"){
			aa=c("INSTN",aa)
		}
		
		db = matrix("", nrow=nrow(fbd), ncol = length(aa))
		#print(db)
		db = as.data.frame(db)
		names(db)=aa
		data = cbind(fbd, db)

		if(!file.exists(fp)) dir.create(fp,rec=T) #GTDM-309
			info=translate("_MSG_PREP_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FBS_")," (%s)"), info), info)
			file.copy(fr,to)	
			#insert fieldbook
			wb <- loadWorkbook(to)
			#insert data dict
			#removeSheet(wb, sheetName="Var List")
			#saveWorkbook(wb, to)
			#write.xlsx2(dict,to,"Var List",append=T, row.names=F)
			#write.xlsx2(data,to,"Fieldbook",append=T, row.names=F)
			#wb <- loadWorkbook(to)
			#In 'Var List' only put selected variables
			#insert site data
			info=translate("_MSG_SET_SITE_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FBS_")," (%s)"), "a"), info)
			
			if(str_detect(out$sdesign,"(MBCRD)")) {
				add.msite.data(is.ms, wb)
			}
			
			add.site.data(siten,wb)
			#saveWorkbook(wb,to)
			#insert other parameter data
			info=translate("_MSG_SET_SHORTN_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_")," (%s)",sep=""), "b"), info)
			add.short.name(fbm,wb)
			#saveWorkbook(wb,to)
			#print(out)
			info=translate("_MSG_SET_PARAMS_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_")," (%s)",sep=""), "c"), info)
			add.params(siten, sitea, out,wb)
			#saveWorkbook(wb,to)
			add.refs2sim.trials(siten, sitea, out,wb)
			
			#saveWorkbook(wb,to)
			info=translate("_MSG_ADD_VARS_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_")," (%s)",sep=""), "d"), info)
			add.vars(vss,wb, season, dict, prefs, out$logPrefix)
			saveWorkbook(wb,to)
			info=translate("_MSG_ADD_VARS_CAL_")
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_")," (%s)",sep=""), "e"), info)
			add.var.list(dict,wb, prefs, vss, out$logPrefix )
			saveWorkbook(wb,to)
			info=translate("_MSG_ADD_FB_")
			
			
			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_"),"(%s)",sep=""), "f"), info)
			#save(data,web,prefs,vss,out$logPrefix,to,file="params.Rdata")
			add.fieldbook(data,wb,prefs, vss, out$logPrefix )
			add.fieldbook(data,wb,prefs, vss, out$logPrefix,"Fieldbook2" )
			#print(to)
			#saveWorkbook(wb,to)
			
			#TODO add layout sheet
			#wb <- loadWorkbook(to)
			#print(out$sdesign)
			#add.layout(data, design = out$sdesign, nblock=out$bsize, layout=out$playout, wb, to)

			#saveWorkbook(wb,to)
			#wb <- loadWorkbook(to)
			saveWorkbook(wb,to)
			#update status table


			setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_ARCH_")," (%s)",sep=""), info), 
					translate("_MSG_UPDATE_DB_"))
			wb=read.fb(to,"Minimal")
			checkin = get.iso.timestamp()
			update.trial.in.status.db(fieldbook_id=wb["Short name or Title",2],
			#crop=wb["Crop",2],
			planting_date= wb["Begin date",2], 
			country=wb["Country",2], trial_type=wb["Type of Trial",2], 
			contact=wb["Leader",2], checkin= checkin, target_path=fp)
			
			
			shell.exec(to)
		}
		info <- sprintf("%d%% done", round(i/xmax*100))
		setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_CREATE_FB_"),"(%s)",sep=""), info), info)
		
	}
	refresh(w)
	if(msg!="") gmessage(msg,translate("_MSG_NOTE_"),"warning")
	close(pb)
	#w=refresh(w)
}


consolidate.genotypes <-function(out){
	#join genotypes
	out$idlabel="INSTN"
	res=NULL
	if (nchar(out$tgenotypesnew)>0){
		res = strsplit(out$tgenotypesnew,"\\n")[[1]]
	}
	accs = res
	accs=accs[accs!=""]
	accs=accs[accs!="none"]
	out$tgenotypesnew=accs
	out
}


createDesignExpDlg <- function(win){
	#crop = paste("New Germplasm list:",crop,pref.defaults$BreedingProgram$crop)
	#out = list()
	# load preferences
	xmin=0
	xmax=2
	i=1
	pb <- winProgressBar(translate("_MSG_PREP_"), paste(translate("_MSG_LOAD_MODULE_")," ... %",sep=""),
			xmin, xmax, xmin)
	#print("0")
	prefs = get.prefs()
	info <- sprintf(paste("%d%% ",translate("_MSG_DONE_"),sep=""), round(i/xmax*100))
	setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_LOADING_")," (%s)",sep=""), info), info)
	#print("1")
	log.list = doLayoutExp(prefs)
	#print("2")
	dlg <- xgwindow(translate("_LBL_DEF_FB_FORMS_"),visible=F, parent=win)
	#print("3")
	g <- ggroup(horizontal = FALSE, cont = dlg)
	wg = ggroup(cont=g)
	wdg = gformlayout(log.list, container=wg)
	#print("3")
	#print(svalue(wdg))
	bg <- ggroup(cont = g)
	addSpring(bg)
	b.cancel <- gbutton("cancel", cont = bg)
	addHandlerClicked(b.cancel, function(h,...) {
				dispose(dlg)
			})
	
	b.ok <- gbutton("ok", cont = bg)
	addHandlerClicked(b.ok, function(h,...) {
				out <- svalue(wdg)
				out <- consolidate.genotypes(out)
				
				#print(out)
				msg=checkDesignParam(out)
				#msg=""
#				#print(nchar(res))
				if(nchar(msg)==0){
					msg=confirmDesignParam(out)
					if(gconfirm(msg,translate("_MSG_CONFIRM_"),"info")){
						save.prefs(out, prefs)
						buildDesign(out)
						
						dispose(dlg)	
						#create.download.page(out)
					
					}
				} else{
					gmessage(msg,translate("_MSG_CORRECT_ERR_"),"error")
				}
			})
	i=2
	info <- sprintf(paste("%d%% ",translate("_MSG_DONE_"),sep=""),  round(i/xmax*100))
	setWinProgressBar(pb, i, sprintf(paste(translate("_MSG_LOADING_")," (%s)",sep=""), info), info)
	
	g <- ggroup(horizontal = FALSE, cont = dlg)
	#fl <- gformlayout(log.list, cont = g, expand=TRUE)
	bg <- ggroup(cont = g)
	
	close(pb)
	
	visible(dlg)=TRUE
	#out
}
