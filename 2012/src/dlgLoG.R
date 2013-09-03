###############################################################################
#
# TODO: Add comment
#
# May 25, 2011
# 4:52:55 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

check.quality = function(h, ...){
	print(paste("Pref:",svalue(h$obj)))
}

doLayout = function(prefs){

log.list=list(type = "ggroup",
		horizontal = FALSE,
		children = list(
				list(type="fieldset",
						columns = 4,
						label = "'List of Germplasm' identifier",
						label.pos = "top",
						label.font = c(weight="bold"),
						children = list(
								list(name = "logPrefix",
										label = prefs[prefs$pr_name=="logPrefix","pr_label_en"],
										type = "glabel",
										text = prefs[prefs$pr_name=="logPrefix","pr_past"]
								),
								list(name = "trialPhase",
										label = prefs[prefs$pr_name=="trialPhase","lpr_abel_en"],
										type = "gcombobox",
										items=toVector(prefs[prefs$pr_name=="trialPhase","pr_values"]),
										selected=which(toVector(prefs[prefs$pr_name=="trialPhase","pr_values"])==
														prefs[prefs$pr_name=="trialPhase","pr_past"])
								),
								list(name = "years",
										label = prefs[prefs$pr_name=="years","pr_label_en"],
										type = "gcombobox",
										items = toVector(prefs[prefs$pr_name=="years","pr_values"]),
										selected=which(toVector(prefs[prefs$pr_name=="years","pr_values"])==
														as.integer(prefs[prefs$pr_name=="years","pr_past"]))
										,
										handler=check.quality
								),
						#)
#,
#								
								list(name = "season",
										label = prefs[prefs$pr_name=="season","lpr_abel_en"],
										type = "gcombobox",
										items=toVector(prefs[prefs$pr_name=="season","pr_values"]),
										selected=which(toVector(prefs[prefs$pr_name=="season","pr_values"])==
														prefs[prefs$pr_name=="season","pr_past"])
										## depends.on = "x",
										## depends.FUN = function(value) nchar(value) > 0,
										## depends.signal = "addHandlerBlur"
								)
#,
#								list(name = "derivedName",
#										label = "Derived name",
#										type = "glabel",
#										text = paste(
#												log.list$children[[1]]$children[[1]]$text,
#												log.list$children[[1]]$children[[2]]$selected,
#												log.list$children[[1]]$children[[3]]$selected,
#												log.list$children[[1]]$children[[4]]$selected,
#												sep="")
#								)
						)
				),
				list(type="fieldset",
						columns = 4,
						label = "'List of germplasm' parameters",
						label.pos = "top",
						label.font = c(weight="bold"),
						children = list(
								list(name = "nGenotypes",
										label = prefs[prefs$pr_name=="nGenotypes","pr_label_en"],
										type = "gcombobox",
										items=toVector(prefs[prefs$pr_name=="nGenotypes","pr_values"]),
										selected=which(toVector(prefs[prefs$pr_name=="nGenotypes","pr_values"])==
														as.integer(prefs[prefs$pr_name=="nGenotypes","pr_past"]))
								),
								list(name = "nSeeds",
										label = prefs[prefs$pr_name=="nSeeds","pr_label_en"],
										type = "gcombobox",
										items=toVector(prefs[prefs$pr_name=="nSeeds","pr_values"]),
										selected=which(toVector(prefs[prefs$pr_name=="nSeeds","pr_values"])==
														as.integer(prefs[prefs$pr_name=="nSeeds","pr_past"]))
								)
					)
			)
						
			)
			
)
log.list
}


dlgLoG <- function(win){
	#crop = paste("New Germplasm list:",crop,pref.defaults$BreedingProgram$crop)
	#out = list()
	# load preferences
	prefs = getPrefs()
	
	
	log.list = doLayout(prefs)
	
	dlg <- xgwindow(visible=F, parent=win)
	g <- ggroup(horizontal = FALSE, cont = dlg)
	wg = ggroup(cont=g)
	wdg = gformlayout(log.list, container=wg)
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
				#print(svalue(out))
				#print(str(svalue(out)))
				#pb = xgProgress(win=win)
				prefs=putPrefs(prefs,out)
				setLoG(prefs)
				#dispose(pb)
				dispose(dlg)
			})
	
	g <- ggroup(horizontal = FALSE, cont = dlg)
	#fl <- gformlayout(log.list, cont = g, expand=TRUE)
	bg <- ggroup(cont = g)
	visible(dlg)=TRUE
	#out
}
