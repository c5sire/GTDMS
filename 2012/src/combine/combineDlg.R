###############################################################################
#
# TODO: Add comment
#
# Oct 25, 2011
# 12:03:45 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

getCombLayout <-function(){
	a.list=list(type = "gnotebook",
			#horizontal = TRUE,
			children = list(
				list(type="glabel",
									name="name",
									label="Name",
									text = "A name for the combined file.",
									editable = TRUE
							),
				list(type="gcheckboxgroup",
					#		type="gtable",
							name="combs",
							#use.table = T,
							multiple=T,
							label = "Fieldbooks",
							items = get.fb.list(),
							checked=FALSE
				)
			)
		
	)
	
	a.list
	
}


combineDlg = function(win){
	wDlg <- xgwindow("Fieldbooks",visible=F, parent=win)
	layout = getCombLayout()
	
	g <- ggroup(horizontal = FALSE, cont = wDlg)
	wg = ggroup(cont=g)
	wdg = gformlayout(layout, container=wg)
	#print("check 1")
	
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
				dispose(wDlg)
				#print(out)
#				if(length(out$combs)<3) {
#					gmessage("Choose at least 3 sites.")
#				} else {
				xmin=0
				xmax=3
				pb <- winProgressBar("Preparing ...", "Combining fieldbooks ... %",
						xmin, xmax, xmin)
				info <- sprintf("%d%% done", round(1/xmax*100))
				setWinProgressBar(pb, 1, sprintf("Preparing (%s)", info), "Combination")
					join.fieldbooks(out$combs, out$name)
				close(pb)
				#shell.exec()
#				}
				
			},
			, container=bg)
	
	visible(wDlg)=T
	#close(pb)
}

