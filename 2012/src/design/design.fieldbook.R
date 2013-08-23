###############################################################################
#
# TODO: Add comment
#
# Sep 23, 2011
# 3:05:23 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

design.fieldbook = function(w){
	dlg <- gwindow("Design field trial",visible=F, parent=w)
	g <- ggroup(horizontal = FALSE, cont = dlg)
	wg = ggroup(cont=g)
	bg <- ggroup(cont = g)

	b.cancel <- gbutton("cancel", cont = bg)
	addHandlerClicked(b.cancel, function(h,...) {
				dispose(dlg)
			})
	
	b.ok <- gbutton("ok", cont = bg)
	addHandlerClicked(b.ok, function(h,...) {
				out <- svalue(wdg)
				msg=checkDesignParam(out)
				if(nchar(msg)==0){
					dispose(dlg)	
				} else{
					gmessage(msg,"Please correct these errors!","error")
				}
			})
	g <- ggroup(horizontal = FALSE, cont = dlg)
	bg <- ggroup(cont = g)
	visible(dlg)=TRUE
}

