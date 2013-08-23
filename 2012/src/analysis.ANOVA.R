###############################################################################
#
# TODO: Add comment
#
# Oct 19, 2011
# 5:52:06 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

do.ANOVA <-function(w){
	# choose from fieldbooks dialog
	res = create.fb.Dlg(w)
	#res = dget(tfl)[[1]]
	# if not cancelled
	if(res!="cancel"){
		# work on a file copy
		
		close.all.excel()
		
		xmin=0
		xmax=3
		pb <- winProgressBar("Preparing ...", "ANOVA ... %",
				xmin, xmax, xmin)
		info <- sprintf("%d%% done", round(1/xmax*100))
		setWinProgressBar(pb, 1, sprintf("Preparing (%s)", info), "ANOVA")
		fr = getFieldBookPath(res)
		fc = file.path(get.local.db.root(),"accepted",basename(fr))
		file.copy(fr, fc, ov=TRUE)
		
		info <- sprintf("%d%% done", round(2/xmax*100))
		setWinProgressBar(pb, 2, sprintf("Analysis (%s)", info), "ANOVA")
		res=calc.anova(fc)
		if(res){
			info <- sprintf("%d%% done", round(3/xmax*100))
			setWinProgressBar(pb, 3, sprintf("Analysis (%s)", info), "ANOVA")
			file.copy(fc, fr, ov=TRUE)
		}
		close(pb)
		shell.exec(fc)
	}
}

