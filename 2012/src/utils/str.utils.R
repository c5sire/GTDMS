###############################################################################
#
# TODO: Add comment
#
# Sep 25, 2011
# 9:18:03 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

capitalise <- function(s){
	sl = tolower(s)
	fs = toupper(substr(s,1,1))
	paste(fs, substr(sl,2,nchar(sl)),sep="")
}

get.iso.timestamp <- function(){
	format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}
