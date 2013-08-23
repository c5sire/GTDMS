###############################################################################
#
# TODO: Add comment
#
# Oct 5, 2011
# 7:48:48 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

get.calculated.vars <- function(dict){
	dict[nchar(dict$Formula)<1,]
}
