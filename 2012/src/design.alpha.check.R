###############################################################################
#
#
# May 3, 2012
# 2:17:14 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

design.alpha.check <- function(trt, k, r){
	res = F
	# # of treatments = k * s
	n = length(trt)
	if(n%%k!=0) return(FALSE)
	s = n/k
	# Series I
	if(r==2 && k<=s) res=T
	# Series II
	if(r==3 && (s-1)%%2==0 && k<=s) res=T
	# Series III
	if(r==3 && (s)%%2==0 && k<=(s-1)) res=T
	# Series IV
	if(r==4 && (s-1)%%2==0 && k<=(s) && (s%%3!=0)) res=T
	
	if(n<12) res=F
	if(k<3) res=F
	return(res)
}
