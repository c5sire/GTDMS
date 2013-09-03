###############################################################################
#
#
# Apr 2, 2012
# 11:31:11 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


get.version.nr <-function(){
	#"1.2.0"
  res=Sys.getenv("DC4RT_VERSION")
  if(str_detect(res,";")){
    res = str_split(res,";")[[1]][2]
  }
  return(res)
}
