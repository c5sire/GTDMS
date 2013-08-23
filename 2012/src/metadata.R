###############################################################################
#
# TODO: Add comment
#
# Sep 26, 2011
# 2:21:50 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

get.dict <- function(){
	read.csv(file.path("res","dictionary.csv"),header=T,sep="\t", stringsAs=F)
}

get.meta.label <- function(select, lang="en_US"){
	meta = get.dict()
	row.names(meta)=meta[,"id_label"]
	#labl = paste("label_",lang,sep="")
	meta[select,lang]
}
