###############################################################################
#
# Translation and back translation functions to support different languages
#
# Apr 26, 2012
# 8:47:25 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################


####################
## Helper functions
get.current.lang <-function(){
	prefs = get.prefs()
	lang=prefs[prefs$pr_name=="language","pr_past"]
	if(length(lang)!=1) lang="en_US"
	return(lang)
}

get.lang.label <- function(id_label, lang){
	dict = get.dict()
	term = dict[dict$id_label==id_label,lang]
	if(length(term)==0) term=id_label
	return(term)
}

get.id.label <- function(label, lang){
	dict = get.dict()
	coln = colnames(dict)
	idx = which(coln==lang)
	term = dict[dict[,idx]==label,"id_label"]
	if(length(term)==0) term=label
	return(term)
}

#########################
## principal functions

translate <- function(id_label){
	# get current language
	lang = get.current.lang()
	# lookup character string
	n = length(id_label)
	labl = character(n)
	for(i in 1:n){
		labl[i] = get.lang.label(id_label[i], lang)
	}
	
	# return string
	return(labl)
}

retranslate <- function(label){
	lang = get.current.lang()
	# lookup character string
	n = length(label)
	labl = character(n)
	for(i in 1:n){
		labl[i] = get.id.label(label[i], lang)
	}
	
	return(labl)
}
