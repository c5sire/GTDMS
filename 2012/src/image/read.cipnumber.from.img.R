###############################################################################
#
# TODO: Add comment
#
# May 8, 2012
# 11:35:42 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################
library(stringr)

read.txt.from.image <- function(fp){
	#call tesseract via command line from R
	cmd = paste("tesseract",fp,fp)
	system(cmd)
	#read archive (several lines)
	txt = readLines(paste(fp,".txt",sep=""))
	#just make one string out of it separating former lines with ;
	txt = paste(txt,collapse=";")
	return(txt)
}

get.cipnumber.from.txt <-function(txt){
	cpn = "(CIP)?[0-9@y]{6}(\\.[0-9@y]{2,4})?" 
	#make sure to preced cip number with CIP
	txt = str_replace(txt," ","")
	txt = str_replace(txt,"@","0")
	txt = str_replace(txt,"y","1")
	cip = paste("CIP",str_extract(txt,cpn),sep="")
	return(cip)
}

read.cipnumber.from.image <- function(fp){
	#CIP number pattern/regular expression
	txt = read.txt.from.image(fp)
	cip = get.cipnumber.from.txt(txt)
	return(cip)
}

#read.cipnumber.from.image("test1.jpg")

# for batch use
img = paste("test",1:9,".JPG",sep="")
sapply(img,read.cipnumber.from.image)
