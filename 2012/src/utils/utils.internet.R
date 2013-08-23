###############################################################################
#
# TODO: Add comment
#
# Mar 30, 2012
# 2:11:16 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

#urlNews = "https://downloads.sourceforge.net/project/cippex/updates/news.txt?r=&ts=1333391266&use_mirror=softlayer"
#urlNews = 'http://sourceforge.net/projects/cippex/files/updates/news.txt/download'
#urlNews = "http://downloads.sourceforge.net/project/cippex/updates/news.txt?r=&amp;ts=1333391979&amp;use_mirror=hivelocity"
urlNews = 'https://research.cip.cgiar.org/confluence/download/attachments/48792806/news.txt'
newspath="temp/news.txt"
require(stringr)

has.connect.www<-function(){
	res=FALSE
	if(capabilities()["http/ftp"]){
		ok = try(zz <- url("http://www.google.com", "r")) #This address might be changed to another one
		# that is also available from China or others?!
		res=!str_detect(ok,"Error")
	}
	res
}

has.news.file <- function(){
	ok = FALSE
	try(unlink(newspath))
	ok = try(zz <- download.file(urlNews, newspath, method="curl", extra="-k"))
	ok = !str_detect(ok,"Error")
	if(ok){
		try(download.file(urlNews, newspath, mode="w", quiet=F, method="curl", extra="-k"))
		ok = file.exists(newspath)
	}
	ok
}

get.news.file = function(){
	
	readLines(newspath)
}

get.versions <- function(filepath=NULL){
	if(is.null(filepath)){
		filepath = urlNews
	}
	txt = get.news.file()
	txt = txt[str_detect(txt,"# Update version:")]
	rr=sapply(txt,str_extract,"[0-9].[0-9].[0-9]")
	as.character(rr)
}

uncompress <- function(update.path){
	oldwd = getwd()
	setwd(dirname(getwd()))
	
	path.7z = file.path(getwd(),"DC4RT","bin","7za.exe")
	cmd = paste(path.7z," x ", "-y ", update.path, sep="") # x extract with filepath conserved; -y overwrite
	ok = shell(cmd)
	setwd(oldwd)
	#print(paste("Status",ok))
	
	ok
}

get.pending.updates <-function(versions){
	vrs = sort(versions)
	dst = vrs
	res = dst[str_detect(dst,get.version.nr())]
	if(length(res)==0) return(vrs)
	return(dst[!str_detect(dst,get.version.nr())])
}

update.dc4rt <- function(hnf){
	#if(has.news.file()){
	if(hnf){
		versions = get.pending.updates(get.versions())
		n = length(versions)
		res = character(n)
		for(i in 1:n){
			fn = paste("DC4RT_",versions[i],"_.7z",sep="")
			#print(fn)
			dest = file.path(getwd(),"temp",fn)
			orig = paste("https://research.cip.cgiar.org/confluence/download/attachments/48792806/",fn,sep="")
			#print(dest)
			#print(orig)
			try(download.file(orig, dest, mode="wb", quiet=F, method="curl", extra="-k -C -O"))
			try({res[i] = uncompress(dest)})
			files = list.files("DC4RT",rec=T)
			unlink(dest)
		}
		res = paste(versions,res)
	}
	return(res)
}

do.update <-function(){
	xmin=1
	xmax=3
	
	pb <- winProgressBar("Checking for available updates ...", "Connecting to internet ... %",
			xmin, xmax, xmin)
	setWinProgressBar(pb, 1, "Updating ...","Getting update news.")
	hnf = has.news.file()
	
	setWinProgressBar(pb, 2, "Updating ...","Downloading ...")
	
	
	if(hnf){
		txt = get.news.file()
		#versions = get.pending.updates(get.versions())
		gvn = get.version.nr()
		#if(TRUE %in% str_detect(txt,gvn)){
		if(length(get.pending.updates(get.versions()))>=1){
			if(TRUE %in% str_detect(txt,gvn)){
				txt = txt[1:which(str_detect(txt,gvn))-1]
			}
			txt = paste(txt,collapse="\n")
			txt = paste("[Current version:",gvn,"]\nThe following upates are available:\n\n", 
					txt,"\n\nDo you want to update?")
			gconfirm(txt, title="Confirm", icon="info", handler = function(h,...){
						setWinProgressBar(pb, 3, "Updating ...","Installing ...")
				update.dc4rt(hnf)		
				#try(res=update.dc4rt(hnf))
				#ok = !str_detect(res,"Error")
				#if(!res) {
					gmessage("Successful update. Application will restart.", title="Info", icon="info")
					dispose(w)
					source("bin/app.R")
				#} else {
				#	gmessage("An error occured during update. Please contact r.simon@cgiar.org.", title="Info", icon="info")
				#}
				}
			)
			w=refresh(w)
			
		} else {
			gmessage("Your DataCollector installation is up-to-date.", title="Info", icon="info")
		}
	} else {
		gmessage("Updates were not accessible. \nThere seems to be a problem with the internet connection.
				\nTry to update at another moment."
						, title="Warning", icon="warning")
	}
	close(pb)
	
}

