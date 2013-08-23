###############################################################################
# June 08, 2012
# 7:59:34 AM
# Author: Mirella Flores
# (c) International Potato Center
#
###############################################################################

# tesseract


read.txt.from.image <- function(fp){
	
	tmpfile <- file.path('temp','unsharped.jpg.txt')
	
	if(file.exists(tmpfile)) file.remove(tmpfile)
	
	#call tesseract via command line from R
	tfn = file.path ("bin","tesseract","tesseract")
	
	cmd = paste(tfn,fp,fp)
	
	system(cmd,ignore.stdout = TRUE,show.output.on.console = FALSE)
	
	#read archive (several lines)
	
	txt = readLines(paste(fp,".txt",sep=""))
	
	#just make one string out of it separating former lines with ;
	
	txt = paste(txt,collapse=";")
	
	file.remove(paste(fp,".txt", sep = ""))
	
	return(txt)
	
}

get.cipnumber.from.txt <-function(txt){
	
	cpn = "(CIP)?[0-9@yDZOI]{6}(\\.[0-9@yDZO]{1,4})?" 
	
	#make sure to preced cip number with CIP
	
	txt = str_replace_all(txt," ","")
	txt = str_replace_all(txt,"@","0")
	txt = str_replace_all(txt,"y","1")
	txt = str_replace_all(txt,"D","0")
	txt = str_replace_all(txt,"Z","2")
	txt = str_replace_all(txt,"A","4")
	txt = str_replace_all(txt,"/","7")
	txt = str_replace_all(txt,"B","8")
	txt = str_replace_all(txt,"O","0")
	txt = str_replace_all(txt,"é","6")
	txt = str_replace_all(txt,"ó","6")
	txt = str_replace_all(txt,"®","0")
	txt = str_replace_all(txt,"I","1")
	txt = str_replace_all(txt,"Â","")
	txt = str_replace_all(txt,"\\]","1")
	txt = str_replace_all(txt,"\\[","1")
	cip = str_extract(txt,cpn)
	
	if(is.na(cip)) cip = ''
	
	return(cip)
}


read.cipnumber.from.image <- function(fp){
	
	#CIP number pattern/regular expression
	
	txt = read.txt.from.image(fp)
	
	cip = get.cipnumber.from.txt(txt)
	
	if (cip == '111111' || cip == '777777' || str_detect(cip,'12345')) cip = ''

	return(cip)
	
}

image.zbar <- function (imagei){
	cip = ""

	tfn = file.path ("bin","zbar","zbarimg")
	
	try(cip <- system2(tfn, args=paste("-q --raw",imagei), stdout = TRUE, stderr = FALSE), silent = TRUE)
	
	#try(A <- system2("C:\\Program Files\\ZBar\\bin\\zbarimg.exe", args=paste("-q --raw",imagei, sep = " "), stdout = TRUE, stderr = FALSE), silent = TRUE)
	return(cip)
}

image.unsharp <- function (imagei,params){
	
	tmpfile <- file.path('temp',"unsharped.jpg")
	
	tfn = file.path ("bin","ImageMagick","convert")
	
	system2(tfn, args=paste(params,imagei, tmpfile, sep = " "), stdout = TRUE, stderr = FALSE)

	#	system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe", args=paste(params,imagei, tmpfile, sep = " "), stdout = TRUE, stderr = FALSE)
	
}

image.typefile <- function (namefile){
	typefile = ""
	typefile <- do.call(rbind, strsplit(namefile,"\\."))
	typefile <- typefile[,length(typefile)]	
	return(typefile)
}

image.name <- function (namefile){
	
	typefile <- paste('.',image.typefile(namefile),sep='')
	
	txt 	 <- str_replace_all(namefile,typefile,"")
	
	txt		 <- basename(txt) #### arreglarrrrrrrrr
	
	return(txt)
}

file.verify.name <- function (namefile){
	blank <- do.call(rbind, strsplit(namefile," "))
	
	if (length(blank) > 1){
		N = paste(blank[1],blank[2], sep = "-")
		file.rename(namefile, N)
		namefile = N
	}
	return(namefile)
}

compare.result <- function(cipz,cipt,list.cip) {
	
	cip = NULL
	strcip = NULL
	cipnumber = NULL
	list.cip = str_replace_all(list.cip,"CIP","")
	ciptmp=c(cipt,cipz)

	if (length(ciptmp)){
		for (i in 1:length(ciptmp)) {
		if (ciptmp[i] %in% list.cip) cip = c(cip,ciptmp[i])
	}}

	#if (!is.null(cipz)) if (cipz %in% list.cip) cip = cipz
	#if (!is.null(cipt)) if (cipt %in% list.cip) cip = c(cipt,cip)

	if(is.null(cip)) {
		
		cip = moda.cipnumber(ciptmp)
		
		if (!is.null(cip)){
			if (length(cip)>1) strcip = 'Please-check-CIP'
			else {
				if (cip != '') strcip = 'Please-check-CIP'
			}}
	}
	else {
		cip = moda.cipnumber(cip)
		strcip = 'CIP'
	}	

	if(!is.null(strcip)) {
		if (length(cip)>1) {
			for(i in 1:length(cip)) {
				a=str_detect(cip, cip[i])
				#if(TRUE %in% a) {
					x=which(a==TRUE)
				if (length(x)>1){
					for (j in 1:length(x)) {
						if(nchar(cip[x[j]])>nchar(cip[i])) {
						cip=cip[-i]
					
				}}
				}}
		}

	 cipnumber = paste(strcip,paste(cip, collapse='-'), sep = "")
	}
	return(cipnumber)
}


image.rename <- function(oldfilesi,filecip){
	
	tmpfile <- file.path('temp','unsharped.jpg')
	
	file_exists = list.files(".", pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
	
	tryCatch(image.crop(oldfilesi), error = function(cond)NA ) 
	
	labelfiles=file.path('temp',list.files('temp', pattern = ".new.jpg"))
		
		if(!length(labelfiles)) labelfiles=oldfilesi
		
		x <- 1
		
		CIPN=NULL
		CIPT=NULL
		CIPZ=NULL
		CIPN1=NULL
		cipnumber=NULL
		
		for(x in 1:length(labelfiles)){
			
			tryCatch({
						
				Z <- image.zbar(labelfiles[x])
				
				T <- read.cipnumber.from.image(labelfiles[x])
					 if (Z != '' && length(Z)) CIPZ = c(Z,CIPZ) 
					 if (T != '' && length(T)) CIPT = c(CIPT,T) 					
				
											
					params = "-unsharp 6x3"
					image.unsharp(labelfiles[x],params)
					
					if(file.exists(tmpfile)){
						Z <- image.zbar(tmpfile)
						T <- read.cipnumber.from.image(tmpfile)
						if (Z != '' && length(Z)) CIPZ = c(Z,CIPZ) 
						if (T != '' && length(T)) CIPT = c(CIPT,T) 
					file.remove(tmpfile)
					}
		
					params = "-unsharp 3x1+7"
					image.unsharp(labelfiles[x],params)
					
					if(file.exists(tmpfile)){
						Z <- image.zbar(tmpfile)
						T <- read.cipnumber.from.image(tmpfile)
						if (Z != '' && length(Z)) CIPZ = c(Z,CIPZ) 
						if (T != '' && length(T)) CIPT = c(CIPT,T) 
					}
									
			}, error=function(cond) NA )
			
			if(labelfiles[x]!=oldfilesi)	file.remove(labelfiles[x])
			
		} 
				
		#if (length(CIPZ)) CIPZ = moda.cipnumber(CIPZ)
		#if (length(CIPT)) CIPT = moda.cipnumber(CIPT)

		CIPN1 = compare.result(CIPZ,CIPT,filecip)

		if (!length(CIPN1) || is.null(CIPN1)){
			
			Z <- image.zbar(oldfilesi)
			T <- read.cipnumber.from.image(oldfilesi)
			if (Z != '' && length(Z)) CIPZ = Z 
			if (T != '' && length(T)) CIPT = T
			CIPN1 <- compare.result(CIPZ,CIPT,filecip)
		}
				
		if(length(CIPN1) || !(is.null(CIPN1))){
						
			#cipnumber <- paste(CIPN1, image.typefile(oldfilesi), sep = ".")
			
			#if(cipnumber %in% file_exists) cipnumber <- paste(CIPN1,'-1',image.typefile(oldfilesi), sep = ".")
			cipnumber <-CIPN1
			if(cipnumber %in% file_exists) cipnumber <- paste(CIPN1,'-1',sep = "")
			
		}
		
		if(file.exists(tmpfile)) file.remove(tmpfile)
		
		return (cipnumber)
		
} 

moda.cipnumber <- function(x) {
	z <- table(x)
	n <- names(z)[z == max(z)]
	
	return(n)
}

menu.rename <- function(dir_initial,fbp,book,ppl,yeardate,autor){

	part_plant = part.plant(ppl)
	# create folder
	dir_final <- file.path(fbp,part_plant) 

	if(!file.exists(dir_final)) dir.create(dir_final, recursive = TRUE)
		
	# copy files to new directory
	
	oldfiles=list.files(dir_initial, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
	
	contador <- 0
	
	if (length(oldfiles)){
		
		pb <- winProgressBar("Renaming", "Progress in %",0, 100, 1)
		
		file.copy(file.path(dir_initial,oldfiles), dir_final)
				
		listcipn= read.list.cipnumber(book,'Material List')
		
		if (!length(listcipn)){
			listcipn = file.path('bin','cipnumber.txt')
			listcipn = readLines(listcipn)
		}
		# list into new directory
		oldfiles = list.files(dir_final, pattern = ".jpg|.png|.PNG|.JPG|.JPEG|.jpeg|.bmp")
		
		oldfiles = file.path(dir_final,oldfiles)
		
		mdata <- read.xls(book,'Minimal')
		print(mdata)
		
		keys <- paste(ppl,';',getCurrentCrop())
		
		for(i in 1:length(oldfiles)) {
			
			# main function: recursive rename
			oldfiles[i] = file.verify.name(oldfiles[i])
			
			CIPN <- image.rename(oldfiles[i],listcipn)
		
			if (length(CIPN)){
				
				tmpimage=image.change.format(oldfiles[i],'.png')
				
				if(file.exists(tmpimage)) { 
					file.remove(oldfiles[i])
					oldfiles[i]=tmpimage
					
				}
		
				#CIPN	= image.name(CIPN)

				new_name= file.path(dir_final,paste(CIPN,'-',part_plant,'.png',sep=''))
				file.rename(oldfiles[i],new_name)
				
				#TODO CIPN!
				#nplot	= read.xls(book,'Material List',CIPN)
				nplot   = length(CIPN)
				mdata1  = c(mdata,CIPN,CIPN) 
			
				add.legend(yeardate,autor,new_name)
				
				print(new_name)
				print(CIPN)
				print(mdata1)
				print(nplot)
				print(keys)
			
				add.metadata(new_name,CIPN,mdata1,nplot,keys,'N')	
				
				contador= contador+1
			} 
			
			setWinProgressBar(pb, round(i/length(oldfiles)*100, 0), label=paste(round(i/length(oldfiles)*100, 0),"% done"))
		}
		
		close(pb)
}
	msg = paste("Renamed:",contador,"files", sep = " ")
	
	gmessage(msg,ico="info")
}

part.plant <- function (p){
	if (p == 'plant') return('p') 
	if (p == 'flower') return('f') 
	if (p == 'flower dissection') return('df') 
	if (p == 'fruit') return('b')
	if (p == 'seeds') return('s')
	if (p == 'tuber') return('t')
	if (p == 'root') return('r')
	if (p == 'sprout') return('br')
	if (p == 'herbarium') return('hr')
	if (p == 'habitat') return('hb')	
}

get.org.list= function(){

	org = c('plant','flower','flower dissection','fruit','seeds','tuber','root','sprout','herbarium','habitat')
	return(org) 
}

create.img.Dlg = function(w){
	
	dput("cancel",file="bin/temp.txt")
	
	win <- gbasicdialog(title='Add images to experiment', handler = function(h,...) {
				
			if(!is.na(svalue(gl[5,1])) && !is.na(svalue(gl[1,1]))){
				
					result = c(svalue(gl[1,2]),svalue(gl[2,2]),svalue(gl[5,1]),svalue(gl[3,2]))
					dput(result,file="bin/temp.txt")
			}
			}, parent=w)
	
	gl = glayout(cont=win)
	gl[1,1]=glabel("Experiment:", cont=gl)
	gl[1,2]=gcombobox(get.fb.list(), cont=gl)
	gl[2,1]=glabel("Types of picture:", cont=gl)
	gl[2,2]=gcombobox(get.org.list(), cont=gl)
	gl[3,1]=glabel("Author(s):", cont=gl)
	gl[3,2]=gedit("a", cont=gl)
	gl[4,1]=gbutton("Select ...", container=gl,
			 handler = function(h,...) {
				 image.dir = choose.dir()
				 
				 if(!is.na(image.dir)){
					 
					 gl[5,1]=glabel(image.dir, cont=gl)
					 svalue(gl[5,1])= image.dir
					 gl[6,1]=glabel("                                    ", cont=gl)
				 }
				 else gl[6,1]=glabel("Enter images directory", cont=gl)
				 
				 })
	
	visible(win, set=TRUE) ## show dialog
	fn=dget("bin/temp.txt")
	dput("cancel",file="bin/temp.txt")
	fn
}

get.images <- function (w){
	
	tryCatch({
				
		res = create.img.Dlg(w)
		
		if ( length(res) == '4'){
		
			year_m = "20[0-9]{4}" 
			dir_date <- str_extract(res[1],year_m)
			yeardate <- str_extract(dir_date,"[0-9]{4}")
			fbp =file.path(getFieldBooks(getCurrentCrop()),dir_date ,res[1])

			#book =file.path(fbp,paste(res[1],'.xls',sep=''))
			book = paste(fbp,'.xls',sep='')

			menu.rename(res[3],fbp,book,res[2],yeardate,res[4])
			
		}
	}, error=function(cond) NA )
} 

read.list.cipnumber <- function(xls,t_sheet){
	
	wb  	<- loadWorkbook(xls)
	sheets	<- getSheets(wb)
	
	sheet 	<- sheets[[t_sheet]]
	if (t_sheet=='Material List'){
		n = sheet$getLastRowNum()+1
		m = 10
		v = readColumns(sheet,1,m,1,n)
		#df = as.data.frame(v[,3],stringsAsFa=F)
		df =as.character(v[,"Institutional.number"])
	}
	return(df)
}

image.change.format<-function(ima,ext){
	
	tfn = file.path ("bin","ImageMagick","convert")
	
	typefile <- paste('.',image.typefile(ima),sep='')
	
	ima2	 <- str_replace_all(ima,typefile,ext)
	
	try(system2(tfn,args=paste(ima,ima2,sep = " "), stdout = FALSE), silent = TRUE)
	
	return(ima2)
}
