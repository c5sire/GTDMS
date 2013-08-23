###############################################################################
# June 20, 2012
# 7:59:34 AM
# Author: Mirella Flores
# (c) International Potato Center
#
###############################################################################
if(!require('EBImage', character.only=TRUE)) install.packages('bin/EBImage.zip',repos=NULL)
library(EBImage)

morpho=function(i){
	
# deteccion de objetos
	
	pth=thresh(i[,,1],2,2,0.0175) 	#compara intensidad de pixeles
	kern=makeBrush(7,shape='disc')
	pf=closing(pth,kern)
	pfh=fillHull(pf)

#conteo de objetos
	
	nobj=bwlabel(pfh)
	
#eliminacion de objetos insignificantes
	
	pp=hullFeatures(nobj)[,'g.s']
	
	id = which(pp<1000)
	rm=rmObjects(nobj,id)
	
	pp1=hullFeatures(rm)[,'g.p']
	id1 = which(pp1<100)     		#puntos menores que
	rm1=rmObjects(rm,id1)
	
	pp2=hullFeatures(rm1)[,'g.p']
	id2 = which(pp2>1000)
	rm2=rmObjects(rm1,id2)
	#rm2=rm1
	#display(rm)
	
	if(max(rm2)==0){
		res='no se detectaron objetos'
		return(res)
	}else{
		
		#marcado de objetos reconocidos
		colorMode(rm2)=Grayscale
		sto=stackObjects(rm2,i)
		obj= paintObjects(rm2, i, col='red')
		
		#caracterizacion de objetos
		oc=ocontour(rm2)
		pp=hullFeatures(rm2)[,c('g.x','g.y','g.s','g.p','g.pdm','g.pdsd')] #puntos de los obj reconocidos
		
		font=drawfont(weight=600, size=16)
		
		if(class(pp)=="numeric"){
			ch=pp[1:2]
			ch=round(ch)
			ch1=pp[3:4]
			ch2=pp[4:6] 
			
			#numeracion de objetos
			ptx=drawtext(obj,xy=ch,labels=as.character(1),font=font,col="red")
			
			#salidas
			names(ch2)=c('p.cm','pdm.cm','pdsd.cm')
			res=c(ch,ch1,ch2)
			res=round(res,4)
			n=1
			
			
			#display(ptx)
			
			res=list(res,sto,ptx,n)
			
			return(res)
			
		}
		else{
			ch=pp[,1:2]
			ch=round(ch)
			ch1=pp[,3:4]
			ch2=pp[,4:6] 
			
			#numeracion de objetos
			ptx=drawtext(obj,xy=ch,labels=as.character(1:nrow(ch1)),font=font,col="red")
			
			#salidas
			colnames(ch2)=c('p.cm','pdm.cm','pdsd.cm')
			res=cbind(ch,ch1,ch2)
			n=nrow(ch)
			#cat('number of objects=',n,'\n')
			#cat('features:','\n')
			#display(ptx)
			
			res=list(res,sto,ptx,n)
			
			return(res)
		}
	}
	
}
imagen.pto.initial <- function(oc,pto2,ch,im,dimens,i,coord){
	
	pto = (oc[[1]][coord,])
	
	pto.min=min(pto[,i])
	pto.max=max(pto[,i])
	distn=(pto.max-pto.min)
	eje=pto2[i]-(ch-pto.min)
	
	if(i==1){
		
		dim.init<-as.numeric(image.identify(im,'%w')) }
	
	else if(i==2){
		
		dim.init<-as.numeric(image.identify(im,'%h')) 
	}
	
	pto.real=eje*dim.init/dimens
	distn.real=distn*dim.init/dimens
	
	result=list(pto.real, distn.real)
	return(result)
}

image.coordenada<-function(img,vect){
	#analisis del objeto seleccionado
	
	dist=matrix(0,length(vect),11)
	go=1
	for(k in vect){
		#deteccion
		x=img[[1]][,,,k]
		pth1=thresh(x[,,1],15,15,0.005)
		kern=makeBrush(13,shape='disc')
		pf1=closing(pth1,kern)
		pf1=fillHull(pf1)
		colorMode(pf1)=Grayscale
		ob1=bwlabel(pf1)
		oc=ocontour(pf1)
		ch=hullFeatures(pf1)[,c('g.x','g.y')]
		ch=round(ch)
		
		h1 = which(oc[[1]][,2]==ch[2]) # devuelve la coordenada
		v1 = which(oc[[1]][,1]==ch[1])
		
		result=list(oc,ch,h1,v1)
		
		return(result)
	}
}
image.identify<-function(ima,prop){
	
	tfn = file.path ("bin","ImageMagick","identify")
	
	dim1 <- system2(tfn,args=paste("-format", prop, ima,sep = " "), stdout = TRUE)
	#dim1<-system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\identify.exe",args=paste("-format", prop, ima,sep = " "), stdout = TRUE)
	return(dim1)
}

image.crop <-function(im){
	
	out <- file.path ("temp","output.jpg")
	
	#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-resample 20",im,o, sep = " "), stdout = TRUE)
	
	# resize image < 300 kb
	
	tfn = file.path ("bin","ImageMagick","convert")
	
	#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-resize 400000@",im,o, sep = " "), stdout = TRUE)
	tryCatch(system2(tfn,args=paste("-resize 400000@",im,out, sep = " "), stdout = TRUE), error = function(cond)NA ) 
	if (file.exists(out)){
	i = readImage(out)
	
	# call to main function for detect objects
	morp = morpho(i)
	
	# for display image: display(morp[[2]])
	
	# number of objects
	n=morp[[4]]
	
	dimens <- dim(i)
	
	if(n > 1){
		k=1
		for(k in 1:n){
			
			# get initial points in objects
			xy <- morp[[1]][k,c('g.x','g.y')]
			
			# get initial points in objects in image resized
			resu=image.coordenada(morp[2],vect=(k))
			
			# get initial points in objects in original image 
			ejex <- imagen.pto.initial(resu[[1]][1],xy,resu[[2]][1],im,dimens[1],1,resu[[3]])
			ejey <- imagen.pto.initial(resu[[1]][1],xy,resu[[2]][2],im,dimens[2],2,resu[[4]])
			
			# only for look for labels for size
			if (ejey[[2]][1]/ejex[[2]][1] < 1.2) {
				
				# split in 2 parts labels 'corbata'
				
				if (ejex[[2]][1]/ejey[[2]][1] > 5.5) {
					
					#convert -crop WxH {+-}x{+-}y {%} {!} input output
					outimage <- paste(out,".",k,"2.new.jpg",sep = "")
					
					#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					
					outimage <- paste(out,".",k,"3.new.jpg",sep = "")
					
					#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.7),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.3,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.7),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
				}
				else if (ejex[[2]][1]/ejey[[2]][1] > 3) { #verificar tooo era 4 ok
					
					#convert -crop WxH {+-}x{+-}y {%} {!} input output
					outimage <- paste(out,".",k,"1.new.jpg",sep = "")
					
					#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					
					outimage <- paste(out,".",k,"2.new.jpg",sep = "")
					
					#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1]*0.55,"x",ejey[[2]][1],"+",ejex[[1]][1]+(ejex[[2]][1]*0.45),"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
				}
				else {
					outimage <- paste(out,".",k,".new.jpg",sep = "")
					
					#convert -crop WxH {+-}x{+-}y {%} {!} input output
					#system2("C:\\Program Files\\ImageMagick-6.5.9-Q16\\convert.exe",args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
					system2(tfn,args=paste("-crop",paste(ejex[[2]][1],"x",ejey[[2]][1],"+",ejex[[1]][1],"+",ejey[[1]][1],sep=""),im, outimage, sep = " "), stdout = TRUE)
				}
			}
			
			k=k+1
		}
	}
	}
	if(file.exists(file.path ('temp','output.jpg'))) file.remove(file.path ('temp','output.jpg'))
}

add.legend <- function (year,autor,im){
	
	h=as.numeric(image.identify(im,'%h')) 

	w=as.numeric(image.identify(im,'%w')) 
	numberautor <- do.call(rbind, strsplit(autor,"\\,"))

	if ((length(numberautor))>1){
		str_autor = "Authors:"
	}	
	else str_autor = "Author:"
	if(w>h){
		point_size = round(as.numeric(w*0.01))
	}
	else{
		point_size = round(as.numeric(h*0.01))
	}
	
	point = h*0.96
	tfn = file.path ("bin","ImageMagick","convert")
	txt = paste('" ©Copyright ',year,' International Potato Center.\n ',str_autor,autor,' "',sep='')
	tryCatch(system2(tfn,args=paste("-pointsize ", point_size," -annotate +1+",point," ",txt," ",im," ",im, sep = ""), stdout = TRUE), error = function(cond)NA ) 

}

