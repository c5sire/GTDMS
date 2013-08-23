###############################################################################
#
# Create the menu from a file 
#
# May 1, 2012
# 9:17:07 AM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

get.menu.file <- function(){
	read.csv(file.path("res","menu.csv"), header=T, strings=F,sep="\t")
}

# make action string
# aQuit  <- gaction(label="Quit",   icon="quit",  handler=function(h,...) dispose(w))

make.action <- function(id, label, icon, action){
	paste("a",id," = gaction(label = '",label ,"', icon='",icon,"', handler=function(h,...) ",action,"(w), parent=w)",sep="")
}

make.menu <- function(){
	# assume for the moment only up to 2nd level
	# get menu data
	mns = get.menu.file()
	# eliminate inactivated items: preceded with #
	its = mns$ID
	act = !str_detect(its,"#")
	mns = mns[act,]
	# get top level menus
	top = mns[str_detect(mns$parent,"top"),"ID"]
	# cycle through top menus
	n = length(top)
	ml = "mlist <- list(\n"
	
	for(i in 1:n){

		its =  mns[str_detect(mns$parent,top[i]),]
		#print(its)
		m = length(its$ID)
		if(m>0){
		
		#print(top[i])
		ml = paste(ml,"'",translate(top[i]),"' = list(\n", sep="")
		# get 2nd level menu items
		
		
		for(j in 1:m){
			sep = str_detect(its[j,"ID"],"__")
			if(sep) {
				itl = 'sep = list(separator = TRUE)'
			} else {
				itl = make.action(its[j,"ID"], translate(its[j,"ID"]), its[j,"icon"], its[j,"action"])
			}
			
			ml=paste(ml, itl,sep="")
			if(j<m) ml= paste(ml,",\n")
		}
		
		ml = paste(ml,"\n)", sep="")
		
		}
		if(i<n) ml = paste(ml,",\n", sep="")
		
	}
	ml = paste(ml,"\n)", sep="")
	fn = paste("main_",get.current.lang(),".R",sep="")
	fp = file.path("res","gui",fn)
	write(ml,fp)
}

get.menu <-function(forceNew=T){
	fn = paste("main_",get.current.lang(),".R",sep="")
	fp = file.path("res","gui",fn)
	if(!file.exists(fp) | forceNew){
		make.menu()
		#print("X")
	}
	source(fp)
	#gmenu(mlist, cont = w)
}

#get.menu(gwindow())
