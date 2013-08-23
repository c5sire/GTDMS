###############################################################################
#
# TODO: Add comment
#
# Apr 3, 2012
# 8:11:59 PM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################

wrapII = function(phrs){
	#print(phrs)
	paste("\"",phrs,"\"",sep="")
}


setCon = function(user, pwd){
	content ="
			@echo off
			rem remember the directory path to this bat file
			set dirPath=%~dp0
			
			rem need to reverse windows names to posix names by changing \ to /
			set dirPath=%dirPath:\\=/%
			rem remove blank at end of string
			set dirPath=%dirPath:~0,-1%
			
			rem - Customize for your installation, for instance you might want to add default parameters like the following:
			java -jar \"%dirPath%\"/lib/confluence-cli-2.1.0.jar %* --server http://research.cip.cgiar.org/confluence --user usr --password pwd %
			
			rem Exit with the correct error level.
			EXIT /B %ERRORLEVEL%
			
			"
	content = gsub("usr",user,content)
	content = gsub("pwd",pwd,content)	
	write(content,"bin/ac-2.1.0/confluence.bat")
}



getRemoteCon = function(){
	cfl = file.path("bin","ac-2.1.0","confluence --action ")
	cfl
}


existsPage = function(ppage, space="GTDBT"){
	ok = TRUE
	cfl = getRemoteCon()
	#out = "_con_test.txt"
	ppage = wrapII(ppage)
	cmd = paste(cfl,"getPageList","--space ",space,"","--title",ppage)
	
	runCmd = function(cmd){
		res=system(cmd, intern=T)
		res[1]!=""
	}
	ok = runCmd(cmd)
#	tryCatch(
#			runCmd(cmd)
#			, finally=ok<-FALSE)
	ok
}

getAttachments = function(page,space){
	cfl = getRemoteCon()
	cmd = paste(cfl,"getAttachmentList","--space ",space,"","--title \"",page,"\"")
	tbl=system(cmd, intern=T)
	rows = as.integer(strsplit(tbl[1]," ")[[1]][1])
	cols = 10
	data = as.data.frame(matrix("",nrow=rows,ncol=cols), stringsAsFactors=F)
	hdr=gsub("\"","",tbl[2])
	names(data) = strsplit(hdr,", ")[[1]]
	for(i in 3:(rows+2)){
		line = gsub("\"","",tbl[i])
		data[i-2,] = strsplit(line,", ")[[1]]
	}
	data
}

getChildren = function(page,space){
	cfl = getRemoteCon()
	cmd = paste(cfl,"getPageList","--space ",space,"","--title",page, " --children")
	lst=system(cmd, intern=T)
	if(length(lst)>3){
		lst = lst[3:(length(lst)-1)]
	} else {
		lst = NULL
	}
	lst
}


getLocalUserId = function(){
	ul = readLines("bin/ac-2.1.0/confluence.bat",n=-1)[12]
	p1 = strsplit(ul,"--user ")[[1]][2]
	p1 = strsplit(p1," --password ")[[1]][1]
	p1
}

addPermission = function(ppage, permission="view",users="local"){
	if(existsPage(ppage)){
		cfl = getRemoteCon()
		if(users=="local") users = getLocalUserId()
		users = wrapII(users)
		#ppage = wrapII(ppage)
		cmd = paste(cfl,"addPermissions","--space \"GTDBT\"","--title",ppage,"--permissions",permission,
				"--descendents","--userId",users)
		system(cmd)
	}
}


removePermission = function(ppage, permission="view",users="local"){
	if(existsPage(ppage)){
		cfl = getRemoteCon()
		if(users=="local") users = getLocalUserId()
		users = wrapII(users)
		ppage = wrapII(ppage)
		cmd = paste(cfl,"removePermissions","--space \"GTDBT\"","--title",ppage,"--permissions",permission,
				"--descendents","--userId",users)
		system(cmd)
	}
}

listPermissions = function(ppage, space){
	Permission = c("View","Edit")
	User = c("user","user")
	tbl = as.data.frame(cbind(Permission,User),stringsAsFactors=F)
	#if(existsPage(ppage,space)){
		cfl = getRemoteCon()
		users = getLocalUserId()
		users = wrapII(users)
		ppage = wrapII(ppage)
		cmd = paste(cfl," getPermissionList"," --space \"",space,"\""," --title ",ppage, sep="")
		res=system(cmd,intern=T)
		if(res[1]!="0 permissions in list"){
			tbl = tbl[-c(1,2),]
			for(i in 3:length(res)){
				tbl[i-2,]=strsplit(res[i],", ")[[1]]	
			}
			tbl$User = tolower(tbl$User)
		} else {
			#users = gsub("\"","",users)
			#tbl$Permission=strtoupper(users)
		}
		#print(res)
	#}
	tbl
}

canEdit = function(ppage, space){
	can = FALSE
	res= listPermissions(ppage, space)
	users = getLocalUserId()
	cond = res[((res$User==users & res$Permission=="Edit") | res$User=="user"), ]
	if(nrow(cond)>0){
		can=TRUE
	}
	can
}


deletePage = function(ppage){
	res = "ok"
	if(existsPage(ppage)){
		if(canEdit(ppage)){
			cfl = getRemoteCon()
			cmd = paste(cfl,"removePage","--space \"GTDBT\"","--title",ppage,"--descendents")
			system(cmd)
		} else{
			ppage=gsub("\"","",ppage)
			res = paste("User '",getLocalUserId(),"' tried to delete '",ppage,
					"' but has no permission.",sep="")
		}
	} else {
		ppage=gsub("\"","",ppage)
		res = paste("User '",getLocalUserId(),"' tried to delete '",ppage,
				"' but page does not exist.",sep="")
	}
	res
}


setRepoCon = function(w){
	
	w <- gbasicdialog(title="Enter your CIP assigned login and password", handler = function(h,...)
				setCon(svalue(usr),svalue(pwd)))
	#glabel("User login:", cont=w)
	usr <- gedit("[your login]",cont=w)
	pwd <- gedit("",cont=w)
	visible(pwd)<-FALSE
	visible(w, set=TRUE) ## show dialog
	
}

addAttachment <-function(file, ppage, space){
	res = "ok"
	if(existsPage(ppage, space=space)){
		if(canEdit(ppage,space)){
			cfl = getRemoteCon()
			cmd = paste(cfl," addAttachment"," --space \"",space,"\""," --title ",ppage," --file \"",file,"\"",sep="")
			system(cmd)
		} else{
			ppage=gsub("\"","",ppage)
			res = paste("User '",getLocalUserId(),"' tried to add attachments to '",ppage,
					"' but has no permission.",sep="")
		}
	} else {
		ppage=gsub("\"","",ppage)
		res = paste("User '",getLocalUserId(),"' tried to add attachments to '",ppage,
				"' but page does not exist.",sep="")
	}
	res
}

