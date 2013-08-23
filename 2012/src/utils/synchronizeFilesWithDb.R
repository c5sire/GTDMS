###############################################################################
#
# TODO: Add comment
#
# Mar 26, 2012
# 10:50:37 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

synchronizeFilesWithDb <- function(data.fb){
	res = "ok"
	# get current crop
	#crop = getCurrentCrop()
	#print(crop)
	# get list of all files
	fbfl = getFieldBooksNames()
	
	# get list of all fieldbooks from db
	#data = get.fieldbooks()
  data = data.fb
	#print(nrow(data))
	if(length(fbfl)<1 | length(fbfl)==nrow(data)) {
		#print("fb.connect")
		fb.connect()
		#print("return")
		return()
		#print("return check")
	} else 	if(nrow(data)<length(fbfl)){
	# get list of files not in db
		#print("if 2")
		if(nrow(data)<1) {
			#print("if 3")
			ndat = fbfl
		} else {
			#print("if 4")
			#ndat = !(data[,"Fieldbook ID"] %in% fbfl)
			#ndat = fbfl[!(fbfl %in% data[,translate("_FIELDBOOK_ID_")])]
			ndat = fbfl[!(fbfl %in% data[,translate("_FIELDBOOK_ID_")])]
		}
		#print(ndat)
		#res = ndat
	# for each of un-registered files do update
		n = length(ndat)
		#print(n)
		for(i in 1:n){
			fp = getFieldBookPath(ndat[i])
			#print(fp)
			wb=read.fb(fp,"Minimal")
			checkin = get.iso.timestamp()
			if(!is.na(wb["Short name or Title",2])){
			update.trial.in.status.db(fieldbook_id=wb["Short name or Title",2],
					#crop=wb["Crop",2],
					planting_date= wb["Begin date",2], 
					country=wb["Country",2], trial_type=wb["Type of Trial",2], 
					contact=wb["Leader",2], checkin= checkin, target_path=fp)
			}
		}
	}
	#res
}
