###############################################################################
#
# TODO: Add comment
#
# Apr 10, 2012
# 3:23:18 PM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

add.localities <- function(w){
	
	# close excel files
	close.all.excel()
	
	# extract only relevant part from excel
	fp = file.path(getwd(),"res","Master-list-trial-sites.xlsx")
	sheetName="Sites"
	db = read.excel(fp, sheetName)
	br = db[1,]
	data = db[-1,-1]
	
	# ... and create new excel with only this
	file = file.path(getwd(),"temp","NewLocations.xlsx")
	write.xlsx2(data, file, sheetName="Locations", 
			col.names=T, row.names=F, append=FALSE)
	
	# open site-only excel for editing
	shell(file)
	# after close: check for duplicates, additions and modifications: do not accept dups
	
	# check: format of coordinates: is numeric with at least 4 digits after the point?
	# check: country names
	# check: coordinates within country; function pip in splancs better 'inout' or use map.where from maps
	
	# save new-sites-list locally 
	# try to upload on confluence! if not possible keep copy
	# check on program start if pending site-list update!
	
}
