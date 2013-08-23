###############################################################################
#
# TODO: Add comment
#
# Oct 25, 2011
# 12:55:34 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

createKML <- function(){
	books = get.fb.list()
	LocationID= character(1)
	Location = character(1)
	Latitude = numeric(1)
	Longitude= numeric(1)
	Country  = character(1)
	
	Year = integer(1)
	Crop = character(1)
	Genotype= character(1)
	Variable= character(1)
	db = as.data.frame(
			cbind(LocationID, Location, Latitude, Longitude, Country, Year,
					Genotype, Variable)
	)
	db = db[-1,]	
	for(i in 1:length(books)){
		
	}
}
