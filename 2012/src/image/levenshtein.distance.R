###############################################################################
#
#
# May 8, 2012
# 5:26:04 PM
# Author: Reinhard Simon (user)
# (c) International Potato Center
#
###############################################################################


levenshtein.distance <- function(string.1, string.2, subst.cost=1) {
	
	c1 <- strsplit(string.1,split="")[[1]]     
	c2 <- strsplit(string.2,split="")[[1]]     
	n <- length(c1)
	m <- length(c2)
	
	d <- array(0,dim=c(n+1,m+1))  
	
	d[,1] <- 1:(n+1)
	d[1,] <- 1:(m+1)
	d[1,1] <- 0		
	
	
	
	for (i in 2:(n+1)) {
		
		for (j in 2:(m+1)) {
			
			if (c1[i-1] == c2[j-1]) cost <- 0 else cost <- subst.cost
			
			d[i,j] <- min(d[i-1,j] + 1,    # insertion
					d[i,j-1] + 1,    # deletion
					d[i-1,j-1] + cost) # substitution
		}
		
		
	}
	
	d[n+1,m+1]
	
} 
