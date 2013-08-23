###############################################################################
#
# TODO: Add comment
#
# Mar 13, 2012
# 11:35:41 AM
# Author: Reinhard Simon (rsimon)
# (c) International Potato Center
#
###############################################################################

responseSelectionSingleExp <- function(win){
	w <- xgwindow("Preferences",visible=F, parent=win)
	svalue(w)="Response to selection for a single experiment"
	#window <- gframe("Response to selection for a single experiment",cont=w)
	window <- ggroup(cont=w)
# Cargar paquetes
#require(gWidgetsRGtk2)
#equire(cairoDevice)
#options("guiToolkit"="RGtk2")
# Drawing function
updatePlot <- function(h,...) {
	r <- seq(1,floor(svalue(N)/svalue(sg1)),1)
	g <- floor(svalue(N)/r)
	alpha <- svalue(sg1)/g
	z <- dnorm(qnorm(1-alpha))
	i <- z/alpha
	rho <- sqrt(svalue(sigmaG2) / (svalue(sigmaG2) + svalue(sigmaE2)/r))
	R <- i*rho
	plot(r, R, xlab="Number of replications", ylab="Response to selection", type="b", 
			xlim=range(r), ylim=range(R))
	points(r[match(max(R),R)], max(R), col = "red", pch=18)
	mtext(paste("Optimum number of replications = ", r[match(max(R),R)]), line=2.9)
	mtext(paste("Number of genotypes at optimum = ", g[match(max(R),R)]), line=1.7)
	mtext(paste("Response to selection at optimum = ", round(max(R),2)), line=0.5)
	#}
	#tkrplot(window,make.plot)
}

BigGroup <- ggroup(cont=window)
group <- ggroup(horizontal=FALSE, container=BigGroup)


# Widgets
tmp <- gframe("Plot capacity", container=group)
N <- gspinbutton(from=100, to=10000, by=100, value=1000, handler = updatePlot, cont=tmp) # Plot capacity
tmp <- gframe("Number of selected genotypes", container=group)
sg1 <- gspinbutton(from=1, to=10000, by=10, value=10, handler = updatePlot, cont=tmp) # selected genotypes
tmp <- gframe("Genotypic variance", container=group)
sigmaG2 <- gspinbutton(from=0.1, to=1000, by=0.1, value=1, digits=3, handler = updatePlot, cont=tmp) # Genotypic variance
tmp <- gframe("Error variance", container=group)
sigmaE2 <- gspinbutton(from=0.1, to=1000, by=0.1, value=1, digits=3, handler = updatePlot, cont=tmp) # Error variance

visible(w)=T
}


