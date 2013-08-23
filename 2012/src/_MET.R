# R script for DataCollector for Roots and Tubers
# Analysis of a RCBD with multiple environments
# Raul H. Eyzaguirre P.
# modified by R. Simon
# International Potato Center
# 18-05-2011
#################################################################################################

#sink("NUL")

# Libraries

if (sum(.packages(all.available = TRUE)=="lme4")==0) install.packages("lme4")
if (sum(.packages(all.available = TRUE)=="agricolae")==0) install.packages("agricolae")

#library(lme4)
#library(agricolae)


# A data set with a selected trait to analyze must be submited to R.
# Data have been loaded in object "datos" in R.
# Column names are "geno" for genotypes, "env" for environments, "rep" for replicates
# or blocks and "y" for the trait to analyze.

#################################################################################################
##############################      Begin function analis      ##################################
#################################################################################################

analis <- function(geno, env, rep, yobs, yp=NULL, numrep=1, nmis, c3, titulo="", tam=.8, traitn=""){
	
	#############################################################################################
	## 0 - Settings
	if (c3==1) y <- yobs else y <- yp
	
	#############################################################################################
	## 1 - Descriptive statistics
	over.stat <- summary(yobs)
	int.mean <- tapply(y, list(geno, env), mean, na.rm=T)
	overall.mean <- mean(int.mean, na.rm=T)
	env.mean <- apply(int.mean, 2, mean, na.rm=T)
	geno.mean <- apply(int.mean, 1, mean, na.rm=T)
	env.num <- length(env.mean)
	geno.num <- length(geno.mean)
	PC <- min(env.num, geno.num)-1
	int.eff <- int.mean + overall.mean
	for (i in 1:env.num) int.eff[,i] <- int.eff[,i] - geno.mean
	for (i in 1:geno.num) int.eff[i,] <- int.eff[i,] - env.mean
	
	#############################################################################################
	## 2.1 - Regression-stability for genotypes
	a <- NULL; b <- NULL; se <- NULL
	ms_dev <- NULL; ms_gxe <- NULL; ms_entry <- NULL; ms_reg <- NULL
	for (i in 1:geno.num){
		modelo <- lm(int.mean[i,]~env.mean)
		a[i] <- coef(modelo)[1]
		b[i] <- coef(modelo)[2]
		se[i] <- summary.lm(modelo)$coefficients[2,2]
		ms_dev[i] <- anova(modelo)[2,3] 
		ms_gxe[i] <- sum((int.mean[i,]-geno.mean[i]-env.mean+overall.mean)^2)/(env.num-1)
		ms_entry[i] <- sum((int.mean[i,]-geno.mean[i])^2)/(env.num-1)
	}
	stability_geno <- cbind(b, se, ms_dev, ms_entry, ms_gxe)
	row.names(stability_geno) <- levels(geno)
	names(a) <- levels(geno)
	names(b) <- levels(geno)
	if (env.num > 2){
		x <- NULL; ypred <- NULL; ymean <- NULL
		for (i in 1:length(y)){
			x[i] <- env.mean[names(env.mean)==env[i]]
			ypred[i] <- a[names(a)==geno[i]] + b[names(b)==geno[i]]*x[i]
			ymean[i] <- int.mean[row.names(int.mean)==geno[i], colnames(int.mean)==env[i]]
		}
		drg_sc <- sum((ypred-ymean)^2)
		hrg_gl <- geno.num - 1
		drg_gl <- (geno.num-1)*(env.num-1)-hrg_gl
		drg_cm <- drg_sc/drg_gl	
	} else {
		drg_sc <- NA; hrg_gl <- NA; drg_gl <- NA; drg_cm <- NA
	}
	
	#############################################################################################
	## 2.2 - Regression-stability for environments
	a <- NULL; b <- NULL; se <- NULL
	ms_dev <- NULL; ms_gxe <- NULL; ms_entry <- NULL; ms_reg <- NULL
	for (i in 1:env.num){
		modelo <- lm(int.mean[,i]~geno.mean)
		a[i] <- coef(modelo)[1]
		b[i] <- coef(modelo)[2]
		se[i] <- summary.lm(modelo)$coefficients[2,2]		
		ms_dev[i] <- anova(modelo)[2,3]
		ms_gxe[i] <- sum((int.mean[,i]-env.mean[i]-geno.mean+overall.mean)^2)/(geno.num-1)
		ms_entry[i] <- sum((int.mean[,i]-env.mean[i])^2)/(geno.num-1)
	}
	stability_env <- cbind(b, se, ms_dev, ms_entry, ms_gxe)
	row.names(stability_env) <- levels(env)
	names(a) <- levels(env)
	names(b) <- levels(env)
	if (geno.num > 2){
		x <- NULL; ypred <- NULL; ymean <- NULL
		for (i in 1:length(y)){
			x[i] <- geno.mean[names(geno.mean)==geno[i]]
			ypred[i] <- a[names(a)==env[i]] + b[names(b)==env[i]]*x[i]
			ymean[i] <- int.mean[row.names(int.mean)==geno[i], colnames(int.mean)==env[i]]
		}
		dre_sc <- sum((ypred-ymean)^2)
		hre_gl <- env.num - 1
		dre_gl <- (geno.num-1)*(env.num-1)-hre_gl
		dre_cm <- dre_sc/dre_gl
	} else {
		dre_sc <- NA; hre_gl <- NA; dre_gl <- NA; dre_cm <- NA
	}
	
	#############################################################################################
	## 3 - ANOVA
	#print(names(y))
	add.anova <- aov(y ~ geno + env + rep %in% env + geno:env)
	at <- summary(add.anova)
	at <- cbind(at[[1]][,1:4], at[[1]][,5])
	at[5,1] <- at[5,1]-nmis
	at[5,3] <- at[5,2]/at[5,1]
	at[1:4,4] <- at[1:4,3]/at[5,3]
	at[1:4,5] <- pf(at[1:4,4], at[1:4,1], at[5,1], lower.tail=F)
	at1 <- at; at2 <- at; at3 <- at
	png(filename = paste("temp/",traitn,"_dp1.png",sep=""), width = 900, height = 900)
	plot(add.anova, 1, sub.caption=traitn)
	dev.off()
	png(filename = paste("temp/",traitn,"_dp2.png",sep=""), width = 900, height = 900)
	plot(add.anova, 2, sub.caption=traitn)
	dev.off()
	btest <- bartlett.test(rstandard(add.anova) ~ paste(geno,env))
	btest$data.name <- "standardized residuals by treatments"
	ntest <- shapiro.test(rstandard(add.anova))
	ntest$data.name <- "standardized residuals"	
	
	#############################################################################################
	## 3.1 - ANOVA for G fixed, E fixed, B random
	if (env.num > 2){
		hrg_sc <- at[4,2] - drg_sc
		hrg_cm <- hrg_sc/hrg_gl
		hrg_f <- hrg_cm/drg_cm
		hrg_p <- pf(hrg_f, hrg_gl, drg_gl, lower.tail=F)
		drg_f <- drg_cm/at[5,3]
		drg_p <- pf(drg_f, drg_gl, at[5,1], lower.tail=F)
	} else {
		hrg_sc <- NA; hrg_cm <- NA; hrg_f <- NA; hrg_p <- NA; drg_f <- NA; drg_p <- NA
	}
	if (geno.num > 2){
		hre_sc <- at[4,2] - dre_sc
		hre_cm <- hre_sc/hre_gl	
		hre_f <- hre_cm/dre_cm
		hre_p <- pf(hre_f, hre_gl, dre_gl, lower.tail=F)
		dre_f <- dre_cm/at[5,3]
		dre_p <- pf(dre_f, dre_gl, at[5,1], lower.tail=F)
	} else {
		hre_sc <- NA; hre_cm <- NA; hre_f <- NA; hre_p <- NA; dre_f <- NA; dre_p <- NA
	}
	at1[2,4] <- at1[2,3]/at1[3,3]
	at1[2,5] <- pf(at1[2,4], at1[2,1], at1[3,1], lower.tail=F)
	filaux <- at1[5,]
	at1[5,] <- c(hrg_gl, hrg_sc, hrg_cm, hrg_f, hrg_p)
	at1 <- rbind(at1, c(drg_gl, drg_sc, drg_cm, drg_f, drg_p))
	at1 <- rbind(at1, c(hre_gl, hre_sc, hre_cm, hre_f, hre_p))
	at1 <- rbind(at1, c(dre_gl, dre_sc, dre_cm, dre_f, dre_p))
	at1[9,] <- filaux
	at1[1,6] <- qt(.975, at1[9,1])*sqrt(at1[9,3]*2/numrep/env.num)
	at1[2,6] <- qt(.975, at1[3,1])*sqrt(at1[3,3]*2/numrep/geno.num)
	at1[4,6] <- qt(.975, at1[9,1])*sqrt(at1[9,3]*2/numrep)
	row.names(at1) <- c("G", "E", "R:E", "GxE", "- Het.Regr.G", "- Dev.Regr.G",
			"- Het.Regr.E", "- Dev.Regr.E", "Residuals")
	colnames(at1)[5:6] <- c("Pr(>F)", "LSD5")
	cv <- cv.model(add.anova)
	
	#############################################################################################
	## 3.2 - ANOVA for G fixed, E random, B random
	at2[1,4] <- at2[1,3]/at2[4,3]
	at2[1,5] <- pf(at2[1,4], at2[1,1], at2[4,1], lower.tail=F)
	at2[2,4] <- at2[2,3]/at2[3,3]
	at2[2,5] <- pf(at2[2,4], at2[2,1], at2[3,1], lower.tail=F)
	at2[1,6] <- qt(.975, at2[4,1])*sqrt(at2[4,3]*2/numrep/env.num)
	row.names(at2) <- c("G", "E", "R:E", "GxE", "Residuals")
	colnames(at2)[5:6] <- c("Pr(>F)", "LSD5")
	
	#############################################################################################
	## 3.3 - ANOVA for G random, E random, B random
	at3[1,4] <- at3[1,3]/at3[4,3]
	at3[1,5] <- pf(at3[1,4], at3[1,1], at3[4,1], lower.tail=F)
	at3[2,4] <- (at3[2,3]+at3[5,3])/(at3[3,3]+at3[4,3])
	gl1 <- (at3[2,3]+at3[5,3])^2/(at3[2,3]^2/at3[2,1]+at3[5,3]^2/at3[5,1])
	gl2 <- (at3[3,3]+at3[4,3])^2/(at3[3,3]^2/at3[3,1]+at3[4,3]^2/at3[4,1])
	at3[2,5] <- pf(at3[2,4], gl1, gl2, lower.tail=F)
	row.names(at3) <- c("G", "E", "R:E", "GxE", "Residuals")
	colnames(at3)[5] <- c("Pr(>F)")
	
	#############################################################################################
	## 4 - Variance Components
	cvm <- lmer(yobs ~ (1|geno) + (1|geno:env) + (1|env/rep))
	vars <- c(VarCorr(cvm)$geno[1], VarCorr(cvm)$env[1], VarCorr(cvm)$'rep:env'[1],
			VarCorr(cvm)$'geno:env'[1], attr(VarCorr(cvm), "sc")^2)
	comp.var <- data.frame(Variance = vars, Std.Dev = vars^.5)
	row.names(comp.var) <- c("G", "E", "R:E", "GxE", "Residual")
	h2 <- VarCorr(cvm)$geno[1]/(VarCorr(cvm)$geno[1] + VarCorr(cvm)$'geno:env'[1]/env.num +
				attr(VarCorr(cvm), "sc")^2/env.num/numrep)*100
	
	#############################################################################################
	## 5.1 - SVD
	dec <- svd(int.eff, nu = PC, nv = PC)
	if (PC > 1) D <- diag(dec$d[1:PC]) else D <- dec$d[1:PC]
	G <- dec$u %*% (D^.5)
	E <- dec$v %*% (D^(1-.5))
	if (PC > 1) {
		stability_geno <- cbind(stability_geno, G[,1], G[,2])
		stability_env <- cbind(stability_env, E[,1], E[,2])
	} else {
		stability_geno <- cbind(stability_geno, G[,1], NA)
		stability_env <- cbind(stability_env, E[,1], NA)
	}
	colnames(stability_env) <- c("Slope", "SE", "MSdev", "MSentry", "MSinteract", "PC1", "PC2")
	PCnum <- paste("PC", c(1:PC), sep = "")
	
	#############################################################################################
	## 5.2 - Contribution of PCs
	PC.cont <- dec$d[1:PC]^2/sum(dec$d[1:PC]^2)*100
	PC.acum <- cumsum(PC.cont)
	tablaPC <- data.frame(PC = PCnum, Cont = PC.cont, Cont_Acum = PC.acum)
	tablaPC <- tablaPC[1:PC,]
	dimnames(E) <- list(levels(env), PCnum)
	dimnames(G) <- list(levels(geno), PCnum)
	
	#############################################################################################
	## 5.3 - Significance PCs
	if (numrep>1){
		int.SS <- (t(as.vector(int.eff))%*%as.vector(int.eff))*numrep
		PC.SS <- (dec$d[1:PC]^2)*numrep
		PC.DF <- env.num + geno.num - 1 - 2*c(1:PC)
		MS <- PC.SS/PC.DF
		F <- MS/deviance(add.anova)*add.anova$df.residual
		probab <- pf(F, PC.DF, add.anova$df.residual, lower.tail=FALSE)
		percSS <- PC.SS/int.SS
		rowlab <- PCnum
		tablaPC <- cbind(tablaPC, PC.DF, PC.SS, MS, F, probab)
		colnames(tablaPC)[4:8] <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
	}
	
	#############################################################################################
	## 6 - Biplots
	png(filename = paste("temp/",traitn,"_biplot1.png",sep=""), width = 900, height = 900)
	plot(1, type = 'n', xlim = range(c(env.mean, geno.mean)), ylim = range(c(E[,1], G[,1])),
			main = paste("AMMI1 biplot:",traitn), xlab = "Mean Response", 				
			ylab = paste("PC1 (",format(PC.cont[1],digits=3),"%)"))
	points(env.mean, E[,1], col = "red", lwd = 1.5, pch=15)
	text(env.mean, E[,1], labels = row.names(E), adj = c(0.5, 0.5),
			col = "red", cex=tam, pos=1, offset=.2)
	points(geno.mean, G[,1], col = "blue", lwd = 1.5, pch=17)
	text(geno.mean, G[,1], labels = row.names(G), adj = c(0.5, 0.5),
			col = "blue", cex=tam, pos=1, offset=0.2)
	abline(h = 0, v = overall.mean, col="green", lty = 2)
	dev.off()
	if (PC > 1){
		png(filename = paste("temp/",traitn,"_biplot2.png",sep=""), width = 900, height = 900)
		plot(1, type = 'n', asp=1, xlim = range(c(E[,1], G[,1])), ylim = range(c(E[,2], G[,2])),
				main = paste("AMMI2 biplot:",traitn), xlab = paste("PC1 (",format(PC.cont[1],digits=3),"%)"),
				ylab = paste("PC2 (",format(PC.cont[2],digits=3),"%)"))
		points(E[,1], E[,2], col = "red", lwd = 2, pch=15)
		text(E[,1], E[,2], labels = row.names(E), adj = c(0.5, 0.5),
				col = "red", cex=tam, pos=1, offset=.3)
		points(G[,1], G[,2], col = "blue", lwd = 2, pch=17)
		text(G[,1], G[,2], labels = row.names(G), adj = c(0.5, 0.5),
				col = "blue", cex=tam, pos=1, offset=.3)
		abline(h = 0, v = 0, col="green", lty = 2)
		for (i in 1:env.num) lines(c(0,E[i,1]), c(0,E[i,2]), col="red", lty=3)
		dev.off()
	}
	
	#############################################################################################
	## 7 - Tai
	sgl <- int.eff
	for (i in 1:geno.num) sgl[i,] <- sgl[i,]*(env.mean - overall.mean)/(env.num-1)
	alpha <- apply(sgl, 1, sum)/(at[2,3]-at[3,3])*geno.num*numrep
	s2gl <- int.eff
	for (i in 1:geno.num) s2gl[i,] <- s2gl[i,]^2/(env.num-1)
	lambda <- (apply(s2gl, 1, sum)-alpha*apply(sgl, 1, sum))/at[5,3]*geno.num*numrep/(geno.num-1)
	lmax <- max(lambda)*1.1
	lx <- seq(0,lmax,lmax/100)
	tailim <- qt(.95,env.num-2)*((lx*(geno.num-1)*at[5,3]*at[2,3])/
				((at[2,3]-at[3,3])*((env.num-2)*at[2,3]-(qt(.95,env.num-2)*at[3,3]))))^.5
	amax <- max(c(abs(alpha), tailim))*1.6
	corden <- cbind(lambda, alpha, alpha)
	corden <- corden[order(lambda),]
	corden[,3] <- corden[,3]/abs(corden[,3])*amax
	corneg <- subset(corden, corden[,3]<0)
	corpos <- subset(corden, corden[,3]>0)
	mmm <- c(.98,.7,.77,.84,.91)
	for(i in 1:length(corneg[,3]))
		corneg[i,3] <- corneg[i,3]*mmm[i%%5+1]
	for(i in 1:length(corpos[,3]))
		corpos[i,3] <- corpos[i,3]*mmm[i%%5+1]
	png(filename = paste("temp/",traitn,"_tai.png",sep=""), width = 600, height = 600)
	plot(1, type = 'n', xlim = c(0, lmax), ylim = c(-amax, amax),
			main = paste("Tai stability analysis:",traitn), xlab = expression(lambda),
			ylab = expression(alpha))
	points(lambda, alpha, col = "blue", lwd = 2, pch=4)
	text(corneg[,1], corneg[,3], labels = row.names(corneg), adj = c(0.5, 0.5), col = "blue",
			cex=tam, pos=1, offset=.3)
	text(corpos[,1], corpos[,3], labels = row.names(corpos), adj = c(0.5, 0.5), col = "blue",
			cex=tam, pos=3, offset=.3)
	points(lx, tailim, type="l", lty = 1)
	points(lx, -tailim, type="l", lty = 1)
	abline(v = qf(.025, env.num-2, env.num*(geno.num)*(numrep-1)), lty = 1)
	abline(v = qf(.975, env.num-2, env.num*(geno.num)*(numrep-1)), lty = 1)
	for (i in 1:length(corpos[,1]))
		lines(c(corpos[i,1],corpos[i,1]), c(corpos[i,2],corpos[i,3]), lty=2, col="red")
	for (i in 1:length(corneg[,1]))
		lines(c(corneg[i,1],corneg[i,1]), c(corneg[i,2],corneg[i,3]), lty=2, col="red")	
	dev.off()	
	stability_geno <- cbind(stability_geno, alpha, lambda)
	colnames(stability_geno) <- c("Slope", "SE", "MSdev", "MSentry", "MSinteract", "PC1", "PC2",
			"alpha", "lambda")
	
	#############################################################################################
	## 8 - Print results
	list(Geno_num = geno.num, Env_num = env.num, Overall_stat = over.stat,
			B_test = btest,
			B_test.pv = btest$p.value,
			SW_test = ntest,
			SW_test.pv = ntest$p.value,
			Geno_means = geno.mean, Env_means = env.mean, Inter_means = int.mean,
			Inter_eff = int.eff, Add_ANOVA1 = at1, CV = cv, Add_ANOVA2 = at2, Add_ANOVA3 = at3,
			Var_Comp = comp.var, Herit = h2, Mult_Inter = tablaPC, Stab_geno = stability_geno,
			Stab_env = stability_env, Geno_list = levels(geno), Env_list = levels(env))
}

#################################################################################################
##############################       End function analis       ##################################
#################################################################################################

MET = function(datos, traitn){

# Check all factors are factors
datos$rep <- factor(datos$rep)
datos$env <- factor(datos$env)
datos$geno <- factor(datos$geno)

# Check frequencies by Genotype and Environment
nmis <- dim(subset(datos, is.na(datos$y)==1))[1]	
subdatos <- subset(datos, datos$y!="NA")
tfreq <- table(subdatos$geno, subdatos$env)
control1 <- 1 # Check for zero. Initial state: no zeros
control2 <- 0 # Check for replicates. Initial state: only one replicate
control3 <- 1 # Check for balance. Initial state: balanced
for (i in 1:dim(tfreq)[1])
	for (j in 1:dim(tfreq)[2]){
		if (tfreq[i,j]==0) control1 <- 0 # State 0: there are zeros
		if (tfreq[i,j]>1) control2 <- 1 # State 1: more than one replicate
		if (tfreq[i,j]!=tfreq[1,1]) control3 <- 0 # State 0: unbalanced
	}

# Run analysis with zeros
if (control1==0){
	output <- NULL
	Error <- "Error: Some GxE cells have zero frequency. Remove genotypes or environments to proceed"
}

# Run analysis with one replicate
if (control1==1 & control2==0){
	output <- NULL
	Error <- "Error: There is just one replicate. Inference is not possible with one replicate."
}

# Run analysis with unbalanced data
# Missing values are estimated if they are less than 10%.
if (control1==1 & control2==1 & control3==0)
	if (mean(is.na(datos$y))<0.1){
		G <- nlevels(datos$geno)
		R <- max(tfreq)
		datos$yp <- datos$y
		datos$ytemp <- datos$y
		mGE <- tapply(datos$y, list(datos$env, datos$geno), mean, na.rm=T)
		for (i in 1:length(datos$y))
			if (is.na(datos$y[i]) == 1) datos$ytemp[i] <- mGE[datos$env[i],datos$geno[i]]
		liscon1 <- array(0, nmis)
		liscon2 <- array(0, nmis)
		cc <- max(datos$y, na.rm=T)
		cont <- 0
		while (cc > max(datos$y, na.rm=T)*1e-06){
			cont <- cont+1
			for (i in 1:length(datos$y))
				if (is.na(datos$y[i]) == 1){
					datos$ytemp[i] <- datos$y[i]
					suma1 <- tapply(datos$ytemp, list(datos$geno, datos$env), sum, na.rm=T)
					suma2 <- tapply(datos$ytemp, list(datos$env, datos$rep), sum, na.rm=T)
					suma3 <- tapply(datos$ytemp, datos$env, sum, na.rm=T)
					datos$yp[i] <- (G*suma1[datos$geno[i],datos$env[i]] +
								R*suma2[datos$env[i],datos$rep[i]] -
								suma3[datos$env[i]]) / (G*R - G - R +1)
					datos$ytemp[i] <- datos$yp[i]
				}
			liscon1 <- liscon2
			liscon2 <- subset(datos, is.na(datos$y)==1)$yp
			cc <- max(abs(liscon1-liscon2))
		}
		#TODO revise this: somewhere something wrong
		output <- analis(datos$geno, datos$env, datos$rep, datos$y, datos$yp, max(tfreq),
				nmis, control3, traitn=traitn)
		Error <- "Analysis OK with estimated missing values."
	} else {
		output <- NULL
		Error <- "Error: Too many missing values. Remove genotypes or environments to proceed."
	}

# Run analysis with balanced data and more than one replicate
if (control1==1 & control2==1 & control3==1){
	output <- analis(datos$geno, datos$env, datos$rep, datos$y, NULL, max(tfreq), 0, control3, traitn = traitn)
	Error <- "Analysis OK."
}

output
}


doMet = function(data, traits){
	res = list(length(traits))
	for(i in 1:length(traits)){
		datos = data
		names(datos)[1:3] = c("env","rep","geno")
		datos=cbind(datos[,c(1:3)],  data[,traits[i]])
		names(datos)[4]="y"
		#print(paste(traits[i],"\n\n"))
		try({res[traits[i]]=list(MET(datos, traits[i]))})
		#print(obs)
	}
	res
}






