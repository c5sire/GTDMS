
#' Response to Selection plot with single location
#' 
#' @param N plot capacity
#' @param sg1 number of selected genotypes
#' @param sigmaG2 genotypic variance
#' @param sigmaE2 error variance
#' @return plot
#' @author Raul Eyzaguirre
#' @export
responseSelectionSingleExp <-function(N, sg1, sigmaG2, sigmaE2){
  r <- seq(1,floor(N/sg1),1)
  g <- floor(N/r)
  alpha <- sg1/g
  z <- dnorm(qnorm(1-alpha))
  i <- z/alpha
  rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaE2/r))
  R <- i*rho
  plot(r, R, xlab="Number of replications", ylab="Response to selection", type="b", 
       xlim=range(r), ylim=range(R))
  points(r[match(max(R),R)], max(R), col = "red", pch=18)
  mtext(paste("Optimum number of replications = ", r[match(max(R),R)]), line=2.9)
  mtext(paste("Number of genotypes at optimum = ", g[match(max(R),R)]), line=1.7)
  mtext(paste("Response to selection at optimum = ", round(max(R),2)), line=0.5)
  
}

#' Response to Selection plot with several locations
#' 
#' @param N plot capacity
#' @param sg1 number of selected genotypes
#' @param sigmaG2 genotypic variance
#' @param sigmaE2 error variance
#' @param k number of locations
#' @param sigmaGl2 location variance
#' @return plot
#' @author Raul Eyzaguirre
#' @export
responseSelectionSeveralExp <- function(N, sg1, sigmaG2, sigmaE2, k, sigmaGl2){
  r <- seq(1,floor(N/sg1/k),1)
  g <- floor(N/k/r)
  alpha <- sg1/g
  z <- dnorm(qnorm(1-alpha))
  i <- z/alpha
  rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaGl2/k + sigmaE2/k/r))
  R <- i*rho
  plot(r, R, xlab="Number of replications", ylab="Response to selection", type="b", 
       xlim=range(r), ylim=range(R))
  points(r[match(max(R),R)], max(R), col = "red", pch=18)
  mtext(paste("Optimum number of replications = ", r[match(max(R),R)]), line=2.9)
  mtext(paste("Number of genotypes at optimum = ", g[match(max(R),R)]), line=1.7)
  mtext(paste("Response to selection at optimum = ", round(max(R),2)), line=0.5) 
}

#' Response to Selection plot with several locations and years
#' 
#' @param N plot capacity
#' @param sg1 number of selected genotypes
#' @param sigmaG2 genotypic variance
#' @param sigmaE2 error variance
#' @param k number of locations
#' @param sigmaGl2 location variance
#' @param q number of years
#' @param sigmaGy2 year variance
#' @param sigmaGly2 location x year variance
#' @return plot
#' @author Raul Eyzaguirre
#' @export
responseSelectionSeveralExpYears <-function(N, sg1, sigmaG2, sigmaE2, k, sigmaGl2,
                                            q, sigmaGy2, sigmaGly2){
  r <- seq(1,floor(N/sg1/k/q),1)
  g <- floor(N/k/q/r)
  alpha <- sg1/g
  z <- dnorm(qnorm(1-alpha))
  i <- z/alpha
  rho <- sqrt(sigmaG2 / (sigmaG2 + sigmaGl2/k + sigmaGy2/q + 
                                   sigmaGly2/k/q + sigmaE2/k/q/r))
  R <- i*rho
  plot(r, R, xlab="Number of replications", ylab="Response to selection", type="b",
       xlim=range(r), ylim=range(R))
  points(r[match(max(R),R)], max(R), col = "red", pch=18)
  mtext(paste("Optimum number of replications = ", r[match(max(R),R)]), line=2.9)
  mtext(paste("Number of genotypes at optimum = ", g[match(max(R),R)]), line=1.7)
  mtext(paste("Response to selection at optimum = ", round(max(R),2)), line=0.5)
  
}

#' Response to Selection plot with two-stage selection
#' 
#' @param g number of genotypes before stage 1
#' @param k1 stage 1: number of locations
#' @param r1 stage 1: number of replicatons
#' @param sg1 stage 1: number of selected genotypes
#' @param k2 stage 2: number of locations
#' @param r2 stage 2: number of replicatons
#' @param sg2 stage 2: number of selected genotypes
#' @param sigmaG2 genotypic variance
#' @param sigmaGL2 location variance
#' @param sigmaGY2 year variance
#' @param sigmaGLY2 location x year variance
#' @param sigmaE2 error variance
#' @return data.frame
#' @author Raul Eyzaguirre
#' @export
responseSelection2stage <- function(g, 
                                    k1, r1, sg1, 
                                    k2, r2, sg2, 
                                    sigmaG2, 
                                    sigmaGL2, sigmaGY2, 
                                    sigmaGLY2, 
                                    sigmaE2){
      out = paste(
      g,
      k1,
      r1,
      sg1,
      k1,
      r2, 
      sg2,
      sigmaG2,
      sigmaGL2,
      sigmaGY2,
      simgaGLY2,
      sigmaE2
      )
      
  
#out = "Invalid combination of parameters."
try({

responseSelection2stage <- function(             g = 100, 
                                    k1=1, r1=1, sg1=50, 
                                    k2=1, r2=1, sg2=10, 
                                    sigmaG2=.1, 
                                    sigmaGL2=.1, sigmaGY2=.1, 
                                    sigmaGLY2=.1, 
                                    sigmaE2=.1){
#try({
  # first stage
  alpha1 <- sg1/g
  x1 <- qnorm(1-alpha1)
  z1 <- dnorm(x1)
  i1 <- z1/alpha1
  rho1 <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2/k1 + sigmaGY2 + sigmaGLY2/k1 + sigmaE2/k1/r1))
  R1 <- i1*rho1
  # second stage
  alpha2 <- sg2/sg1
  rho2 <- sqrt(sigmaG2 / (sigmaG2 + sigmaGL2/k2 + sigmaGY2 + sigmaGLY2/k2 + sigmaE2/k2/r2))
  # both togheter
  alpha <- alpha1*alpha2
  rho <- rho1*rho2
  int <- function(x){
    (2*pi)^(-.5)*exp(-x^2/2) * pnorm((x1-rho*x)/(sqrt(1-rho^2)), lower.tail=FALSE)
  }
  f <- function(t){
    integrate(int, t, Inf)$value - alpha
  }
  x2 <- uniroot(f, c(0,20))$root
  z2 <- dnorm(x2) 
  a <- (x1 - rho*x2)/sqrt(1-rho^2)
  b <- (x2 - rho*x1)/sqrt(1-rho^2)
  I1 <- 1 - pnorm(a)
  I2 <- 1 - pnorm(b)
  R2 <- (rho1*z1*I2 + rho2*z2*I1)/alpha
  salida <- data.frame(row.names=c("1st stage =", "2nd stage ="))
  salida$x <- c(x1,x2)
  salida$Ru <- c(R1,R2)
  out = salida
}, silent=T)  
out
#  out = salida
#}, silent=T)  
salida
}




