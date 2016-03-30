# 443 Assignment
# c)

num = 120 #Number of times process is repeated

# arch.sim: ARCH(1) function copied from slides
# n = number of ARCH(1) steps run
# omega = alpha 0 value for ARCH(1)
# alpha1 = alpha 1 value for ARCH(1)
# sigma = sigma value for ARCH(1)
arch.sim <- function(n, omega, alpha1, sigma)
  {
  out <- sqrt(omega)
  for(i in 2:n)
    {
    out[i] <- sqrt(omega + alpha1*out[i-1]^2)*rnorm(1, sd=sigma)
    }
  out
  }

# newARCH: Runs a new ARCH(1) given n, omega, and alpha1. We assume sigma = 1
# n = number of ARCH(1) steps run
# om = alpha 0 value for ARCH(1)
# alp = alpha 1 value for ARCH(1)
newARCH <- function(n,om,alp)
{
  sim.arch <- arch.sim(n=n, omega = om, alpha1 = alp, sigma = sqrt(1))
  return(sim.arch)
}

#AICmatrix: uses min/max inputs of p & q and creates numb matrices of AICs from the models used to fit 
#           ARCH(1) to ARMA(p,q). Finds the ARMA(p,q) model with the minimum AIC and uses that to predict
#           10 steps ahead using the predict function. Outputs list of matricies, list of (p,q) values with
#           the lowest AICs, forecasted values, and generated values.
# n = number of ARCH(1) steps run
# numb = number of times process is repeated
# pstart = minimum p used in ARMA models used to fit
# pfinish = maximum p used in ARMA models used to fit
# qstart = minimum q used in ARMA models used to fit
# qfinish = maximum q used in ARMA models used to fit
AICmatrix <- function(n,numb,pstart,pfinish,qstart,qfinish)
{
  output <- list(); model <- list(); mat <- list(); pqValues <- list(); forecast10 <- list()
  
  for (p in 1:numb)
  {
    # Generating simulated data from model 
    model[[p]] <- newARCH(n,0.3,0.65)
    mat[[p]] <- matrix(nrow = pfinish + 1,ncol = qfinish + 1)
    dimnames(mat[[p]]) <- list(c(pstart:pfinish),c(qstart:qfinish))
    
    for (i in pstart:pfinish)
    {
      for (l in qstart:qfinish)
      {
        # We pretend that we don't know p and q so we look at the matrix.
        k <- arima(model[[p]][1:(n-10)], order=c(i,0,l), include.mean = F, method="ML") 
        mat[[p]][i+1,l+1] <- k$aic
      }
    }
    
    # Getting output of min aic values and the corresponding matrices
    output[[p]] <- mat[[p]]
    pqValues[p] <- paste(as.character(c(which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)) - 1),collapse="")
    pval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[1] - 1
    qval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[2] - 1
    
    # Predicting model[[p]] for 10 steps ahead
    forecast10[p] <- predict(arima(model[[p]][1:(n-10)], order=c(pval,0,qval), method="ML"),n.ahead=10)
  }
  
  out <- list(output,pqValues,forecast10,model)
  return(out)
}

# Runs AICmatrix with 110 data points and with p & q values between 0-2
out <- AICmatrix(110,num,0,2,0,2)
matrices <- out[[1]] # matrices of AICs
pqVals <- unlist(out[2]) # pq values with lowest AICs
f10 <- out[[3]] # forecasted values
models <- out[[4]] # generated ARCH values
names(matrices) <- pqVals

s10 <- list()

# Calculates SEs for each prediction
for (i in 1:num)
{
  s10[[i]] <- (models[[i]][101:110] - f10[[i]])^2
}

sum10 <- c(0,0,0,0,0,0,0,0,0,0)

# Sums SEs at each step
for (i in 1:num)
{
  sum10 <- sum10 + s10[[i]]
}

# calculates MSE
mse10 <- sum10/num

# MSEs for steps 1,2,5,10 isolated
mylist <- mse10[c(1,2,5,10)]

# Output
mylist
table(pqVals)
