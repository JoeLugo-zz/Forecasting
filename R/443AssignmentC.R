# 443 Assignment

numb = 100

arch.sim <- function(n, omega, alpha1, sigma)
  {
  out <- sqrt(omega)
  for(i in 2:n)
    {
    out[i] <- sqrt(omega + alpha1*out[i-1]^2)*rnorm(1, sd=sigma)
    }
  out
  }



newARCH <- function(n,om,alp)
{
  sim.arch <- arch.sim(n=n, omega = om, alpha1 = alp, sigma = sqrt(1))
  return(sim.arch)
}

AICmatrix <- function(n,pstart,pfinish,qstart,qfinish)
{
  
  output <- list(); model <- list(); mat <- list(); pqValues <- list(); forecast10 <- list()
  
  for (p in 1:numb)
  {
    # Generating simulated data from model 
    model[[p]] <- newARCH(n,1,1)
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
    forecast10[p] <- predict(arima(model[[p]][1:(n-10)], order=c(pval,0,qval)),n.ahead=10)
  }
  
  out <- list(output,pqValues,forecast10,model)
  return(out)
}

out <- AICmatrix(110,0,2,0,2)
matrices <- out[[1]]
pqVals <- unlist(out[2])
f10 <- out[[3]]
models <- out[[4]]
names(matrices) <- pqVals

s10 <- list()

for (i in 1:numb)
{
  s10[[i]] <- (models[[i]][101:110] - f10[[i]])^2
}

sum10 <- c(0,0,0,0,0,0,0,0,0,0)

for (i in 1:numb)
{
  sum10 <- sum10 + s10[[i]]
}

mse10 <- sum10/numb

