# 443 Assignment
# b)

# Number of times process is repeated
num = 120 

newARMAb <- function(n,ar)
{
  sim.arma <- arima.sim(n=n, model=list(ar=ar), sd = sqrt(1))
  return(sim.arma)
}

AICmatrixB <- function(n,numb,pstart,pfinish,qstart,qfinish)
{
  
  output <- list(); simData <- list(); mat <- list(); pqValues <- list(); forecast10 <- list()
  
  for (p in 1:numb)
  {
    # Generating simulated data from model 
    simData[[p]] <- newARMAb(n,c(0.3))
    mat[[p]] <- matrix(nrow = pfinish + 1,ncol = qfinish + 1)
    dimnames(mat[[p]]) <- list(c(pstart:pfinish),c(qstart:qfinish))
    
    for (i in pstart:pfinish)
    {
      for (l in qstart:qfinish)
      {
        # We pretend that we don't know p and q so we look at the matrix.
        k <- arima(simData[[p]][1:(n-10)], order=c(i,0,l), include.mean = F,method="ML") 
        mat[[p]][i+1,l+1] <- k$aic
      }
    }
    
    # Getting output of min aic values and the corresponding matrices
    output[[p]] <- mat[[p]]
    pqValues[p] <- paste(as.character(c(which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)) - 1),collapse="")
    pval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[1] - 1
    qval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[2] - 1
    
    # Predicting simData[[p]] for 10 steps ahead
    forecast10[p] <- predict(arima(simData[[p]][1:(n-10)], order=c(pval,0,qval), method="ML"),n.ahead=10)
  }
  
  out <- list(output,pqValues,forecast10,simData)
  return(out)
}

# Running the function to get output
out <- AICmatrixB(30,num,0,1,0,1)
# Extracting Matrices with AIC values
matrices <- out[[1]]
# Extracting the p & q values for the model 
pqVals <- unlist(out[2])
# Extracting the forecasts 
f10 <- out[[3]]
# Extracting the simulated data
sData <- out[[4]]

# Renaming the matrices list with the corresponding p & q values
names(matrices) <- pqVals

# Creating table to see how many times it picked the correct p & q values
freq <- table(pqVals == "10")
# Getting the proportion 
proportion <- freq[[2]]/(freq[[1]] + freq[[2]])

# This part is getting the squarred differences between the forecasted and actual data
d10 <- list()

for (i in 1:num)
{
  d10[[i]] <- (sData[[i]][21:30] - f10[[i]])^2
}

# This part is adding up all the values
sum10 <- c(0,0,0,0,0,0,0,0,0,0)

for (i in 1:num)
{
  sum10 <- sum10 + d10[[i]]
}

# Dividing to get the MSE values
mse10 <- sum10/num

# Getting the MSE's for 1,2,5,10 steps ahead forecast
mylist <- mse10[c(1,2,5,10)]

mylist
table(pqVals)
proportion