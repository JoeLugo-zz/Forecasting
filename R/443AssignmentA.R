# 443 Assignment
# a)

num = 120 #Number of times process is repeated

newARMAa <- function(n,ar,ma)
{
  sim.arma <- arima.sim(n=n, list(ar=ar,ma=ma), sd = sqrt(1))
  return(sim.arma)
}

AICmatrixA <- function(n,numb,pstart,pfinish,qstart,qfinish)
{
  
  output <- list(); model <- list(); mat <- list(); pqValues <- list(); forecast10 <- list()
  
  for (p in 1:numb)
  {
    # Generating simulated data from model 
    model[[p]] <- newARMAa(n,c(0.5),c(1))
    mat[[p]] <- matrix(nrow = pfinish + 1,ncol = qfinish + 1)
    dimnames(mat[[p]]) <- list(c(pstart:pfinish),c(qstart:qfinish))
    
    for (i in pstart:pfinish)
    {
      for (l in qstart:qfinish)
      {
        # We pretend that we don't know p and q so we look at the matrix.
        k <- arima(model[[p]][1:(n-10)], order=c(i,0,l), include.mean = F,method="ML") 
        mat[[p]][i+1,l+1] <- k$aic
      }
    }
    
    # Getting output of min aic values and the corresponding matrices
    output[[p]] <- mat[[p]]
    pqValues[p] <- paste(as.character(c(which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)) - 1),collapse="")
    pval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[1] - 1
    qval <- which(mat[[p]] == min(mat[[p]]), arr.ind = TRUE)[2] - 1
    
    # Predicting model[[p]] for 10 steps ahead
    forecast10[p] <- predict(arima(model[[p]][1:(n-10)], order=c(pval,0,qval),method="ML"),n.ahead=10)
  }
  
  out <- list(output,pqValues,forecast10,model)
  return(out)
}

out <- AICmatrixA(110,num,0,2,0,2)
matrices <- out[[1]]
pqVals <- unlist(out[2])
f10 <- out[[3]]
models <- out[[4]]
names(matrices) <- pqVals

freq <- table(pqVals == "11")
proportion <- freq[[2]]/(freq[[1]] + freq[[2]])
s10 <- list()

for (i in 1:num)
{
  s10[[i]] <- (models[[i]][101:110] - f10[[i]])^2
}

sum10 <- c(0,0,0,0,0,0,0,0,0,0)

for (i in 1:num)
{
  sum10 <- sum10 + s10[[i]]
}

mse10 <- sum10/num
mylist <- mse10[c(1,2,5,10)]

mylist
table(pqVals)
proportion