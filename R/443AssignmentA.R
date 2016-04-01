# 443 Assignment
# a)

# Number of times process is repeated
num = 120 

# newARMAa: Runs a new ARMA(p,q) given n, ar, and ma. 
# n = number of ARMA(p,q) steps run
# ar = vector with coefficients for the AR portion
# ma = vector with coefficients for the MA portion
newARMAa <- function(n,ar,ma)
{
  sim.arma <- arima.sim(n=n, list(ar=ar,ma=ma), sd = sqrt(1))
  return(sim.arma)
}

#AICmatrixA: uses min/max inputs of p & q and creates numb matrices of AICs from the models used to fit 
#           ARMA(p,q). Finds the ARMA(p,q) model with the minimum AIC and uses that to predict
#           10 steps ahead using the predict function. Outputs list of matricies, list of (p,q) values with
#           the lowest AICs, forecasted values, and generated values.
# n = number of ARMA(p,q) steps run
# numb = number of times process is repeated
# pstart = minimum p used in ARMA(p,q) models used to fit
# pfinish = maximum p used in ARMA(p,q) models used to fit
# qstart = minimum q used in ARMA(p,q) models used to fit
# qfinish = maximum q used in ARMA(p,q) models used to fit
AICmatrixA <- function(n,numb,pstart,pfinish,qstart,qfinish)
{
  
  output <- list(); simData <- list(); mat <- list(); pqValues <- list(); forecast10 <- list()
  
  for (p in 1:numb)
  {
    # Generating simulated data from model 
    simData[[p]] <- newARMAa(n,c(0.5),c(1))
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
    forecast10[p] <- predict(arima(simData[[p]][1:(n-10)], order=c(pval,0,qval),method="ML"),n.ahead=10)
  }
  
  out <- list(output,pqValues,forecast10,simData)
  return(out)
}

# Running the function to get output
out <- AICmatrixA(110,num,0,2,0,2)
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
freq <- table(pqVals == "11")
# Getting the proportion 
proportion <- freq[[2]]/(freq[[1]] + freq[[2]])

# This part is getting the squarred differences between the forecasted and actual data
d10 <- list()

for (i in 1:num)
{
  d10[[i]] <- (sData[[i]][101:110] - f10[[i]])^2
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

# plot
# Makes the pq plots 
pqVals.df <- as.data.frame(table(pqVals))
pqPlot11 <- ggplot(data=pqVals.df, aes(x=pqVals, y=Freq)) + geom_bar(stat="identity")
pqPlot11 <- pqPlot11 + labs(title = "Suggested pq Values from ARMA(1,1) Data",x="pq Values",y="Frequency")

# Makes the MSE plots
mseNames <- c(1,2,5,10)
mse.df <- data.frame(mseNames,mylist)
msePlot11 <- ggplot(data = mse.df, aes(x=factor(mseNames), y=mylist)) + geom_bar(stat="identity")
msePlot11 <- msePlot11 + labs(title = "MSE Values from ARMA(1,1) Data",x="Steps Ahead",y="MSE")
