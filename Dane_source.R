need<-c("httr","jsonlite","lubridate", "PerformanceAnalytics", "PortfolioAnalytics", "zoo", "plotly", "ROI.plugin.quadprog", "quadprog", "quantmod")
ins<-installed.packages()[,1]
(Get<-need[which(is.na(match(need,ins)))])
if(length(Get)>0){install.packages(Get)}
eval(parse(text=paste("library(",need,")")))
rm(Get, need, ins)

library(httr)
library(jsonlite)
library(lubridate)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(zoo)
library(plotly)
library(ROI.plugin.quadprog)
library(quadprog)
library(quantmod)

url <- "https://min-api.cryptocompare.com"
path1 <- "data/histoday?fsym="
ticker <- read.csv2("Tickery.csv")
ticker <- colnames(ticker)
path2 <- "&tsym=USD&allData=true"
keep <- "close"

for(x in 1:length(ticker))
{
  path_fin <- paste(path1, ticker[x], path2, sep="")
  raw.result <- GET(url = url, path = path_fin)
  this.raw.content <- rawToChar(raw.result$content)
  this.content <- fromJSON(this.raw.content)
  this.content[[4]] <- head(this.content[[4]], -1)
  this.content[[4]]$time <- as.POSIXct(this.content[[4]]$time, origin = "1970-01-01", tz = "GMT")
  this.content[[4]][this.content[[4]]$volumeto < 1000, 7] <- NA
  if(is.na(this.content[[4]][nrow(this.content[[4]]), 7]))
  {
    i <- 1
    while(is.na(this.content[[4]][nrow(this.content[[4]])-i, 7]))
      {
        i <- i+1
    }
    this.content[[4]][nrow(this.content[[4]]), 7] <- this.content[[4]][nrow(this.content[[4]])-i, 7]
  }
  this.content[[4]]$close <- na.locf(this.content[[4]]$close, fromLast = TRUE)
  write.csv(this.content[[4]], file = paste("Dane/", ticker[x], ".csv", sep=""), row.names=FALSE)
  this.content[[4]] <- as.xts(this.content[[4]], order.by=this.content[[4]]$time, dateFormat="POSIXct", frequency=NULL, RECLASS=FALSE)
  storage.mode(this.content[[4]]) <- "numeric"
  this.content[[4]] <- subset(this.content[[4]], select = keep)
  colnames(this.content[[4]]) <- ticker[x]
  assign(ticker[x], this.content[[4]])
  if(x==1)
    z <- this.content[[4]]
  else
    z <- merge.xts(z, this.content[[4]])
  Sys.sleep(1)
  rm(raw.result, this.raw.content, x, this.content)
}

index <- read.csv("Dane/cci30_OHLC.csv")
index$Date <- as.POSIXct(index$Date, origin = "1970-01-01", tz = "GMT")
index <- as.xts(index, order.by=index$Date, dateFormat="POSIXct", frequency=NULL, RECLASS=FALSE)
keep <- ("Close")
index <- subset(index, select = keep)
colnames(index) <- "index"
z <- merge.xts(z, index)

returns.data <- CalculateReturns(z)
returns.data <- tail(returns.data, -1)
covMat <- cov(returns.data, use = "complete.obs")
keep <- "index"
index.returns <- dailyReturn(index, subset = '2016::')
variance.index <- var(index.returns)
variance.index <- as.vector(variance.index)
beta <- covMat/variance.index
beta <- subset(beta, select = keep)
beta <- head(beta, -1)
risk_free <- 1.03^(1/365) -1
storage.mode(index) <- "numeric"
average.returns <- mean(index.returns)
CAPM <- risk_free + beta*(average.returns - risk_free)

covMat <- covMat[,-c(ncol(covMat))]
covMat <- head(covMat, -1)

Amat <- matrix (1, length(ticker))
bvec <- 1
Amat <- cbind(1, diag(length(ticker)))
bvec <- c(bvec, rep(0, length(ticker)))
Amat <- cbind(Amat, -diag(length(ticker)))
bvec <- c(bvec, rep(-0.2, length(ticker)))
zeros <- array(0, dim = c(length(ticker),1))
weights <- solve.QP(covMat, zeros, Amat = Amat, bvec=bvec, meq = 1)
weights <- as.matrix(round(weights[["solution"]], 4))
row.names(weights) <- ticker
returns <- weights * CAPM
stdDv.Data <- StdDev(returns.data)
stdDv.Data <- stdDv.Data[-length(stdDv.Data)]
weights.std <- stdDv.Data * weights
corelation.data <- cor(returns.data, use = "complete.obs")
corelation.data <- corelation.data[,-c(ncol(corelation.data))]
corelation.data <- head(corelation.data, -1)
weights.cor <-  t(as.matrix(weights.std)) %*% corelation.data
risk <- weights.cor %*% weights.std

standard_deviation <- as.matrix(stdDv.Data)
row.names(standard_deviation) <- ticker 
write.csv(round(beta, 2), file = "beta.csv")
write.csv(round(corelation.data, 2), file = "correlation.csv")
write.csv(round(CAPM, 4), file = "capm.csv")
write.csv(round(stdDv.Data, 4), file = "standard_deviation.csv")
write.csv(round(weights, 4), file = "wagi.csv")
write.csv(round(standard_deviation, 4), file = "standard_deviation.csv")

print("ryzyko portfela")
risk
print("zwrot portfela")
sum(returns)
print("wagi")
weights