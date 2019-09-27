
library(Rblpapi)
library(TTR)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

# define Bloomberg tickers

tickers <- c(".DSTRAT Index", "TFCFR1E LX Equity", "MLAQSEI ID Equity",
             "TBWLSIE ID Equity", "BLWEERE ID Equity",
             "SXXR Index", "SPTR500N Index", "SPTRNE Index", "SPXUXEN Index",
             "EECBDEPO Index", "FDFD Index")


# define study periods
periods <- c(as.Date("2018-12-31"), as.Date("2018-02-27"), as.Date("2016-08-09"))


# connect to Bloomberg and download datas
opt <- c("periodicitySelection"="DAILY",
         "nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
         "nonTradingDayFillMethod"="PREVIOUS_VALUE")

con <- blpConnect()

prices <- bdh(tickers, "PX_LAST", min(periods), options = opt)

blpDisconnect(con)


# tidy data to xts and carry froward missing values
prices <- lapply(prices, function(x) xts(x[,2], x[,1]))
prices <- do.call(merge, prices)
prices <- na.locf(prices)

# rename columns
colnames(prices) <- c("SPhEur", "EuOn", "UsOn", "Dharma", "Q-Cap", "Quest", 
                      "Trium", "BlackLong", "Stoxx", "SP", "SPEur")

# calc Interest rates Indexes
days= c(0, diff(.indexDate(prices)))

prices$UsOnIdx <- 1 + (lag(prices$UsOn, 1) * days / 36500)
prices$EuOnIdx <-  1 + (lag(prices$EuOn, 1) * days / 36500)

prices$UsOnIdx[1] <- 1
prices$EuOnIdx[1] <- 1

prices$UsOnIdx <- cumprod(prices$UsOnIdx)
prices$EuOnIdx <- cumprod(prices$EuOnIdx)

# reorder columns
prices <- prices[, c(4:11, 1, 12, 13, 2 ,3 )]

# calc returns ad carry forward
returns <- ROC(prices[,1:11])
returns[1,] <- 0
returns <- na.locf(returns)

# plot rates indexes
plot(exp(cumsum(returns$EuOnIdx))-1)
plot(exp(cumsum(returns$UsOnIdx))-1)


# test results
ActiveReturn(returns[paste(periods[1],"/"), ], 
             returns[paste(periods[1],"/"), "Stoxx"], 
             scale=252)

AdjustedSharpeRatio(returns[paste(periods[1],"/"), "Dharma"], 
                    prices[paste(periods[1],"/"), "EuOn"] / 36500)

AppraisalRatio(returns[paste(periods[1],"/"),1], 
               returns[paste(periods[1],"/"),6], 
               mean(prices[paste(periods[1],"/"), "EuOn"] / 36500), 
               method="modified") # "appraisal", "alternative"

AverageDrawdown(returns[paste(periods[1],"/")])


AverageLength(returns[paste(periods[1],"/")])
AverageRecovery(returns[paste(periods[1],"/")])

BernardoLedoitRatio(returns[paste(periods[1],"/")])

BetaCoVariance(returns[paste(periods[1],"/")], returns[paste(periods[1],"/")])
BetaCoSkewness(returns[paste(periods[1],"/")], returns[paste(periods[1],"/")])
BetaCoKurtosis(returns[paste(periods[1],"/")], returns[paste(periods[1],"/")])


BurkeRatio(returns[paste(periods[1],"/")],
           mean(prices [paste(periods[1],"/"), "EuOn"]),
           modified=FALSE) # TRUE


CalmarRatio(returns[paste(periods[1],"/")], scale=252)
SterlingRatio(returns[paste(periods[1],"/")], scale=252, excess=0.1)

# CAPM functions a bit disregarder now
CAPM.alpha(returns[paste(periods[1],"/")], 
           returns[paste(periods[1],"/"),6], 
           (prices[paste(periods[1],"/"), "EuOn"] / 36500))

CAPM.beta(returns[paste(periods[1],"/")], 
          returns[paste(periods[1],"/"),6], 
          prices[paste(periods[1],"/"), "EuOn"] / 36500)

CAPM.CML.slope(returns[paste(periods[1],"/"),6],
               prices[paste(periods[1],"/"), "EuOn"] / 36500)

CAPM.SML.slope(returns[paste(periods[1],"/"), 6],
               prices[paste(periods[1],"/"), "EuOn"] / 36500)


CAPM.CML(returns[paste(periods[1],"/")],
          returns[paste(periods[1],"/"),6],
          prices[paste(periods[1],"/"), "EuOn"] / 36500)


CAPM.RiskPremium(returns[paste(periods[1],"/")],
         prices[paste(periods[1],"/"), "EuOn"] / 36500)

CAPM.dynamic(returns[paste(periods[1],"/"), 1:3], 
             returns[paste(periods[1],"/"),6], 
             prices[paste(periods[1],"/"), "EuOn"] / 36500,
             returns[paste(periods[1],"/"), 4:5]) # fake Z facrot (public info not directly linked, ie US10y CPI etc...)

CAPM.epsilon(returns[paste(periods[1],"/")], 
          returns[paste(periods[1],"/"),6], 
          mean(prices[paste(periods[1],"/"), "EuOn"] / 36500))


CAPM.jensenAlpha(returns[paste(periods[1],"/")], 
             returns[paste(periods[1],"/"),6], 
             mean(prices[paste(periods[1],"/"), "EuOn"] / 36500))


CDD(returns, weight= NULL, p= 0.95)

chart.ACF(returns[paste(periods[1],"/"), "Q.Cap"])
chart.ACFplus(returns[paste(periods[1],"/"), "Dharma"])

chart.Bar(returns[paste(periods[1],"/"), "Dharma"])
charts.Bar(returns[paste(periods[1],"/"), 1:9], main="Returns")

chart.BarVaR(returns[paste(periods[1],"/"), "Dharma"])
chart.BarVaR(returns[paste(periods[1],"/"), "Dharma"],
             methods="HistoricalVaR")
chart.BarVaR(returns[paste(periods[1],"/"), 1:3],
             methods= "GaussianVaR",
             all=TRUE, lty=, lwd=2, colorset=c("red", rep("gray",2)))
chart.BarVaR(returns[paste(periods[1],"/"), 1],
             methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"))

chart.BarVaR(returns[paste(periods[1],"/"), 1],
             methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),
             lwd=2, ypad=.01)
chart.BarVaR(returns[paste(periods[1],"/"), 1],
             methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),
             lwd=2, ypad=.01, clean="boudt")
chart.BarVaR(returns[paste(periods[1],"/"), 1],
             methods="ModifiedVaR",
             lwd=2, ypad=.01, clean="boudt", show.horizontal = TRUE, lty=2)

chart.Boxplot(returns[paste(periods[1],"/"),])
chart.Boxplot(returns[paste(periods[1],"/"),],
              as.Tufte = TRUE)

chart.CaptureRatios(returns[paste(periods[1],"/"), 1:9], 
             returns[paste(periods[1],"/"),6])
             

chart.Correlation(returns[paste(periods[1],"/"), 1:6],
                  histogram=TRUE)

chart.CumReturns(returns[paste(periods[2],"/"), 1:9],
                 legend.loc="topleft")

chart.Drawdown(returns[paste(periods[1],"/"), 1:9],
                 legend.loc="bottomleft")


chart.ECDF(returns[paste(periods[1],"/"), "Q.Cap"])

n <- table.Drawdowns(returns[paste(periods[1],"/"), 1])
chart.Events(Drawdowns(returns[paste(periods[1],"/"), 1]),
             dates= n$Trough,
             prior=max(na.omit(n$`To Trough`)),
             post=max(na.omit(n$Recovery)),
             lwd=2, xaxis=TRUE,  legend.loc="bottomleft")


chart.Histogram(returns[paste(periods[1],"/"), "Dharma"],
                breaks=40,
                methods= c("add.density", "add.rug",
                            "add.centered",
                           "add.risk", "add.qqplot"),
                show.outliers = TRUE)

# QQplot
chart.QQPlot(returns[paste(periods[1],"/"), "Dharma"],
             line=c("quartiles"),
             distribution= "norm")

library(MASS)
x <- checkData(returns[paste(periods[1],"/"), "Dharma"], na.rm=TRUE, method="vector")
fit= fitdistr(1+x, "lognormal")
chart.QQPlot(1+x, main="log-normal distrivbution", 
             envelope=0.95,
             distribution="lnorm", 
             distributionParameter = 'meanlog = fit$estimate[[1]], sdlog= fit$estimate[[2]]')

library(nor1mix)
obj= norMixEM(x, m=2)
chart.QQPlot(x, main="normal mixture distribution",
             envelope=0.95,
             line=c("quartiles"),
             distribution="norMix", 
             distributionParameter = 'obj')


library(sn)
n <- length(x)
fit.tSN <- st.mple(as.matrix(rep(1,n)), x, symmetr=TRUE)
names(fit.tSN$dp) <- c("location", "scale", "dof")
round(fit.tSN$dp, 3)
chart.QQPlot(x, main="MO Symmetric t-Distribution",
             xlab="qunatilesSymmetricTdistEst",
             envelope=0.95,
             line=c("quartiles"),
             distribution="t", 
             distributionParameter = 'df=fit.tSN$dp[3]',
             pch=20)

fit.st <- st.mple(as.matrix(rep(1,n)), x)
names(fit.st$dp) <- c("location", "scale", "skew", "dof")
round(fit.st$dp, 3)
chart.QQPlot(x, main="MO Returns Skewed t-Distribution",
             xlab="qunatilesSkewedTdistEst",
             envelope=0.95,
             line=c("quartiles"),
             distribution="st", 
             distributionParameter = 'xi = fit.st$dp[1], 
                                omega= fit.st$dp[2],
                                alpha= fit.st$dp[3],
                                nu= fit.st$dp[4]',
             pch=20)


chart.Regression(returns[paste(periods[1],"/"), 1:5],
         returns[paste(periods[1],"/"),6],
         prices[paste(periods[1],"/"), "EuOn"] / 36500,
         excess.return=TRUE,
         fit= c("loess", "linear"),
         legend.loc= "bottomright")

chart.RelativePerformance(returns[paste(periods[1],"/"), 1:5],
                 returns[paste(periods[1],"/"),6],
                 colorset=rich8equal,
                 legend.loc="bottomleft",
                 main="Relative Performance to SXXP")

chart.RiskReturnScatter(returns[paste(periods[1],"/"), 1:5],
                        mean(prices[paste(periods[1],"/"), "EuOn"] / 36500),
                        add.boxplots= FALSE)

chart.RollingCorrelation(returns[paste(periods[2],"/"), 1:5],
                          returns[paste(periods[2],"/"),6],
                         width=100,
                         legend.loc="bottomright")

chart.RollingMean(returns[paste(periods[1],"/"), 1],
                         width=100,
                         legend.loc="bottomright")

chart.RollingPerformance(returns[paste(periods[1],"/"), 1:6],
                  width=30,
                  legend.loc="bottomleft")

chart.RollingRegression(returns[paste(periods[1],"/"), 1:6],
                        returns[paste(periods[1],"/"),6],
                         width=30,
                         legend.loc="bottomleft")


chart.RollingQuantileRegression(returns[paste(periods[1],"/"), 1:5],
                        returns[paste(periods[1],"/"),6],
                        width=30,
                        legend.loc="bottomleft")

chart.Scatter(returns[paste(periods[1],"/"), 1],
              returns[paste(periods[1],"/"), 6])

chart.SnailTrail(returns[paste(periods[1],"/"), 1:3],
                 rf=prices[paste(periods[1],"/"), "EuOn"] / 36500,
                 width=20, stepsize=5,
                 add.names="firstandlast")

chart.StackedBar(returns[paste(periods[1],"/"), 1:3])

chart.TimeSeries(exp(cumsum(returns[paste(periods[1],"/"), 1])))

  
chart.VaRSensitivity(returns[paste(periods[1],"/"), 1])    

charts.PerformanceSummary(returns[paste(periods[1],"/"), 1:3],
                          prices[paste(periods[1],"/"), "EuOn"] / 36500,
                          methods="StdDev") 
                 
charts.PerformanceSummary(returns[paste(periods[1],"/"), 1:9])
                 
charts.RollingPerformance(returns[paste(periods[1],"/"), 1:3],
                          prices[paste(periods[1],"/"), "EuOn"] / 36500,
                          width=100,
                          legend.loc="topleft")

length(returns[paste(periods[3],"/"), "Dharma"])
cb <- clean.boudt(returns[paste(periods[3],"/"), "Dharma"], alpha=0.01, trim=0.001)
length(cb[[1]])

CoSkewnessMatrix(returns[paste(periods[3],"/"), 1:3])
CoKurtosisMatrix(returns[paste(periods[3],"/"), 1:3])
CoVariance(returns[paste(periods[3],"/"), 1:5],
           returns[paste(periods[3],"/"), 6])
CoSkewness(returns[paste(periods[3],"/"), 1:5],
           returns[paste(periods[3],"/"), 6])
CoKurtosis(returns[paste(periods[3],"/"), 1:5],
           returns[paste(periods[3],"/"), 6])

M3.MM(returns[paste(periods[3],"/"), 1:3], as.mat=FALSE, unbias=TRUE)
M4.MM(returns[paste(periods[3],"/"), 1:3], as.mat=FALSE)


DownsideDeviation(returns[paste(periods[3],"/"), 1:5],
                  MAR=0.0)
DownsidePotential(returns[paste(periods[3],"/"), 1:5],
                  MAR=0.0)
SemiDeviation(returns[paste(periods[3],"/"), 1:5])
SemiVariance(returns[paste(periods[3],"/"), 1:5])

DownsideFrequency(returns[paste(periods[3],"/"), 1:5], MAR=0)

DRatio(returns[paste(periods[3],"/"), 1:5])

DrawdownDeviation(returns[paste(periods[3],"/"), 1:5])

Drawdowns(returns[paste(periods[3],"/"), 1])
          
findDrawdowns(returns[paste(periods[3],"/"), 1])
sortDrawdowns(findDrawdowns(returns[paste(periods[3],"/"), 1]))

ETL(returns[paste(periods[3],"/"), 1:6],
    p=0.099,
    method = "historical")

sigma <- M2.ewma(returns[paste(periods[3],"/"), 1:5], lambda = 0.94)
m3    <- M3.ewma(returns[paste(periods[3],"/"), 1:5], lambda = 0.94)
m4    <- M4.ewma(returns[paste(periods[3],"/"), 1:5], lambda = 0.94)
#♦compute equal weigthe portfolio
mu <- colMeans(returns[paste(periods[3],"/"), 1:5])
p  <- length(mu)
ES(p=0.95, portfolio_method = "component",
   weights= rep(1/p,p),
   mu= mu, sigma= sigma, m3=m3, m4=m4)

FamaBeta(returns[paste(periods[3],"/"), 1:5],
         returns[paste(periods[3],"/"), 6])

Frequency(returns[paste(periods[3],"/"), 1])
frequency(returns[paste(periods[3],"/"), 1])

HurstIndex(returns[paste(periods[3],"/"), ]) # 1 persistent, 0.5 random, 0 mean reverting

InformationRatio(returns[paste(periods[3],"/"), 1:5], 
                 returns[paste(periods[3],"/"), 6:7],
                 scale=252)
TrackingError(returns[paste(periods[3],"/"), 1:5], 
              returns[paste(periods[3],"/"), 6],
              scale=252)
ActivePremium(returns[paste(periods[3],"/"), 1:5], 
              returns[paste(periods[3],"/"), 6],
              scale=252)
SharpeRatio(returns[paste(periods[3],"/"), 1:5], 
            returns[paste(periods[3],"/"), 6],
            p=0.95)

summary((exp(returns[paste(periods[1],"/"), 1:5])-1) * 100)[,-1]

Kappa(returns[paste(periods[1],"/"), 1:5],
      MAR=0, l=2)


