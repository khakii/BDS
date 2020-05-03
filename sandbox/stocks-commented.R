# 0. Introduction 
#### Libraries #################################################################
library(tidyr)
library(dplyr)

#### Figure 1.1: Monthly stock returns #########################################
# Daily stock prices
stocks <- read.csv("stocks.csv")

# RET is a factor.
stocks$RET <- as.numeric(as.character(stocks$RET))

# date is an int.
stocks$date <- as.Date(as.character(stocks$date), format="%Y%m%d")

# Remove rows with empty values
stocks <- stocks %>% filter(TICKER!="" & RET!="")

# Remove duplicates
dups <- which(duplicated(stocks[,c("TICKER","date")]))
stocks <- stocks[-dups,]

# Create month
stocks$month <- paste(format(stocks$date, "%Y-%m"),"-01",sep="")
stocks$month <- as.Date(stocks$month)

# Aggregate daily returns to monthly return
agg <- function(r) prod(1+r, na.rm=TRUE) - 1
mnthly <- stocks %>%
  group_by(TICKER, month) %>%
  summarize(RET = agg(RET), SNP = agg(sprtrn))

# Monthly returns of stocks
RET <- as.data.frame(mnthly[,-4]) %>% spread(TICKER, RET)

# Monthly returns of S&P500
# Since sprtrn is a column in stocks with duplicate entries for each stock,
# take only the first one for each month.
SNP <- as.data.frame(mnthly[,c("month","SNP")])
SNP <- SNP[match(unique(SNP$month),SNP$month),]

# Remove outlier: MPET (Magellan Petroleum)
RET <- RET %>% select(-MPET)

# Monthly return of US Treasury bills
tbills <- read.csv("tbills.csv")
tbills$date <- as.Date(tbills$date)

# Plot
par(mai=c(.6,.8,.1,.1))  # margin size in inches: bottom, left, top, right
dgrid <- as.Date(c("2010-01-01", "2011-01-01","2012-01-01",
                   "2013-01-01", "2014-01-01","2015-01-01",
                   "2016-01-01","2017-01-01"))
matplot(x=RET[,1],       # x-axis: date
        y=RET[,-1],      # y-axis: individual stock returns
        xlab="",         # x-axis label
        ylab="Return",   # y-axis label
        bty="n",         # box type: no box
        type="l",        # plot type: line plot
        lty=2,           # line type: dashed
        col=heat.colors(24),  # line colors, to be used cyclically
        xaxt="n",        # suppress default x-axis
        yaxt="n")        # suppress default y-axis
axis(1, at=as.numeric(dgrid), labels=2010:2017)  # custom x-axis
axis(2, at=c(-.5,0,.5))                          # custom y-axis
lines(tbills,            # line plot of T-bill returns
      lwd=2,             # line width
      col=8)             # line color
lines(SNP,               # line plot of S&P500 returns
      lwd=2)             # line width
legend(legend=c("S&P500", "T-bills"),  # legend text
       "top",            # position
       lwd=2,            # line width
       col = c(1, 8),    # line colors: black, gray
       bty="n")          # box type: no box

#### Figure 1.3: Big stocks ####################################################
# Market capitalizations of big stocks
bigs <- read.csv("bigstocks.csv", header=FALSE, as.is=TRUE)

# Excess returns of big stocks
exr <- (as.matrix(RET[,bigs[,1]]) - tbills[,2])

# Excess returns of the market
mkt <- (SNP[,2] - tbills[,2])

# Capital Asset Pricing Model (CAPM)
capm <- lm(exr ~ mkt)

# column 1: beta
# column 2: alpha
(ab <- t(coef(capm))[,2:1])

# Remove outlier: WMT (Walmart Inc)
ab <- ab[-9,]

# Plot
par(mai=c(.8,.8,0,0), xpd=FALSE)
plot(ab, type="n", bty="n", xlab="beta", ylab="alpha")
abline(v=1, lty=2, col=8)
abline(h=0, lty=2, col=8)
text(ab, labels=rownames(ab), cex=bigs[,2]/350, col="navy") 
