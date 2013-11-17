setwd("~/GitHub/DataAnalysis/DataAnalysisProject1/Data/Raw")
loansData <- read.csv("loansData.csv")

# Convert Numerics
loansData$Interest.Rate = gsub("%","",loansData$Interest.Rate)
loansData$Debt.To.Income.Ratio = gsub("%","",loansData$Debt.To.Income.Ratio)

loansData$Interest.Rate <- as.numeric(loansData$Interest.Rate)
loansData$Debt.To.Income.Ratio <- as.numeric(loansData$Debt.To.Income.Ratio)

# Standardize Loan.Length
# Make it Ordinal
loansData$Loan.Length <- ordered(loansData$Loan.Length)
attach(loansData)

# Standardize Employment.Length
# Make it Ordinal  http://www.statmethods.net/input/datatypes.html


# Standardize FICO.Range
#  Make it Ordinal

#Strong Relationship for Loan Length
plot(loansData$Interest.Rate ~ jitter(as.numeric(loansData$Loan.Length)))
meanRateLL <- tapply(loansData$Interest.Rate,loansData$Loan.Length,mean)
points(1:2,meanRateLL,col="red",pch="-",cex=5)

#strong relatioship with Credit Balance
plot(loansData$Interest.Rate ~ jitter(log(as.numeric(Revolving.CREDIT.Balance+1))))
lm1 <- lm(loansData$Interest.Rate ~ log(as.numeric(Revolving.CREDIT.Balance+1)))
lines(log(as.numeric(loansData$Revolving.CREDIT.Balance[!is.na(Revolving.CREDIT.Balance)]+1)), lm1$fitted, col="red", lwd=3)

#Strong Correlation with FICO.Range
plot(loansData$Interest.Rate ~ jitter(as.numeric(FICO.Range)))
lm1 <- lm(loansData$Interest.Rate ~ as.numeric(FICO.Range))
lines(as.numeric(FICO.Range),lm1$fitted, col="red", lwd=3)

#strong correlation with dabt to income ratio
plot(loansData$Interest.Rate ~ jitter(as.numeric(Debt.To.Income.Ratio)))
lm1 <- lm(loansData$Interest.Rate ~ as.numeric((Debt.To.Income.Ratio)))
lines(as.numeric(Debt.To.Income.Ratio),lm1$fitted, col="red", lwd=3)

# Stong Correlation with Loan Purpose
plot(loansData$Interest.Rate ~ jitter(as.numeric(Loan.Purpose)))
meanRateLP <- tapply(loansData$Interest.Rate,Loan.Purpose,mean)
points(1:14,meanRateLP,col="blue",pch="-",cex=5)
lm1 <- lm(loansData$Interest.Rate ~ as.factor(Loan.Purpose))
summary(lm1)
anova(lm1)

#Strong Correlation with Amount Funded

plot(loansData$Interest.Rate ~ jitter(Amount.Funded.By.Investors))
lm1 <- lm(loansData$Interest.Rate ~ Amount.Funded.By.Investors)
lines(Amount.Funded.By.Investors, lm1$fitted, col="red", lwd=3)

#Moderate Correlation with State
plot(loansData$Interest.Rate ~ jitter(as.numeric(State)))
meanRateSt <- tapply(loansData$Interest.Rate,State,mean)
points(1:46,meanRateSt,col="red",cex=5,pch="-")

#Moderate relationship with Open Credit Lines
plot(loansData$Interest.Rate ~ jitter(log(as.numeric(Open.CREDIT.Lines+1))))
lm1 <- lm(loansData$Interest.Rate ~ log(as.numeric(Open.CREDIT.Lines+1)))
lines(log(as.numeric(Open.CREDIT.Lines[!is.na(Open.CREDIT.Lines)]+1)), lm1$fitted, col="red", lwd=3)

#Moderate inverse Relationship with Monthly.Income
plot(loansData$Interest.Rate ~ jitter(log(as.numeric(Monthly.Income+1))))
lm1 <- lm(loansData$Interest.Rate ~ log(as.numeric(Monthly.Income+1)))
lines(log(as.numeric(Monthly.Income[!is.na(Monthly.Income)]+1)),lm1$fitted, col="red", lwd=3)

#Some relationship between Inquiries and interest rate
plot(loansData$Interest.Rate ~ jitter(as.numeric(Inquiries.in.the.Last.6.Months)))
meanRateInq <- tapply(loansData$Interest.Rate,Inquiries.in.the.Last.6.Months,mean)
points(0:9,meanRateInq,col="blue",pch="-",cex=5)

#No relationship with Home.Ownership
plot(loansData$Interest.Rate ~ jitter(as.numeric(Home.Ownership)))
meanRateHM <- tapply(loansData$Interest.Rate,as.numeric(Home.Ownership),mean)
points(1:5,meanRateHM,col="red",cex=5,pch="-")

#no relatioship in employment time
attach(loansData)
plot(loansData$Interest.Rate ~ jitter(as.numeric(Employment.Length)))
meanRateEmp <- tapply(loansData$Interest.Rate,Employment.Length,mean)
points(1:12,meanRateEmp,col="red",pch="-",cex=5)


