par(mfrow=c(2,2))
#Strong Relationship for Loan Length
plot(loansData$Interest.Rate ~ jitter(as.numeric(loansData$Loan.Length)), xlab="Loan Length", ylab="Interest Rate (%)"
     , xaxt="n")
axis(1, at=unique(as.numeric(loansData$Loan.Length)), labels=unique(loansData$Loan.Length))
meanRateLL <- tapply(loansData$Interest.Rate,loansData$Loan.Length,mean)
points(1:2,meanRateLL,col="red",pch="-",cex=5)
lm1 <- lm(loansData$Interest.Rate ~ as.numeric(loansData$Loan.Length))
lines(loansData$Loan.Length,lm1$fitted, col="red", lwd=3)
mtext(text="(a)",side=3,line=1)

#strong relatioship with Credit Balance
plot(loansData$Interest.Rate ~ log(as.numeric(Revolving.CREDIT.Balance+1)), xlab="Revolving Credit Balance (Log10)", ylab="Interest Rate (%)"
     )
lm1 <- lm(loansData$Interest.Rate ~ log(as.numeric(Revolving.CREDIT.Balance+1)))
lines(log(as.numeric(loansData$Revolving.CREDIT.Balance[!is.na(Revolving.CREDIT.Balance)]+1)), lm1$fitted, col="red", lwd=3)
mtext(text="(b)",side=3,line=1)

#Strong Correlation with FICO.Range
plot(loansData$Interest.Rate ~ jitter(as.numeric(FICO.Range)), xlab="", ylab="Interest Rate (%)"
    )
axis(1, at=unique(as.numeric(FICO.Range)), labels=unique(FICO.Range), las=2)
mtext("FICO Score Range", 1, 4)
lm1 <- lm(loansData$Interest.Rate ~ as.numeric(FICO.Range))
lines(as.numeric(FICO.Range),lm1$fitted, col="red", lwd=3)
mtext(text="(c)",side=3,line=1)

#Amount Funded
plot(loansData$Interest.Rate ~ jitter(Amount.Funded.By.Investors), xlab="Amount Funded ($)", ylab="Interest Rate (%)")
lm1 <- lm(loansData$Interest.Rate ~ Amount.Funded.By.Investors)
lines(Amount.Funded.By.Investors, lm1$fitted, col="red", lwd=3)
mtext(text="(d)",side=3,line=1)
