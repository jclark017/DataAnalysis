loansData <- read.csv("loansData.csv")

# Convert Numerics
loansData$Interest.Rate = gsub("%","",loansData$Interest.Rate)
loansData$Debt.To.Income.Ratio = gsub("%","",loansData$Debt.To.Income.Ratio)

loansData$Interest.Rate <- as.numeric(loansData$Interest.Rate)
loansData$Debt.To.Income.Ratio <- as.numeric(loansData$Debt.To.Income.Ratio)

# Standardize Loan.Length
# Make it Ordinal
loansData$Loan.Length <- ordered(loansData$Loan.Length)

# Standardize Employment.Length
# Make it Ordinal  http://www.statmethods.net/input/datatypes.html


# Standardize FICO.Range
#  Make it Ordinal
