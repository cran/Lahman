# Create LahmanData.RData
# 
# Author: Michael Friendly
###############################################################################

# This is the script used to create the LahmanData data set in the package. 
# It only needs to be run again in case the main data tables in the Lahman data base
# are renamed or new ones are created.

require(vcdExtra)
library(Lahman)
LahmanData <- vcdExtra::datasets(package="Lahman")
nobs <- as.numeric(gsub('x[0-9]+', '', LahmanData$dim))
nvar <- as.numeric(gsub('[0-9]+x', '', LahmanData$dim))

LahmanData <- data.frame(LahmanData[,1:2], nobs, nvar)
colnames(LahmanData)[1] <- "file"
me <- which(LahmanData$file=="LahmanData")
LahmanData <- LahmanData[-me,]
# also remove the *Labels data sets
labeldata <- grep("Labels", LahmanData$file)
LahmanData <- LahmanData[-labeldata,]
str(LahmanData)

cd("C:/R/data/Lahman2013/data")

save(LahmanData, file="LahmanData.RData")

