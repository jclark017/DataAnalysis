setwd("~/GitHub/DataAnalysis/DataAnalysisProject2")
load("samsungData.rda")
sd <- samsungData

#fix bad column names
oldnames <- names(samsungData) # for the write up
sd <- data.frame(sd)
sd$activity <- as.factor(sd$activity)

sdtrain <- subset(sd, !(subject %in% c(27,28,29,30)))
sdtrain <- sdtrain[,-562] #removes the subject variable
sdtest <- subset(sd,subject %in% c(27,28,29,30))
sdtest <- sdtest[,-562] #removes the subject variable

sdtree <- tree(as.factor(activity) ~ ., data = sdtrain)

plot(sdtree)
text(sdtree)

par(mfrow=c(1,2))
plot(cv.tree(sdtree,FUN=prune.tree))
plot(cv.tree(sdtree))

prunetree <- prune.tree(sdtree,best=6)
plot(prunetree)
text(prunetree)

par(mfrow=c(1,2))
plot(cv.tree(sdtree, method="misclass"))
plot(cv.tree(prunetree, method="misclass"))

cv.tree(sdtree, method="misclass")
cv.tree(prunetree, method="misclass")

par(mfrow=c(1,2))
plot(sdtree)
text(sdtree)
plot(prunetree)
text(prunetree)

table(sdtest$activity,predict(sdtree,sdtest,type="class"))

table(sdtest$activity,predict(prunetree,sdtest,type="class"))

summary(sdtree)
summary(prunetree)

#Final figure
cv.sdtree <- cv.tree(sdtree, method="misclass")
cv.prunetree <- cv.tree(prunetree, method="misclass")

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
par(mgp=c(3,.5,0))
plot(prunetree)
title(main="Pruned Classification Tree")
text(prunetree)
plot(cv.sdtree,main="Misclassification (Unpruned)")

plot(cv.prunetree,main="Misclassification (Pruned)")


