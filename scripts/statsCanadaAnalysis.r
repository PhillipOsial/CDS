load("data/Heart Survey.RData")
filterData <- as.data.frame(x)
filterData <- filterData[, colSums(is.na(filterData)) == 0]
#filterData[["MSYS"]] = NULL

smp_size <- floor(0.80 * nrow(filterData))

set.seed(123)
train_ind <- sample(seq_len(nrow(filterData)), size = smp_size)

train <- filterData[train_ind, ]
test <- filterData[-train_ind, ]

#install.packages("forecast")

library(bnlearn)
library(gRim)
library(forecast)
#bn.gs <- gs(filterData);
#bn2 <- iamb(filterData);
#bn3 <- fast.iamb(filterData);
#bn4 <- inter.iamb(filterData);
bn.hc <- hc(filterData, score = "aic")

#highlight.opts <- list(nodes = c("hdap"), fill = "grey")
#graphviz.plot(bn.gs)
#graphviz.plot(bn2)
#graphviz.plot(bn3)
#graphviz.plot(bn4)
graphviz.plot(bn.hc)

#score(bn.gs, data = filterData, type = "bic-g")
score(bn.hc, data = filterData)

#query stuff here
testBn <- bn.fit(bn.hc, train)
cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = (SEX == "MALE"), n=1000000)

#pred = predict(bn.hc, "SEX", test)
#results_prob = data.frame(t(attributes(pred)$prob))

#ROC curve
install.packages("ROCR")
library(ROCR)

bn.hc

cv.bic = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bic"))

cv.bde = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bde", iss = 1))

plot(cv.bic, cv.bde, xlab = c("BIC", "BDe"))


