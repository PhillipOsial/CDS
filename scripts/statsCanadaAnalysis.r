load("data/Heart Survey.RData")
filterData <- as.data.frame(x)
filterData <- filterData[, colSums(is.na(filterData)) == 0]
#filterData[["MSYS"]] = NULL


smp_size <- floor(0.80 * nrow(filterData))

set.seed(123)
train_ind <- sample(seq_len(nrow(filterData)), size = smp_size)

train <- filterData[train_ind, ]
test <- filterData[-train_ind, ]

library(bnlearn)
library(gRim)
library(forecast)
library(caret)

bn.gs <- gs(filterData);
bn2 <- iamb(filterData);
bn3 <- fast.iamb(filterData);
bn4 <- inter.iamb(filterData);
bn.hc <- hc(filterData, score = "aic")

#highlight.opts <- list(nodes = c("hdap"), fill = "grey")
graphviz.plot(bn.gs)
graphviz.plot(bn2)
graphviz.plot(bn3)
graphviz.plot(bn4)
graphviz.plot(bn.hc)

score(bn.gs, data = filterData, type = "bic-g")
score(bn.hc, data = filterData)

#query stuff here
testBn <- bn.fit(bn.hc, train, method='bayes')
cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = (SEX == "MALE" & GPAGE2 == "65-74" & 
    BPHIGH == "YES" & HYPER == "HYPERTENSIVE" & BMICAT == ">=35" & EXER == "YES" & SYSCAT == ">=160"), n=100000000)

table(cpdist(testBn, "EVERHA",SEX == "MALE"))
#ROC curve
library(ROCR)

bn.hc

cv.bic = bn.cv(filterData, bn = "hc", runs = 1, algorithm.args = list(score = "bic"))

cv.bde = bn.cv(filterData, bn = "hc", runs = 1, algorithm.args = list(score = "bde", iss = 1))

plot(cv.bic, cv.bde, xlab = c("BIC", "BDe"))


