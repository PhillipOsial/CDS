load("data/Heart Survey.RData")
filterData <- as.data.frame(x)
filterData <- filterData[, colSums(is.na(filterData)) == 0]
#filterData[["MSYS"]] = NULL

library(gRim)
bn.gs <- gs(filterData);
bn2 <- iamb(filterData);
bn3 <- fast.iamb(filterData);
bn4 <- inter.iamb(filterData);
bn.hc <- hc(filterData, score = "aic")

library(bnlearn)
#highlight.opts <- list(nodes = c("hdap"), fill = "grey")
graphviz.plot(bn.gs)
graphviz.plot(bn2)
graphviz.plot(bn3)
graphviz.plot(bn4)
graphviz.plot(bn.hc)

#query stuff here
testBn <- bn.fit(bn.hc, data = filterData)
cpquery(testBn, event = (EVERHA == "YES"), evidence = (SMOKECAT == "REG CIGARETTE"), n=1000000)
