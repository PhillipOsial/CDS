load("data/Heart Survey.RData")
filterData <- as.data.frame(x)
filterData <- filterData[, colSums(is.na(filterData)) == 0]
filterData[["PROV"]] = NULL
filterData[["HGTC"]] = NULL
filterData[["WGTC"]] = NULL
filterData[["ALCEVER"]] = NULL
filterData[["ALC12MTH"]] = NULL
filterData[["DIABET"]] = NULL
filterData[["WGTNOW"]] = NULL
filterData[["WGTLOSE"]] =NULL
filterData[["EDUCYRS"]] = NULL
filterData[["DIABCAT"]] = NULL
filterData[["SEDENT"]] = NULL
filterData[["WGT"]] = NULL
filterData[["SALTCOOK"]] = NULL
filterData[["BPRXNOW"]] = NULL
filterData[["HDL"]] = NULL
filterData[["EXER"]] = NULL
filterData[["ALCOHOL"]] = NULL

smp_size <- floor(0.80 * nrow(filterData))

set.seed(123)
train_ind <- sample(seq_len(nrow(filterData)), size = smp_size)

train <- filterData[train_ind, ]
test <- filterData[-train_ind, ]

library(bnlearn)

bn.hc <- hc(filterData, score = "aic")

graphviz.plot(bn.hc)

testBn <- bn.fit(bn.hc, train, method='bayes')

cv.bic = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bic"))

cv.bde = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bde", iss = 1))

plot(cv.bic, cv.bde, xlab = c("BIC", "BDe"))

cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = (( GPAGE2 == "55-64" | GPAGE2 == "65-74") & (BMICAT == ">=35" | BMICAT == "30-34.9") & 
                                          (SYSCAT == ">=160" | SYSCAT == "150-159") &
                                          (DIASCAT== ">=100" | DIASCAT== "95-99" | DIASCAT== "90-94") & TCHOLCAT == ">=6.2 MMOL/L" & SALTFOOD == "OFTEN" ), n=10000000)

cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = TRUE, n=10000000)



table(cpdist(testBn, "BMICAT", EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"))















