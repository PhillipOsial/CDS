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

filterData$AREA <- as.character(filterData$AREA)
filterData$AREA[filterData$AREA=="VARIABLE UNKNOWN"] <- "vARIABLE_UNKNOWN"
filterData$AREA <- as.factor(filterData$AREA)

smp_size <- floor(0.80 * nrow(filterData))

set.seed(123)
train_ind <- sample(seq_len(nrow(filterData)), size = smp_size)

train <- filterData[train_ind, ]
test <- filterData[-train_ind, ]

library(bnlearn)

bn.hc <- hc(filterData, score = "aic")

graphviz.plot(bn.hc)

testBn <- bn.fit(bn.hc, train, method='bayes')

#cv.bic = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bic"))

#cv.bde = bn.cv(filterData, bn = "hc", runs = 10, algorithm.args = list(score = "bde", iss = 1))

#plot(cv.bic, cv.bde, xlab = c("BIC", "BDe"))

#get level values
testQuery = "GPAGE2 == ";
areaQuery = "AREA == ";

lv_AREA = strsplit(levels(filterData$AREA), " ")
lv_GPAGE2 = strsplit(levels(filterData$GPAGE2), " ")

for(x_area in 1:length(lv_AREA)){
  lv_combn_AREA = combn(lv_AREA, x_area)
  #col
  for(i in 1:dim(lv_combn_AREA)[2]){
    #row
    for(j in 1:dim(lv_combn_AREA)[1]){
      if(j < dim(lv_combn_AREA)[1]){
        AREA_Query <- paste(areaQuery, "\'", lv_combn_AREA[j, i], "\'", '| AREA == ', sep="")
      }else{
        AREA_Query <- paste(areaQuery, "\'",lv_combn_AREA[j, i], "\'", sep="")
      }
    }
    for(x_gpage2 in 1:length(lv_GPAGE2)){
      lv_combn_GPAGE2 = combn(lv_GPAGE2, x_gpage2)
      #col
      for(i in 1:dim(lv_combn_GPAGE2)[2]){
        #row
        for(j in 1:dim(lv_combn_GPAGE2)[1]){
          if(j < dim(lv_combn_GPAGE2)[1]){
            GPAGE2_Query <- paste(testQuery, "\'", lv_combn_GPAGE2[j, i], "\'", '| GPAGE2 == ', sep="")
          }else{
            GPAGE2_Query <- paste(testQuery, "\'",lv_combn_GPAGE2[j, i], "\'", sep="")
          }
        }
        tempScore <- cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = ((eval(parse(text=GPAGE2_Query))) & (eval(parse(text=AREA_Query))) & (BMICAT == ">=35" | BMICAT == "30-34.9") & (SYSCAT == ">=160" | SYSCAT == "150-159") & (DIASCAT== ">=100" | DIASCAT== "95-99" | DIASCAT== "90-94") & TCHOLCAT == ">=6.2 MMOL/L" & SALTFOOD == "OFTEN" ), n=1000000)
        print(tempScore)
        print(GPAGE2_Query)
        print(AREA_Query)
        GPAGE2_Query = "GPAGE2 == ";
      }
    }
    AREA_Query = "AREA == ";
  }
}

#tempScore <- cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = ((eval(parse(text=testQuery))) & (BMICAT == ">=35" | BMICAT == "30-34.9") & (SYSCAT == ">=160" | SYSCAT == "150-159") & (DIASCAT== ">=100" | DIASCAT== "95-99" | DIASCAT== "90-94") & TCHOLCAT == ">=6.2 MMOL/L" & SALTFOOD == "OFTEN" ), n=1000000)



#derp <- "GPAGE2 == '55-64' | GPAGE2 == '65-74'"


#cpquery(testBn, event = (EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"), evidence = TRUE, n=10000000)



table(cpdist(testBn, "BMICAT", EVERHA == "YES" | EVERSTR == "YES" | OTHHD == "YES"))















