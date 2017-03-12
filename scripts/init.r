heart_data <- read.table("data/heart.dat", col.names = c("age", "sex", "cpt", "rbp", "sc", "fbs", "rest ecg", "max hr", "eia", "oldpeak", "sop", "num mv", "thal", "hdap"))

heart_data$sex <- factor(heart_data$sex)
heart_data$cpt <- factor(heart_data$cpt)
heart_data$fbs <- factor(heart_data$fbs)
heart_data$rest.ecg <- factor(heart_data$rest.ecg)
heart_data$eia <- factor(heart_data$eia)
heart_data$sop <- factor(heart_data$sop)
heart_data$thal <- factor(heart_data$thal)
heart_data$hdap <- factor(heart_data$hdap)

library(bnlearn)

bn.hc <- hc(heart_data, score = "aic")
plot(bn.hc)

bn.gs <- gs(heart_data)
plot(bn.gs)

bn2 <- iamb(heart_data)
plot(bn2)
bn3 <- inter.iamb(heart_data)
plot(bn3)

compare(bn.gs, bn2)
compare(bn.gs, bn3)
compare(bn.gs, bn.hc)

library(graphwiz)

par(mfrow = c(1,2))
plot(bn.gs, main = "Constraint-based algorithms")
plot(res, main = "Hill-Climbing")

res <- hc(heart_data)
plot(res)

fittedbn <- bn.fit(res, data = heart_data)

age <- fittedbn$age
print(fittedbn$hdap)

# CLEAR CONSOLE COMMAND
# cat("\014") or CTRL+L

#summary(heart_data)

# library(evtree)
# shw <- array(1, nrow(heart_data))
# shw[heart_data$hdap == "2"] <- 5
# sht <- evtree(hdap ~ . , data = heart_data, weights = shw, control = evtree.control(maxdepth = 3))
# sht
# table(predict(sht), heart_data$hdap)
# plot(sht)

# num_of_tests <- 1
# max_prob <- 0.0
# min_prob <- 1.0
# myList <- list()
# for(i in 0:1){
#   for(j in 71:202){
#     curr_prob <- cpquery(fittedbn, event = (hdap == "2"), evidence = (eia == i & max.hr >= j))
#     if(curr_prob > max_prob){
#       max_prob <- curr_prob
#       myList[["eia"]] <- i
#       myList[["max.hr"]] <- j
#     } 
#     if(curr_prob < min_prob){
#       min_prob <- curr_prob
#     } 
#   }
# }
# print("Done")

# min_heart_rate <- min(heart_data$max.hr)
# max_heart_rate <- max(heart_data$max.hr)
# max_prob <- 0.0
# for(i in min_heart_rate:max_heart_rate){
#   curr_prob <- cpquery(fittedbn, event = (hdap == 2), evidence = (eia == 1 & max.hr <= i))
#   print(curr_prob)
#   if(curr_prob > max_prob){
#     max_prob <- curr_prob
#   }
# }
