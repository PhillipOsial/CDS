heart_data <- read.table("data/heart.dat", col.names = c("age", "sex", "cpt", "rbp", "sc", "fbs", "rest ecg", "max hr", "eia", "oldpeak", "sop", "num mv", "thal", "hdap"))

heart_data_factor <- as.data.frame(matrix(factor(), ncol = 8, nrow=270))
colnames(heart_data_factor) <- c("sex", "cpt", "fbs", "rest.ecg", "eia", "sop", "thal", "hdap")
heart_data_factor$sex <- factor(heart_data$sex)
heart_data_factor$cpt <- factor(heart_data$cpt)
heart_data_factor$fbs <- factor(heart_data$fbs)
heart_data_factor$rest.ecg <- factor(heart_data$rest.ecg)
heart_data_factor$eia <- factor(heart_data$eia)
heart_data_factor$sop <- factor(heart_data$sop)
heart_data_factor$thal <- factor(heart_data$thal)
heart_data_factor$hdap <- factor(heart_data$hdap)

library(gRim)
msat <- dmod( ~.^., data=heart_data_factor )
mnew1 <- stepwise( msat, details=1, k=2 ) # use aic
plot( mnew1 )

mnew2 <- stepwise( msat, details=1, k=log(nrow(heart_data_factor)) ) # use bic
plot( mnew2 )

#From graph and data to network
#Create graphs from models
ug1 <- ugList( terms( mnew1 ) )
ug2 <- ugList( terms( mnew2 ) )
par(mfrow=c(1,2)); plot( ug1 ); plot( ug2 )

bn.gs <- gs(heart_data_factor); bn.gs;
bn2 <- iamb(heart_data_factor); bn2;
bn3 <- fast.iamb(heart_data_factor); bn3;
bn4 <- inter.iamb(heart_data_factor); bn4;
bn.hc <- hc(heart_data_factor, score = "aic"); 
bn.hc;

compare(bn.gs, bn2);
compare(bn.gs, bn3);
compare(bn.gs, bn4);
compare(bn.hc, bn.gs);

graphviz.plot(bn.gs);
graphviz.plot(bn2);
graphviz.plot(bn3);
graphviz.plot(bn4);
highlight.opts <- list(nodes = c("hdap", "sex"), arcs = c("sex", "hdap"), col = "red", fill = "grey");
graphviz.plot(bn.hc, highlight = highlight.opts);


#Create Bayesian networks from (graph, data):
bn1 <- compile( grain( ug1, data=heart_data_factor, smooth=0.1 )); 
plot(bn1)

bn2 <- compile( grain( ug2, data=heart_data_factor, smooth=0.1 )); 
plot(bn2)

#querygrain( bn1, "sex")

#z<-setEvidence( bn1, nodes=c("AngPec", "Hypertrophi"), c("Typical","Yes"))
# alternative form
highest_prob <- 0.0
lowest_prob <- 1.0
for(loop_sex in levels(heart_data_factor$sex)){
  for(loop_cpt in levels(heart_data_factor$cpt)){
    for(loop_fbs in levels(heart_data_factor$fbs)){
      for(loop_rest.ecg in levels(heart_data_factor$rest.ecg)){
        for(loop_sop in levels(heart_data_factor$sop)){
          for(loop_eia in levels(heart_data_factor$eia)){
            for(loop_thal in levels(heart_data_factor$thal)){
              z<-setEvidence( bn1, evidence=list(sex=loop_sex,cpt=loop_cpt,fbs=loop_fbs,rest.ecg=loop_rest.ecg,sop=loop_sop,eia=loop_eia,thal=loop_thal))
              curr_prob <- querygrain( z, "hdap")
              curr_prob <- curr_prob$hdap[[2]]
              if(curr_prob > highest_prob){
                highest_prob <- curr_prob
                worst_case = list()
                worst_case[["sex"]] <- loop_sex
                worst_case[["cpt"]] <- loop_cpt
                worst_case[["fbs"]] <- loop_fbs
                worst_case[["rest.ecg"]] <- loop_rest.ecg
                worst_case[["sop"]] <- loop_sop
                worst_case[["eia"]] <- loop_eia
                worst_case[["thal"]] <- loop_thal
              }
              if(curr_prob < lowest_prob){
                lowest_prob <- curr_prob
                best_case = list()
                best_case[["sex"]] <- loop_sex
                best_case[["cpt"]] <- loop_cpt
                best_case[["fbs"]] <- loop_fbs
                best_case[["rest.ecg"]] <- loop_rest.ecg
                best_case[["sop"]] <- loop_sop
                best_case[["eia"]] <- loop_eia
                best_case[["thal"]] <- loop_thal
              }
            }
          }
        }
      }
    }
  }
}