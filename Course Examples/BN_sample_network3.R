#####################################################################################
####Learning the model structure
#The next step is to learn the structure of association between the
#variables. By this we mean learn the conditional independencies among the
#variables from data.

#Characteristics of 409 lizards were recorded, namely species (S),
#perch diameter (D) and perch height (H).
data(lizardRAW, package="gRbase")
dim(lizardRAW)
head(lizardRAW, 4)

#It is common to organize such data in a contingency table
lizard<-xtabs(~., data=lizardRAW)
dim( lizard )
ftable( lizard ) #Flat Contingency Tables

#One estimate of the probabilities is by the relative frquencies:
lizardProb <- lizard/sum(lizard); 
ftable(lizardProb)

#tableMargin compute the sum of table entries for a given index (i.e. a marginal table)
tableMargin(lizard, ~height+species) 
tableMargin(lizardProb, ~height+species)


#Model search in log-linear models using gRim
#CAD is the diseae; the other variables are risk factors and disease manifestations/symptoms.
library(gRim)
data(cad1, package="gRbase")
args(stepwise.iModel)

#statistically significant as P < 0.05 and statistically highly significant as 
#P < 0.001 (less than one in a thousand chance of being wrong)
msat <- dmod( ~.^., data=cad1 )
mnew1 <- stepwise( msat, details=1, k=2 ) # use aic
plot( mnew1 )

mnew2 <- stepwise( msat, details=1, k=log(nrow(cad1)) ) # use bic
plot( mnew2 )


#From graph and data to network
#Create graphs from models
ug1 <- ugList( terms( mnew1 ) )
ug2 <- ugList( terms( mnew2 ) )
par(mfrow=c(1,2)); plot( ug1 ); plot( ug2 )

#Create Bayesian networks from (graph, data):
bn1 <- compile( grain( ug1, data=cad1, smooth=0.1 )); 
bn1

bn2 <- compile( grain( ug2, data=cad1, smooth=0.1 )); bn2

querygrain( bn1, "CAD")

z<-setEvidence( bn1, nodes=c("AngPec", "Hypertrophi"),
                c("Typical","Yes"))
# alternative form
z<-setEvidence( bn1,
                nslist=list(AngPec="Typical", Hypertrophi="Yes"))
querygrain( z, "CAD")




########################################################################
#bnlearn package
install.packages("bnlearn")
library(bnlearn)

data(learning.test)
str(learning.test)

#learning.test contains six discrete variables, stored as factors, 
#each with 2 (for F) or 3 (for A, B, C, D and E) levels. 
#The structure of the Bayesian network associated with this data set can be learned 
#for example with the Grow-Shrink algorithm, implemented in the gs function, 
#and stored in an object of class bn

bn.gs <- gs(learning.test);
bn.gs

bn2 <- iamb(learning.test)
bn2

#Other constraint-based algorithms return the same partially directed network structure 
#(again as an object of class bn), as can be readily seen with compare.
bn2 <- iamb(learning.test)
bn2
bn3 <- fast.iamb(learning.test); bn3
bn4 <- inter.iamb(learning.test); bn4

compare(bn.gs, bn2)
compare(bn.gs, bn3)
compare(bn.gs, bn4)

#On the other hand hill-climbing results in a completely directed network, which differs from
#the previous one because the arc between A and B is directed (A→B instead of A − B).

bn.hc <- hc(learning.test, score = "aic") #Akaike information criterion (AIC)
bn.hc

compare(bn.hc, bn.gs)

#Another way to compare the two network structures is to plot them side by side and highlight
#the differing arcs.
par(mfrow = c(1,2))
highlight.opts <- list(nodes = c("A", "B"), arcs = c("A", "B"), 
                       col = "red", fill = "grey")
graphviz.plot(bn.hc, highlight = highlight.opts)
graphviz.plot(bn.gs, highlight = highlight.opts)


#Debugging utilities and diagnostics
gs(learning.test, debug = TRUE)



#More information https://arxiv.org/pdf/0908.3817.pdf