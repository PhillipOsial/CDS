#Simple BN example. Feb 8, 2017
library(gRain)

yn <- c("yes","no");
x <- c(5,95,1,99)

t.a <- array(x, dim=c(2,2), dimnames=list(tub=yn,asia=yn))

# Alternative specification: parray() from gRbase
t.a <- parray(c("tub","asia"), levels=list(yn,yn), values=x)

# with a formula interface
t.a <- parray(~tub:asia, levels=list(yn,yn), values=x)

# Alternative (partial) specification
t.a <- cptable(~tub | asia, values=c(5,95,1,99), levels=yn)



#Specify chest clinic network. Can be done in many ways; one is from a list of CPTs

yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99), levels=yn)
t.a <- cptable(~tub | asia, values=c(5,95,1,99), levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung | smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc | smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either | lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray | either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp | bronc:either, values=c(9,1,7,3,8,2,1,9),levels=yn)

cpt.list <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
cpt.list

#let's see what we have in CPTs
cpt.list$asia
cpt.list$tub
ftable(cpt.list$either, row.vars=1) # Notice: logical variable


# Create network from CPT list:
bnet <- grain(cpt.list)


# Compile network (details follow)
bnet <- compile(bnet)
bnet


# Query network to find marginal probabilities of diseases
querygrain(bnet, nodes=c("tub","lung","bronc"))


# Set evidence and query network again
bnet.ev<-setEvidence(bnet, nodes = c("asia","dysp"),states = c("yes","yes"))
querygrain(bnet.ev, nodes=c("tub","lung","bronc"))

# Set additional evidence and query again
bnet.ev<-setEvidence(bnet.ev, nodes = "xray", states = "yes")
querygrain(bnet.ev, nodes=c("tub","lung","bronc"))

# Probability of observing the evidence (the normalizing constant)
pEvidence(bnet.ev)

# Get joint dist of tub, lung, bronc given evidence
x<-querygrain(bnet.ev, nodes=c("tub","lung","bronc"), type="joint")
ftable(x, row.vars=1)

# Get distribution of tub given lung, bronc and evidence
x<-querygrain(bnet.ev, nodes=c("tub","lung","bronc"),
              type="conditional")
ftable(x, row.vars=3)


# Remove evidence
bnet.ev<-retractEvidence(bnet.ev, nodes="asia")
bnet.ev




############################################################################################
#Building CPTs from data:
## Example: Simulated data from chest network
data(chestSim1000, package="gRbase")
head(chestSim1000)

## Extract empirical distributions
s <- xtabs(~smoke, chestSim1000); 
s

b.s <- xtabs(~bronc+smoke, chestSim1000); 
b.s

d.b <- xtabs(~dysp+bronc, chestSim1000); 
d.b

## Normalize to CPTs if desired (not necessary because
## we can always normalize at the end)
s <- as.parray(s, normalize="first"); 
s

b.s <- as.parray(b.s, normalize="first"); 
b.s
d.b <- as.parray(d.b, normalize="first"); 
d.b
cpt.list <- compileCPT(list(s, b.s, d.b)); 
cpt.list

net <- grain( cpt.list ); 
plot(net)
plot(dag(~bronc + smoke|bronc + dysp|bronc))


#But we could just as well extract CPTs for this model,
#in the sense that the joint distribution will become the same.
## Extract empirical distributions

b <- xtabs(~bronc, chestSim1000);
b

s.b <- xtabs(~smoke+bronc, chestSim1000);

d.b <- xtabs(~dysp+bronc, chestSim1000);
plot(dag( ~smoke + dysp + bronc|smoke:dysp ))


#Extracting clique marginals
#We consider the undirected graph
plot(ug( ~smoke:bronc+bronc:dysp ))

#corresponds to p(s, b, d) = q1(s, b)q2(s, b)
q1.sb <- xtabs(~smoke+bronc, data=chestSim1000); 
q1.sb

q2.db <- xtabs(~bronc+dysp, data=chestSim1000); 
q2.db


####Learning the model structure
#The next step is to learn the structure of association between the
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











