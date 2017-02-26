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


library(gRain)
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



