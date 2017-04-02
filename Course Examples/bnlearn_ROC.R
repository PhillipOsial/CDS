#bnlearn and cross validation
#https://arxiv.org/pdf/0908.3817.pdf

library(bnlearn)
data(alarm)
bn.cv(alarm, bn = "hc", algorithm.args = list(score = "bde", iss = 1))

#In this case the bn argument specifies the label of a structure learning algorithm, 
#and algorithm.args is an optional list of arguments for the algorithm specified by bn.

bn.cv(alarm, bn = "hc", algorithm.args = list(score = "bde", iss = 10))

#score parameter
#k2              Cooper & Herskovits' K2
#bde             Bayesian Dirichlet (BDe)
#bds             Bayesian Dirichlet Sparse (BDs)
#mbde            Bayesian Dirichlet (interventional data)
#aic             AIC (disc.): Akaike Information Criterion
#bic             BIC (disc.) :  Bayesian information criterion
#loglik          Log-Likelihood (disc.)
#bge             Bayesian Gaussian (BGe)
#loglik-g        Log-Likelihood (Gauss.)
#aic-g           AIC (Gauss.)
#bic-g           BIC (Gauss.)
#loglik-cg       Log-Likelihood (cond. Gauss.)
#aic-cg          AIC (cond. Gauss.)
#bic-cg          BIC (cond. Gauss.)

#imaginary sample size (iss) for the Dirichlet posterior distribution of discrete networks

bn.cv(alarm, bn = "hc", algorithm.args = list(score = "bic"))

bn.cv(alarm, bn = "iamb")

#Typically, 10-fold cross-validation is performed 10 times for each learning strategy
#(golden standard originally introduced here), and the resulting sets of loss values 
#compared by plotting them as boxplots.


cv.bic = bn.cv(alarm, bn = "hc", runs = 10, algorithm.args = list(score = "bic"))

cv.bde = bn.cv(alarm, bn = "hc", runs = 10, algorithm.args = list(score = "bde", iss = 1))

plot(cv.bic, cv.bde, xlab = c("BIC", "BDe"))



#A different number of folds can be specified with the k argument.
bn.cv(alarm, bn = "iamb", k = 5) # Incremental Association
 

############################################################
#Custom folds in cross-validation
#The custom-folds method works in the same way as k-fold cross-validation, 
#but folds are specified manually with a folds argument.

#folds = list(1:5000, 5001:15000, 15001:20000)
#cv.custom = bn.cv(alarm, bn = "hc", method = "custom-folds", folds = folds)
#cv.custom

############################################################

#Hold-out cross-validation
#The syntax for hold-out cross-validation is very similar to that for k-fold cross-validation, but:
# -method must be set to "hold-out";
# -k denotes the number of times the sample will be split into training and test subsamples (the default is 10);
# -and optionally m denotes the number of observations to be sampled for the test subsample 
#(the default is 1/10th of the sample size).

bn.cv(alarm, bn = "hc", method = "hold-out", k = 25, m = 200,
      algorithm.args = list(score = "bde", iss = 10))

#MUST: http://www.cs.ru.nl/~marinav/Teaching/BDMinAI/assign210-11.pdf

install.packages("ROCR")
library(ROCR)

library(gplots)

data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)

## precision/recall curve (x-axis: recall, y-axis: precision)
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
perf1 <- performance(pred, "sens", "spec")
plot(perf1)

data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
perf.svm <- performance(pred.svm,  'tpr' ,  'fpr' )
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn,  'tpr' ,  'fpr' )
plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of
     HIV-1 coreceptor usage")
plot(perf.nn, lty=3, col="blue",add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue",
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.6,0.6,c( 'SVM' , 'NN' ),col=c( 'red' , 'blue' ),lwd=3)

#More: https://cran.r-project.org/web/packages/bnlearn/bnlearn.pdf