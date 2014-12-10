library(bbmle)

#negative log likelihood function. monotonically increasing
#and maximizing the likelihood function is equivalent 
#in all ways to minimizing the negative log likelihood. 
binom = function(probability,results,numberOfTrials){
  -sum(dbinom(results,prob=probability,size=numberOfTrials,log=TRUE))
}

#test data. this is to test for a normal Bernoulli coin-toss
#trial style data-set. 
testData = c(1,1,0,0,1,1,0,1,1,1,1,0,1)

#for plotting
xVec = 0:(length(testData)-1)
yVec = dbinom(testData,prob=.5,size=length(testData),log=FALSE); 

plot(xVec,yVec,xlab="X",ylab="Prob")

#r = mle2(minuslogl = binom, 
#     start = list(probability=.75), 
#     data = list(numberOfTrials=length(testData), results = testData))

#summary(r)
