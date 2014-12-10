library(bbmle)

#negative log likelihood function. monotonically increasing
#and maximizing the likelihood function is equivalent 
#in all ways to minimizing the negative log likelihood. 
binom = function(probability,results,numberOfTrials){
  -sum(dbinom(results,prob=probability,size=numberOfTrials,log=TRUE))
}

#test data. this is to test for a normal Bernoulli coin-toss
#trial style data-set. 
testData = 0:11

#for plotting
xVec = testData
yVec = dbinom(testData,prob=0.5,size=length(testData),log=FALSE); 

print(sum(dbinom(testData,prob=0.5,size=length(testData),log=TRUE)))

plot(xVec,yVec,xlab="X",ylab="Prob")

r = mle2(minuslogl = binom, 
     start = list(probability=.6), 
     data = list(numberOfTrials=length(testData), results = testData))

print(r)
