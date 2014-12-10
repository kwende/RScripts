#negative log likelihood function. monotonically increasing
#and maximizing the likelihood function is equivalent 
#in all ways to minimizing the negative log likelihood. 
binom = function(probability,results,numberOfTrials){
  -sum(dbinom(results,prob=probability,size=numberOfTrials,log=TRUE))
}

#test data. this is to test for a normal Bernoulli coin-toss
#trial style data-set. 
