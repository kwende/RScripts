library(bbmle)

sigmoid = function(x,a,d,b,x0){
  ret = ((a-d)*(1+exp(-b*x0)))/(1+exp(b*(x-x0))) + d
  return(ret)
}

probability = function(x,a1,b1,x01,d2,b2,x02,c){
  return(c * sigmoid(x,a1,1,b1,x01) * sigmoid(x,1,d2,b2,x02))
}

likelihood = function(values,data,a1,b1,x01,d2,b2,x02,c){
  sum = 0
  for(i in 1:length(values)){
    
    environ = data[i]; 
    isFound = values[i]
    prob = probability(environ,a1,b1,x01,d2,b2,x02,c);
    val = 0;
    if(prob!=0){
      val = -log10(prob^isFound * (1-prob))
    }
    sum = sum + val
  }
  return(sum)
}

csv = read.csv(file="thresholds.csv",head=TRUE,sep=",")

v = csv[,1]
d = csv[,12]

r = mle2(minuslogl = likelihood, 
         start = list(x01=0.1,x02=0.1, c=1, a1=.1, b1=.1, d2=.1, b2=.1), 
         data = list(values=v,data=d),
         lower = c(a1=0,b1=0,d2=0,b2=0),
         upper = c(a1=1,b1=1,d2=1,b2=1), 
         method="L-BFGS-B")

print(r)