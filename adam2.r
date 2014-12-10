library(bbmle)

sigmoid = function(x,a,d,b,x0){
  ret = ((a-d)*(1+exp(-b*x0)))/(1+exp(b*(x-x0)))
  if(ret < 0){
    print(c("Sigmoid error: ",c(x,a,d,b,x0)))
  }
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
    val = -log10(prob^isFound * (1-prob))
    #if(is.nan(val)){
    #  print(c("ERROR: isFound:",isFound,", prob: ", prob))
    #}
    sum = sum + val
  }
  return(sum)
}

csv = read.csv(file="C:/Users/brush/Downloads/Threshold modelling.csv",head=TRUE,sep=",")

v = csv[,1]
d = csv[,12]

#print(probability(1,0,0,0,0,0,0,1))
#print(likelihood(v,d,0,0,0,0,0,0,1))

r = mle2(minuslogl = likelihood, 
         start = list(a1=0.1,b1=0.1,x01=0.1,d2=0.1,b2=0.1,x02=0.1,c=1), 
         data = list(values=v,data=d))