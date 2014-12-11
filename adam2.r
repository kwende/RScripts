library(bbmle)

sigmoid = function(x,a,d,b,x0){
  if(a < 0) a = 0; 
  if(a > 1) a = 1; 
  if(d < 0) d = 0; 
  if(d > 1) d = 1; 
  if(b < 0) b = 0; 
  
  ret = ((a-d)*(1+exp(-b*x0)))/(1+exp(b*(x-x0))) + d

  if(ret < 0){
    print(c(a,d,b,x0));
    ret = 0; 
  }
  if(ret > 1){
    print(c(a,d,b,x0));
    ret = 1; 
  }
  
  return(ret)
}

PM3 = function(x,a1,b1,x01,d2,b2,x02,c){
  if(x01 >= x02) return(0)
  
  ret = c * sigmoid(x,a1,1,b1,x01) * sigmoid(x,1,d2,b2,x02); 
  return(ret)
}

likelihood = function(values,data,a1,b1,x01,d2,b2,x02,c){
  
  sum = 0
  for(i in 1:length(values)){
    
    environ = data[i]; 
    isFound = values[i]
    
    prob = PM3(environ,a1,b1,x01,d2,b2,x02,c);
    val = 0;
    
    if(prob>0 && prob<1){
      val = -log10(prob^isFound * (1-prob)^(1-isFound))
    }
    else{
      val = 10000000000
    }
    
    sum = sum + val
  }
  
  return(sum)
}

csv = read.csv(file="thresholds2.csv",head=TRUE,sep=",")

v = csv[,1]
d = csv[,2]

leftHandInflectionSlope = .3 #b1
rightHandInfectionSlope = .3 #b2
leftHandAsymptote = 0 #a1
rightHandAsymptote = 0 #d2
leftHandInflectionPoint = 10 #x01
rightHandInflectionPoint = 50 #x02
peak = 1

leftHandInflectionMin = 1
leftHandInflectionMax = 25
rightHandInflectionMin = 26
rightHandInflectionMax = 75

r = mle2(minuslogl = likelihood, 
        start = list(x01=leftHandInflectionPoint,
                     x02=rightHandInflectionPoint, 
                     a1=leftHandAsymptote, 
                     b1=leftHandInflectionSlope, 
                     d2=rightHandAsymptote, 
                     b2=rightHandInfectionSlope, 
                     c=peak), 
        data = list(values=v,data=d),
        lower = c(x01=leftHandInflectionMin,x02=rightHandInflectionMin, a1=0, b1=0, d2=0, b2=0, c=0),
        upper = c(x01=leftHandInflectionMax, x02=rightHandInflectionMax, a1=1, b1=1, d2=1, b2=1, c=1),
        method="L-BFGS-B")

print(r)

x = 0:100
y = 0:100
for(i in 0:101){
  
  a1 = r@coef["a1"]
  b1 = r@coef["b1"]
  x01 = r@coef["x01"]
  d2 = r@coef["d2"]
  b2 = r@coef["b2"]
  x02 = r@coef["x02"]
  c = r@coef["c"]  
  
  #a1 = .1
  #b1 = .3
  #x01 = 20
  #d2 = .3
  #b2 = .3
  #x02 = 50
  #c = 1
  
  y[i] = PM3(i, a1, b1, x01, d2, b2, x02, c)
}

plot(x, y, xlab="X",ylab="Prob", type="o")