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

PM2 = function(x,a,d,b,x0){
  
  ret = sigmoid(x,a,d,b,x0)
  return(ret);
}

PM3Likelihood = function(values,data,a1,b1,x01,d2,b2,x02,c){
  
  sum = 0
  for(i in 1:length(values)){
    
    environ = data[i]; 
    isFound = values[i]
    
    prob = PM3(environ,a1,b1,x01,d2,b2,x02,c);
    val = 0;
    
    if(prob>0 && prob<1){
      val = -log10(prob^isFound * (1-prob)^(1-isFound))
    }
    else if(prob <= 0){
      val = 10000000000
    }
    else if(prob >=1){
      val = -10000000000
    }
    
    sum = sum + val
  }
  
  return(sum)
}

PM2Likelihood = function(values,data,a,d,b,x0){
  sum = 0; 
  
  for(i in 1:length(values)){
    environ = data[i]; 
    isFound = values[i]; 
    
    prob = PM2(environ,a,d,b,x0); 
    val = 0;
    
    if(prob > 0 && prob < 1){
      val = -log10(prob^isFound * (1-prob)^(1-isFound));
    }
    else{
      val = 10000000000
    }
    
    sum = sum + val; 
  }
  
  return(sum); 
}

PM3MLE = function(v,d){
  leftHandInflectionSlope = .5 #b1
  rightHandInfectionSlope = .5 #b2
  leftHandAsymptote = 0 #a1
  rightHandAsymptote = 0 #d2
  leftHandInflectionPoint = 50 #x01
  rightHandInflectionPoint = 100 #x02
  peak = 1
  
  leftHandInflectionMin = 1
  leftHandInflectionMax = 75
  rightHandInflectionMin = 76
  rightHandInflectionMax = 150
  
  r = mle2(minuslogl = PM3Likelihood, 
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
  
  return(r);
}

PM2MLE = function(v,d){
  inflectionPointSlope = .3 #b
  leftHandAsymptote = 0 #a
  rightHandAsymptote = 0 #d
  inflectionPoint = 20 #x0
  
  inflectionPointMin = 1
  inflectionPointMax = 100
  
  r = mle2(minuslogl = PM2Likelihood, 
           start = list(x0=inflectionPoint,
                        a=leftHandAsymptote, 
                        b=inflectionPointSlope, 
                        d=rightHandAsymptote), 
           data = list(values=v,data=d),
           lower = c(x0=inflectionPointMin, a=0, b=0, d=0),
           upper = c(x0=inflectionPointMax, a=1, b=1, d=1),
           method="L-BFGS-B")
  
  return(r);  
}

csv = read.csv(file="thresholds.csv",head=TRUE,sep=",")

v = csv[,9]
d = csv[,12]
#v = csv[,1]
#d = csv[,2]

r = PM3MLE(v,d);
#r = PM2MLE(v,d);

print(r)

x = 0:which.max(d)
y = 0:which.max(d)
for(i in 0:length(y)){
  
  a1 = r@coef["a1"]
  b1 = r@coef["b1"]
  x01 = r@coef["x01"]
  d2 = r@coef["d2"]
  b2 = r@coef["b2"]
  x02 = r@coef["x02"]
  c = r@coef["c"]  
  
  #a = r@coef["a"]; 
  #b = r@coef["b"]; 
  #d = r@coef["d"];
  #x0 = r@coef["x0"];
  
  #a1 = .1
  #b1 = .3
  #x01 = 20
  #d2 = .3
  #b2 = .3
  #x02 = 50
  #c = 1
  
  #(x,a,d,b,x0)
  y[i] = PM3(i, a1, b1, x01, d2, b2, x02, c)
  #y[i] = PM2(i,a,d,b,x0)
}

plot(x, y, xlab="X",ylab="Prob", type="o")