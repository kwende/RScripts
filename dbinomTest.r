#for plotting
xVec = 0:1
yVec = dbinom(xVec,prob=0.5,size=length(xVec),log=FALSE); 

plot(xVec,yVec,xlab="X",ylab="Prob")