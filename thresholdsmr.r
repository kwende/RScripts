
#########################################

csv = read.csv(file="thresholds.csv",head=TRUE,sep=",")
head(csv)

library(mgcv)

v = csv[,1]
d = csv[,12]
plot(d,v)
fm=glm(v~d+I(d^2),family=binomial)
dpredvals = 0:200
preds=predict(fm,data.frame(d=dpredvals ),type='response')
lines(dpredvals,preds,col='red')
fm=gam(v~s(d),family=binomial)
preds=predict(fm,data.frame(d=dpredvals ),type='response')
lines(dpredvals,preds,col='blue')

v = csv[,9]
d = csv[,12]
plot(d,v)
fm=glm(v~d+I(d^2),family=binomial)
dpredvals = 0:200
preds=predict(fm,data.frame(d=dpredvals ),type='response')
lines(dpredvals,preds,col='red')
fm=gam(v~s(d),family=binomial)
preds=predict(fm,data.frame(d=dpredvals ),type='response')
lines(dpredvals,preds,col='blue')

#################### or could try one model with all explanatory variables and interactions, 
#### and squared terms to allow for curvature ie optimality

## try for species 1 

v = csv[,1]
fm=glm(v~(HYDRPERI+NUMBINNU+CATCSIZE+MAXIDEPT+SEDIDEPT)^2+
         I(HYDRPERI^2)+I(NUMBINNU^2)+I(CATCSIZE^2)+I(MAXIDEPT^2)+I(SEDIDEPT^2),
       family=binomial,data=csv)
summary(fm)
n=length(csv$HYDRPERI)
fm1=step(fm,k=log(n))  ## then simplify fitted model based on BIC to remove non-significant terms
summary(fm1)
## looks like interaction between hydroperiod and catchment size is important, so look more at that:
preddf = expand.grid(HYDRPERI=1:220, CATCSIZE=c(0,15,30), NUMBINNU=mean(csv$NUMBINNU), 
                     MAXIDEPT=mean(csv$MAXIDEPT), SEDIDEPT=mean(csv$SEDIDEPT))
preddf$preds=predict(fm1,preddf ,type='response')
plot(preddf$HYDRPERI , preddf$preds, col=as.numeric(as.factor(preddf$CATCSIZE)),pch=16,ylab="Probablity")
legend('top',levels(as.factor(preddf$CATCSIZE)),col=1:3,pch=16,title='Catchment Size',inset=0.01)

## hard to interpet this one as probability increases at high and low hydroperiods (at least for low catchment sizes)?? 

## so probably best to leave out the interaction
v = csv[,1]
fm=glm(v~(HYDRPERI+NUMBINNU+CATCSIZE+MAXIDEPT+SEDIDEPT)+
         I(HYDRPERI^2)+I(NUMBINNU^2)+I(CATCSIZE^2)+I(MAXIDEPT^2)+I(SEDIDEPT^2),
       family=binomial,data=csv)
summary(fm)
n=length(csv$HYDRPERI)
fm1=step(fm,k=log(n))  ## then simplify fitted model based on BIC to remove non-significant terms
summary(fm1)
## pretty clear except for maximum depth
preddf = expand.grid(HYDRPERI=mean(csv$HYDRPERI), CATCSIZE=mean(csv$CATCSIZE), NUMBINNU=mean(csv$NUMBINNU), 
                     MAXIDEPT=1:620, SEDIDEPT=mean(csv$SEDIDEPT))
preddf$preds=predict(fm1,preddf ,type='response')
plot(preddf$MAXIDEPT, preddf$preds,pch=16,ylab="Probablity")
## looks like a clear optimum depth here!

## try species 9
v = csv[,9]
fm=glm(v~(HYDRPERI+NUMBINNU+CATCSIZE+MAXIDEPT+SEDIDEPT)+
         I(HYDRPERI^2)+I(NUMBINNU^2)+I(CATCSIZE^2)+I(MAXIDEPT^2)+I(SEDIDEPT^2),
       family=binomial,data=csv)
summary(fm)
n=length(csv$HYDRPERI)
fm1=step(fm,k=log(n))  ## then simplify fitted model based on BIC to remove non-significant terms
summary(fm1)
## all about maximum depth
preddf = expand.grid(HYDRPERI=mean(csv$HYDRPERI), CATCSIZE=mean(csv$CATCSIZE), NUMBINNU=mean(csv$NUMBINNU), 
                     MAXIDEPT=1:620, SEDIDEPT=mean(csv$SEDIDEPT))
preddf$preds=predict(fm1,preddf ,type='response')
plot(preddf$MAXIDEPT, preddf$preds,pch=16,ylab="Probablity")
## looks like a clear optimum depth here too!


###### but predictors are probably quite interrelated so maybe simpler to consider one at a time, as above



library(rpart)

v = csv[,1]
# grow tree
fit <- rpart(v ~ HYDRPERI+NUMBINNU+CATCSIZE+MAXIDEPT+SEDIDEPT,
             method="class", data=csv)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
