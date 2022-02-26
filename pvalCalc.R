##p-value calculator
library(tidyverse)
##########################create function for graph#############################

##normal distribution#####################
graphnormcurveR = function(test.stat,pval,mean=0,sd=1){
  x = seq(-3,3,length=100)*sd + mean
  ggplot(data.frame(x), aes(x)) +
    stat_function(fun = dnorm,args = list(mean = mean,sd = sd))+
    stat_function(fun = dnorm,geom = "area",xlim=c(test.stat,max(x)),fill = "steelblue",args = list(mean = mean,sd = sd))+
    labs(y="density",subtitle=(pval))
}
graphnormcurveL = function(test.stat,pval,mean=0,sd=1){
  x = seq(-3,3,length=100)*sd + mean
  ggplot(data.frame(x), aes(x)) +
    stat_function(fun = dnorm,args = list(mean = mean,sd = sd))+
    stat_function(fun = dnorm,geom = "area",xlim=c(min(x),test.stat),fill = "steelblue",args = list(mean = mean,sd = sd))+
    labs(y="density",subtitle=(pval))
}
graphnormcurve2 = function(test.stat,pval,mean=0,sd=1){
  x = seq(-3,3,length=100)*sd + mean
  negvalue=-1*test.stat
  ggplot(data.frame(x), aes(x)) +
    stat_function(fun = dnorm,args = list(mean = mean,sd = sd))+
    stat_function(fun = dnorm,geom = "area",xlim=c(test.stat,max(x)),fill = "steelblue",args = list(mean = mean,sd = sd))+
    stat_function(fun = dnorm,geom = "area",xlim=c(min(x),negvalue),fill = "steelblue",args = list(mean = mean,sd = sd))+
    labs(y="density",subtitle=(pval))
}

##F-distribution#####################
graphFcurveR = function(test.stat,df1,df2,pval){
ggplot(data.frame(x = c(0, 5)), aes(x)) +
  stat_function(fun = df,args = list(df1 = df1,df2 = df2)) +
  stat_function(fun = df,xlim=c(test.stat,5),geom = "area",fill = "steelblue",args = list(df1 = df1,df2 = df2)) +
  labs(y="density",subtitle=(pval))
}
graphFcurveL = function(test.stat,df1,df2,pval){
  ggplot(data.frame(x = c(0, 5)), aes(x)) +
    stat_function(fun = df,args = list(df1 = df1,df2 = df2)) +
    stat_function(fun = df,xlim=c(0,test.stat),geom = "area",fill = "steelblue",args = list(df1 = df1,df2 = df2)) +
    labs(y="density",subtitle=(pval))
}

##t-distribution#####################
graphtcurveR = function(test.stat,df,pval){
  ggplot(data.frame(x = c(-4, 4)), aes(x)) +
    stat_function(fun = dt, args =list(df = df)) +
    stat_function(fun = dt, args =list(df = df),xlim = c(test.stat,4),geom = "area",fill="steelblue") +
    labs(y="density",subtitle=(pval))
}
graphtcurveL = function(test.stat,df,pval){
  ggplot(data.frame(x = c(-4, 4)), aes(x)) +
    stat_function(fun = dt, args =list(df = df)) +
    stat_function(fun = dt, args =list(df = df),xlim = c(-4,test.stat),geom = "area",fill="steelblue") +
    labs(y="density",subtitle=(pval))
}
graphtcurve2 = function(test.stat,df,pval){
  neg=-1*test.stat
  ggplot(data.frame(x = c(-4, 4)), aes(x)) +
    stat_function(fun = dt, args =list(df = df)) +
    stat_function(fun = dt, args =list(df = df),xlim = c(-4,neg),geom = "area",fill="steelblue") +
    stat_function(fun = dt, args =list(df = df),xlim = c(test.stat,4),geom = "area",fill="steelblue")+
    labs(y="density",subtitle=(pval))
}

###Chi-Squared squared distribution#############
graphchiR = function(test.stat,df,pval){
  ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
    stat_function(fun = dchisq, args = list(df = df))+
    stat_function(fun = dchisq, args = list(df = df),xlim = c(test.stat,20),geom = "area",fill="steelblue")+
    labs(y="density",subtitle=(pval))
}
graphchiL = function(test.stat,df,pval){
  ggplot(data.frame(x = c(0, 20)), aes(x = x)) +
    stat_function(fun = dchisq, args = list(df = df))+
    stat_function(fun = dchisq, args = list(df = df),xlim = c(0,test.stat),geom = "area",fill="steelblue")+
    labs(y="density",subtitle=(pval))
}

#########################create function for p-value############################

pvalCalc = function(distr,alt.hypo,test.stat,mean=0,sd=1,df,df2){
 #Normal distribution
  if (distr == "norm"){
    if (alt.hypo == "leftT"){
      pval=pnorm(q=test.stat,mean=mean,sd=sd,lower.tail = T)
      graphnormcurveL(test.stat=test.stat,pval=pval,mean = mean,sd=sd)
    } else if (alt.hypo == "rightT"){
      pval=pnorm(q=test.stat,mean=mean,sd=sd,lower.tail = F)
      graphnormcurveR(test.stat=test.stat,pval=pval,mean = mean,sd=sd)
    } else {
      pval=2*pnorm(q=test.stat,mean=mean,sd=sd,lower.tail = F)
      graphnormcurve2(test.stat=test.stat,pval=pval,mean = mean,sd=sd)
    }
 #t-distribution
  } else if (distr == "tdistr"){
    if (alt.hypo == "leftT"){
      pval=pt(q=test.stat,df=df,lower.tail = T)
      graphtcurveL(test.stat,df,pval)
    } else if (alt.hypo == "rightT"){
      pval=pt(q=test.stat,df=df,lower.tail = F)
      graphtcurveR(test.stat,df,pval)
    } else {
      pval=2*pt(q=test.stat,df=df,lower.tail = F)
      graphtcurve2(test.stat,df,pval)
    }
 #F-distribution
  } else if (distr == "Fdistr"){
    if (alt.hypo == "leftT"){
      pval=pf(q=test.stat, df1 = df, df2 = df2, lower.tail = T)
      graphFcurveL(test.stat,df,df2,pval)
    } else {
      pval=pf(q=test.stat, df1 = df, df2 = df2, lower.tail = F)
      graphFcurveR(test.stat,df,df2,pval)
    }
 #Chi-squared distribution
  } else {
    if (alt.hypo == "leftT"){
    pval=pchisq(q=test.stat, df=df, lower.tail=T)
    graphchiL(test.stat,df,pval)
    } else {
    pval=pchisq(q=test.stat, df=df, lower.tail=F)
    graphchiR(test.stat,df,pval)
    }
  }
}



