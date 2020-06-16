#Kolejne jednostki/punkty np. czasu:

t<-seq(0,1,by=0.01) #punkty czasu
p<-length(t)
cov<-matrix(ncol=p, nrow=p)
for(ss in 1:p){
  for(tt in 1:p){
    cov[tt,ss]<-exp(-abs(t[tt]-t[ss]))
  }
}

autocov<- exp(-(t))

#b

#test perm
pvalues <- as.numeric(1000)
pvaluesa <- as.numeric(1000)
for( i in 1:1000){
  #dane 
  n<-50
  X<-sapply(1:n, function(x) simGauss(autocov)+4*t)
  fd_X<-fdata(t(X))
  Y<-sapply(1:n, function(x) simGauss(autocov)+4*t+2) 
  fd_Y<-fdata(t(Y))
  X_proc<-Data2fd(fd_X$argvals,t(fd_X$data))
  Y_proc<-Data2fd(fd_Y$argvals,t(fd_Y$data))
  
  #permutacyjny 
  test<-tperm.fd(X_proc,Y_proc,plotres = T,argvals =fd_X$argval , nperm=100)
  pvalues[i] <- test$pval 
  
  
  ## anova 
  
  n1<-dim(t(fd_X$data))[2]
  n2<-dim(t(fd_Y$data))[2]
  dane<-fdata(t(cbind(t(fd_X$data),t(fd_Y$data))),fd_X$argvals)
  grupa<-factor(c(rep(1,n1),rep(2,n2)))
  testa<-fanova.onefactor(dane,grupa,nboot=50,plot=TRUE, verbose=T)
  pvaluesa[i] <- testa$pvalue
  
  
  print(i)
  
}
sum(pvalues < 0.05) / 1000
sum(pvaluesa < 0.05) / 1000


