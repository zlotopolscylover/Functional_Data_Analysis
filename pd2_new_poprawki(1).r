# Grupa 3: Wygeneruj dane z procesu Gaussowskiego o sredniej ??(t) = 4t i
# kowariancji Cov(X(t),X(s)) = exp(?||t ?| s|). Uzyj np funkcji
# simGauss(). Przeanalizuj zbieznosc sredniej probkowej. Porownaj
# kowariancje probowa z kowariancja populacji.


library(longmemo)
library(fda)
library(fda.usc)
#install.packages("MLmetrics")
library(MLmetrics)

#Kolejne jednostki/punkty np. czasu:
t<-seq(0,1,by=0.01) #punkty czasu
p<-length(t)

#Autokowariancja:
autocov<- exp(-(t)) # ro??nice mi??dzy jednostkami czasu wynosz??: 0, 0.01, ... ,0.99, 1
#Cov(X(t),X(s)) = exp(?||t ?| s|)

#Kowariancja (posta?? macierzowa):
cov<-matrix(ncol=p, nrow=p)
for(ss in 1:p){
  for(tt in 1:p){
    cov[tt,ss]<-exp(-abs(t[tt]-t[ss]))
  }
}

####################################Generowanie danych z procesu Gaussowskiego 
# o kowariancji Cov(X(t),X(s)) = exp(?||t ?| s|) i ??redniej ??(t) = 4t
#Przyk??ad:
x<-simGauss(autocov)+4*t
plot(t,x,type="l")



X_argval <- t

#Graficzna reprezentacja danych dla 10 probek:
X10<-sapply(1:10, function(x) simGauss(autocov)+4*t) #101x10
fd10<-fdata(t(X10))

fd10$names$main <- "Proces Gaussowski 10 probek"
fd10$names$xlab = "Jednostki czasu"
fd10$names$ylab = "Warto??ci procesu"
fd10$argvals = X_argval
fd10$rangeval = c(min(X_argval),max(X_argval))

plot(fd10,cex.lab=1.5)


#Graficzna reprezentacja danych dla 100 probek:
n<-100
X<-sapply(1:n, function(x) simGauss(autocov)+4*t) #101x100
fd<-fdata(t(X))

fd$names$main <- "Proces Gaussowski 100 probek"
fd$names$xlab = "Jednostki czasu"
fd$names$ylab = "Warto??ci procesu"
fd$argvals = X_argval
fd$rangeval = c(min(X_argval),max(X_argval))

plot(fd,cex.lab=1.5)


################################################??rednia probkowa vs populacyjna 


par(mfrow=c(4,4), mar=c(2,2,1,1))

for(n_ in c(1,2,5,10,20,30,40,50,60,70,80,90,100,300,500,1000)){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  fd$argvals = X_argval
  fd$rangeval = c(min(X_argval),max(X_argval))
  m<-func.mean(fd)
  plot(t,4*t, main = paste0('n=', n_),type="l")
  lines(m,col="red",lwd=2)
}

#msee <- rep(0,6)
#i=1
# dla roznych licznosci probek:5,10,50,100,500,1000
par(mfrow=c(2,3), mar=c(4,4,2,1))
for(n_ in c(5,10,50,100,500,1000)){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  fd$argvals = X_argval
  fd$rangeval = c(min(X_argval),max(X_argval))
  m<-func.mean(fd)
  plot(t,4*t, main = paste0('n=', n_),type="l",xlab="czas", ylab="warto??ci ??redniej",cex.lab=1.5)
  lines(m,col="red",lwd=2)
  #msee[i] <- MSE(m$data, 4*t)
  #i <- i+1
}


mean_populacyjna <- fdata(4*t)
mean_populacyjna$argvals = X_argval
mean_populacyjna$rangeval = c(min(X_argval),max(X_argval))
par(mfrow=c(2,3), mar=c(4,4,2,1))
for(n_ in c(5,10,50,100,500,1000)){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  fd$argvals = X_argval
  fd$rangeval = c(min(X_argval),max(X_argval))
  m<-func.mean(fd)
  plot(fd,col="grey", main = paste0('n=', n_),type="l",xlab="czas", ylab="warto??ci ??redniej",cex.lab=1.5)
  lines(mean_populacyjna,col="black",lwd=2)
  lines(m,col="red",lwd=2)
}



#Czarna linia to ??rednia populacyjna rowna 4t, natomaist czerwona krzywa to ??rednia probkowa

# Srednia bardzo szybko zbiega do sredniej (populacyjnej?) 4t


#MSE(m$data, 4*t)


######### MSE:

msee <- matrix(rep(0,6*1000),nrow=1000)
for (L in 1:1000)
{
i=1

for(n_ in c(5,10,50,100,500,1000)){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  fd$argvals = X_argval
  fd$rangeval = c(min(X_argval),max(X_argval))
  m<-func.mean(fd)
  msee[L,i] <- MSE(m$data, 4*t)
  i <- i+1
}}

apply(msee,2,mean)
apply(msee,2,median)
################################################### Kowariancja

#Kowariancja probkowa vs Kowariancja populacyjna
par(mfrow=c(1,2),  mar=c(1,1,1,1))
# Dla liczno??ci proby 1000
X<-sapply(1:1000, function(x) simGauss(autocov)+4*t)
fd<-fdata(t(X))
fd$argvals = X_argval
fd$rangeval = c(min(X_argval),max(X_argval))

fd_fd<-Data2fd(fd$argval,t(fd$data))
cov2<-var.fd(fd_fd,fd_fd)
fd_cov2<-eval.bifd(fd$argval,fd$argval,cov2)

d<-dim(fd$data)
persp(1:d[2],1:d[2],cov, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t",main="Kowariancja populacyjna")

persp(1:d[2],1:d[2],fd_cov2, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t",main="Kowariancja probkowa dla n=1000")




# Kowariancja probkowa dla ro??nych liczno??ci probek:

nn<-c(2, 5, 10, 50, 100, 500, 1000, 5000)
par(mfrow=c(2,4), mar=c(0.1, 0.3,0.1,0.1))#mar=rep(0.1, 4))

for(n_ in nn){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  
  fd_fd<-Data2fd(fd$argval,t(fd$data))
  cov2<-var.fd(fd_fd,fd_fd)
  fd_cov2<-eval.bifd(fd$argval,fd$argval,cov2)
  

  persp(t,t,fd_cov2, theta = 30, phi = 30,
        expand = 0.5, col=5, xlab="s",ylab="t", main=paste0('\n',paste0("n=", n_)))
}


