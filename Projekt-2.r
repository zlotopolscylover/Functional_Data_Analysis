

# Grupa 3: Wygeneruj dane z procesu Gaussowskiego o sredniej μ(t) = 4t i
# kowariancji Cov(X(t),X(s)) = exp(−|t − s|). Uzyj np funkcji
# simGauss(). Przeanalizuj zbieznosc sredniej próbkowej. Porównaj
# kowariancje próbowa z kowariancja populacji.



library(longmemo)
library(fda)
library(fda.usc)

p<-12

autocov<- exp(-(0:(p-1)))

cov<-matrix(ncol=p, nrow=p)
for(s in 1:p){
  for(t in 1:p){
    cov[t,s]<-exp(-abs(t-s))
  }
}

t<-(1:p)


x<-simGauss(autocov)+4*t
plot(x)



n<-100
X<-sapply(1:n, function(x) simGauss(autocov)+4*t)
fd<-fdata(t(X))
m<-func.mean(fd)
plot(m)
dev.off()




par(mfrow=c(5,5), mar=c(2,1,1,1))

for(n_ in 1:25){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  m<-func.mean(fd)
  plot(4*t, main = paste0('', n_),type="l")
  lines(m,col="red")
}


# dla roznych licznosci probek:2,5,10,25,50,100
par(mfrow=c(2,3), mar=c(2,2,2,1))
for(n_ in c(2,5,10,25,50,100)){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  m<-func.mean(fd)
  plot(4*t, main = paste0('', n_),type="l")
  lines(m,col="red")
}


# Srednia bardzo szybko zbiega do sredniej (populacyjnej?) 4t


dev.off()
par(mfrow=c(2,1), mar=rep(1,4))



X<-sapply(1:1000, function(x) simGauss(autocov)+4*t)
fd<-fdata(t(X))

fd_fd<-Data2fd(fd$argval,t(fd$data))
cov2<-var.fd(fd_fd,fd_fd)
fd_cov2<-eval.bifd(fd$argval,fd$argval,cov2)

d<-dim(fd$data)
persp(1:d[2],1:d[2],fd_cov2, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")


persp(1:d[2],1:d[2],cov, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")





nn<-c(2, 5, 10, 50, 100, 150, 1000, 5000)
par(mfrow=c(2,4), mar=rep(0.1, 4))

for(n_ in nn){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  
  fd_fd<-Data2fd(fd$argval,t(fd$data))
  cov2<-var.fd(fd_fd,fd_fd)
  fd_cov2<-eval.bifd(fd$argval,fd$argval,cov2)
  
  d<-dim(fd$data)
  persp(1:d[2],1:d[2],fd_cov2, theta = 30, phi = 30,
        expand = 0.5, col=5, xlab="s",ylab="t", main=paste0('\n', n_))
}




?kde2d
smooth<-kde2d(1:d[2],1:d[2],fd_cov2, n = 1000)

persp(smooth$x, smooth$y, smooth$z, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")
# bez sensu


# kowariancja po wygladzeniu, punkty czasu co 0.1:

nn2<-c(2,5,10, 50)
par(mfrow=c(2,2), mar=rep(0.1, 4))

for(n_ in nn2){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  
  fd_fd<-Data2fd(fd$argval,t(fd$data))
  cov2<-var.fd(fd_fd,fd_fd)
  x<-seq(min(fd$argval),
         max(fd$argval),
         by=0.1)
  cov_smooth<-eval.bifd(x,x,cov2)
  
  
  persp(x,x,cov_smooth, theta = 30, phi = 30,
        expand = 0.5, col=5, xlab="s",ylab="t", main=paste0('\n', n_))
}




nn3<-c(100, 150, 1000, 5000)
par(mfrow=c(2,2), mar=rep(0.1, 4))

for(n_ in nn3){
  X<-sapply(1:n_, function(x) simGauss(autocov)+4*t)
  fd<-fdata(t(X))
  
  fd_fd<-Data2fd(fd$argval,t(fd$data))
  cov2<-var.fd(fd_fd,fd_fd)
  x<-seq(min(fd$argval),
         max(fd$argval),
         by=0.1)
  cov_smooth<-eval.bifd(x,x,cov2)
  
  
  persp(x,x,cov_smooth, theta = 30, phi = 30,
        expand = 0.5, col=5, xlab="s",ylab="t",main=paste0('\n', n_))
}



# Kowariancja populacyjna:

cov_new <- matrix(rep(0,12*12),nrow=12)
x_new <- seq(min(fd$argval),
             max(fd$argval),by=1)

for (t in x_new){
  for (s in x_new){
    cov_new[t,s] <- exp(-abs(t-s))
  }
}

par(mfrow=c(1,1))
persp(x_new,x_new,cov_new, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")


#wygladzony:

x_new <- seq(min(fd$argval),
             max(fd$argval),by=0.1)
cov_new<-matrix(ncol=length(x_new), nrow=length(x_new))

for(t in 1:length(x_new)){
  for(s in 1:length(x_new)){
    cov_new[t,s]<-exp(-abs(x_new[t]-x_new[s]))
  }
}

par(mfrow=c(1,1))
persp(x_new,x_new,cov_new, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")


# Kowariancja próbkowa wolno zbiega do kowariancji populacyjnej




h<-seq(-1, 1, by = 0.01)
dev.off()
plot(h, exp(-abs(h)), type= 'l', main='Kowariancja',  xlab='t-s', ylab = 'exp(-abs(t-s))')





