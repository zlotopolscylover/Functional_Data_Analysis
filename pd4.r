library(fda.usc)
data(gait)
gait
as.numeric(dimnames(gait)[[1]])
Y$rangeval
Y <- gait[,,"Knee Angle"]
X <- gait[,,"Hip Angle"]
X<- t(X)
Y<-t(Y)
Y <- fdata(Y)
X <- fdata(X)
Y$data
Y$argvals
X$data
X$argvals
aemet_y<-fdata(Y$data,as.numeric(dimnames(gait)[[1]]))
aemet_x1<-fdata(X$data,as.numeric(dimnames(gait)[[1]]))
reg_new<-fregre.basis.fr(aemet_x1,aemet_y)
reg_new$alpha.est
?fregre.basis.fr
par(mar=c(7,1,1,1),mfrow=c(1,2))
tt<-reg_new$argvals.y
beta<-eval.bifd(tt,tt,reg_new$beta.estbifd)
persp(tt,tt,beta)
plot(reg_new$alpha.est)
?fregre.basis.fr
reg_new$coefficients
reg_new$basis.s
X
par(mar=c(7,1,0,0))
plot(reg_new$beta.est,as.numeric(dimnames(gait)[[1]]),as.numeric(dimnames(gait)[[1]]),type="persp",theta=45,phi=30)
?flm_test
par(mar=c(7,1,0,0))
plot(reg_new$beta.est,as.numeric(dimnames(gait)[[1]]),as.numeric(dimnames(gait)[[1]]))

?fregre.basis.fr
par(mar=c(6,5,1,1),mfrow=c(1,1))
plot(aemet_y,col=1,main="Boys",ylab="Y(t)",xlab="Part of gait cycle")
lines(reg_new$fitted.values,col=2)
plot(reg_new$residuals,main="residuals",ylab="Y(t)",xlab="Part of gait cycle")

?plot
a<-colSums(((aemet_y-reg_new$fitted.values)$data)^2)
b<-rowSums((t(aemet_y$data)-colMeans(aemet_y$data))^2)
f<-1-a/b
(R2<-1-sum(a)/sum(b))

install.packages("goffda")
library(goffda)


data(aemet)
row.names(Y$data)<-NULL
row.names(X$data)<-NULL
aemet_y<-fdata(Y$data,Y$argval)
aemet_x1<-fdata(X$data,X$argval)
reg_new_2<-fregre.basis.fr(aemet_x1,aemet_y)

plot(aemet_y,col=1,main="aemet")
lines(reg_new_2$Y_hat+func.mean(aemet_y),col=2)
test<-flm_test(X=aemet_x1,Y=aemet_y,plot_dens = F,plot_proc = F)


summary(test)
?flm_test
#nie mamy podstaw do odrzucenia H_0, ¿e jest dobrze dopasowany















data(aemet)
aemet_y_scalar<-as.vector(rowMeans(Y$data))
y<-as.data.frame(aemet_y_scalar)
aemet_x1<-fdata(aemet$wind.speed$data,aemet$temp$argval)
f=aemet_y_scalar~aemet_x1
ldata=list("df"=y,"aemet_x1"=aemet_x1)
reg_new_scalar<-fregre.lm(f, ldata)
plot(reg_new_scalar)
summary(reg_new_scalar)


a<-sum(((aemet_y_scalar-reg_new_scalar$fitted.values))^2)
b<-sum((aemet_y_scalar-mean(aemet_y_scalar))^2)
f<-1-a/b
(R2<-1-sum(a)/sum(b))


plot(reg_new_scalar$beta.l$aemet_x1,main="functional beta estimation")

data(aemet)
aemet_y_scalar<-as.vector(rowMeans(aemet$temp$data))
aemet_x1<-fdata(aemet$wind.speed$data,aemet$temp$argval)
reg_new_scalar2<-fregre.pc(aemet_x1, aemet_y_scalar,kmax = 7,lambda=1)


reg_new_scalar2
plot(reg_new_scalar2$beta.est)


data(aemet)
aemet_y_scalar<-as.vector(rowMeans(aemet$temp$data))
aemet_x1<-fdata(aemet$wind.speed$data,aemet$temp$argval)
reg_new_scalar3<-fregre.pls(aemet_x1, aemet_y_scalar)

plot(reg_new_scalar3$beta.est)

?gait
