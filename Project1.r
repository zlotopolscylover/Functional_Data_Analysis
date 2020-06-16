#Grupa 1: Przeanalizuj dane o â€Knee Angleâ€ ze zbioru danych â€gaitâ€ z pakietu fda.
install.packages("fda.usc")
library(fda.usc)
data(gait)
names(gait)
dane = gait[,,"Hip Angle"]

dane = fdata(dane)
names(dane)
dane$data
dane$rangeval
#dane dotyczace kolan ch³opców 10 cio letnich 
# 0.025 k¹t u³amek pe³nego okresu chodu 
# cykl kroku od 0 do 100 procent cyklu co 5 % wartosci to k¹ty 10 to jaka czesc kroku 
plot(dane)

dane <-fdata(dane, argvals = seq(0.025, 0.975, 0.05) )
plot(dane)

############ graficzna reprezenacja#######
data(gait)
names(gait)
df <- gait[,,"Hip Angle"]
t(df)

dane_fd<-fdata(t(df))
names(dane_fd)
dane_fd$argvals <- as.numeric(dimnames(gait)[[1]])
par(mar=c(6,5,1,1),mfrow=c(1,1))
plot(dane_fd, main = "Boys", xlab = "Parf of gait cycle", ylab = "Hip Angle(t)")

############################################

func.mean(dane_fd)
func.med.FM(dane_fd)
func.trim.FM(dane_fd)
func.var(dane_fd)


plot(dane_fd, col="gray", main = "Boys",
     xlab = "Parf of gait cycle", ylab = "Angle",
      ylim = c(0,90))
#lines(func.mean(dane_fd), lwd=2,col="red")
lines(func.var(dane_fd), lwd=2,col="blue")
legend(1,90,c("variance"), col=c("blue"),
       lty=c(1,1),lwd=c(2,2))


# wnioski:
# mediana pokrywa sie ze srednia wiec nie ma obserwacji odstajacych 


plot(func.var(dane_fd), lwd=2,col="blue")

# wariancja 



nox_cent<-fdata.cen(dane_fd)$Xcen$data
d<-dim(nox_cent)
nox_cov<-array(0,dim=c(d[2],d[2]))
for(i in 1:d[1]){
  nox_cov<-nox_cov+outer(nox_cent[i,],nox_cent[i,],"*")
}
nox_cov<-nox_cov/(d[1]-1)
nox_var<-func.var(dane_fd)$data[1,]
M<-sqrt(outer(nox_var,nox_var,"*"))
nox_cor<-nox_cov/M


par(mfrow=c(1,2),mar=c(7,1,0,0))
persp(1:d[2],1:d[2],nox_cov, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")
persp(1:d[2],1:d[2],nox_cor, theta = 30, phi = 30,
      expand = 0.5, col=5, xlab="s",ylab="t")


###########################################
dane_fd1<-Data2fd(dane_fd$argval,t(dane_fd$data))

par(mfrow=c(1,1))
par(oma=c(2,3,3,2))
par(mar=c(5,4,4,2)+0.2)
title(outer=TRUE,adj=0,main = list("Boxplot", cex=1.1,col="black", font=2))
pudelkowy<-boxplot.fd(dane_fd1, col=7, 
                      xlab = "Part of gait cycle", 
                      ylab = "Angle")

lines(dane_fd2[pudelkowy$outpoint],lwd=2,col="red")# to mi nie dzia³a



out.trim<-outliers.depth.trim(dane_fd,dfunc=depth.FM,nb=100)
names(out.trim)[1:3]
plot(dane_fd,col="grey")
out.trim[1]
lines(dane_fd[out.trim[[1]]],col="blue",lwd=2)


## baza furiera - lepsza gdy sezonowosc (bazuje na sinusach i cosinusach wiec jest ok dla 
#powtarzajacyh sie)
## spliny nie dla sezonowosci


basis_nox <- create.fourier.basis(nox$rangeval, nbasis=27)
nox_fd_new <- Data2fd(nox$argval,t(nox$data),
                      basisobj=basis_nox)
plot(nox_fd_new, col="black",ylab=expression(mglm^3),
     xlab = "Parf of gait cycle",main="Boys")


##  nie ma autlierow 


# creates fourier basis
basis_dane <- create.fourier.basis(dane_fd$rangeval, nbasis=27)
dane_fd_new <- Data2fd(dane_fd$argval,t(dane_fd$data),
                       basisobj=basis_dane)
plot(dane_fd_new, col="black",ylab = "Angle",
     xlab = "Part of gait cycle",main="Boys")
#nie bierzemy tego bo nie ma sezonowoœci
#zdecydowa³yœmy siêna na bazê splineów bo nie ma sezonowoœci, a w nim s¹ sin i cosinusy wiêc on dobrze modeluje dane sezonowe


#spline'y
basis_dane_b<-create.bspline.basis(dane_fd$argval,norder=3)
dane_fd_new_b<-Data2fd(dane_fd$argval,t(dane_fd$data),
                       basisobj=basis_dane_b)
plot(dane_fd_new_b, col="black",ylab = "Angle",
     xlab = "Part of gait cycle",main="Boys")
#wyg³adzanie nam daje te same wykresy, nie wnosi za du¿o

pincz<-Data2fd(pinchtime,pinchraw[,1:10])
plot(pincz, xlab = "Part of gait cycle",main="Boys", )

