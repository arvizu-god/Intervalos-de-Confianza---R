library(readxl)
library(ggplot2)

peso_h1<-read_excel("peso.xlsm",sheet='Hoja4',range='K1:K728')
hist(peso_h1$peso_fin,freq=FALSE,col="lightcyan",ylim=c(0,0.05),main="Histograma del peso en los hombres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(peso_h1$peso_fin),sd=sd(peso_h1$peso_fin)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


altura_h1<-read_excel("peso.xlsm",sheet='Hoja4',range='L1:L726')
hist(altura_h1$estura_fin,freq=FALSE,col="lightcyan",ylim=c(0,0.05),main="Histograma de la estatura en los hombres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(altura_h1$estura_fin),sd=sd(altura_h1$estura_fin)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


peso_m1<-read_excel("peso.xlsm",sheet='hoja5',range='G1:G796')
hist(peso_m1$peso_fin2,freq=FALSE,col="lightcyan",ylim=c(0,0.05),main="Histograma del peso en las mujeres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(peso_m1$peso_fin2),sd=sd(peso_m1$peso_fin2)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")

altura_h2<-read_excel("peso.xlsm",sheet='hoja5',range='H1:H726')
hist(altura_h2$altura_fin2,freq=FALSE,col="lightcyan",ylim=c(0,0.09),main="Histograma de la estatura en las mujeres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(altura_h2$altura_fin2),sd=sd(altura_h2$altura_fin2)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


cintura_h1<-read_excel("cintura.xlsm",sheet='Hoja1',range='G1:G471')
hist(cintura_h1$cintura_H,freq=FALSE,col="lightcyan",ylim=c(0,0.05),main="Histograma de la cintura en los hombres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(cintura_h1$cintura_H),sd=sd(cintura_h1$cintura_H)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


cintura_m1<-read_excel("cintura.xlsm",sheet='Hoja1',range='I1:I607')
hist(cintura_m1$cintura_M,freq=FALSE,col="lightcyan",ylim=c(0,0.05),main="Histograma de la cintura en las mujeres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(cintura_m1$cintura_M),sd=sd(cintura_m1$cintura_M)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


pantorilla_h1<-read_excel("pantorilla.xlsm",sheet='Hoja1',range='G1:G467')
hist(pantorilla_h1$pantorilla_H,freq=FALSE,col="lightcyan",ylim=c(0,0.11),main="Histograma de la pantorilla en los hombres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(pantorilla_h1$pantorilla_H),sd=sd(pantorilla_h1$pantorilla_H)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


pantorilla_m1<-read_excel("pantorilla.xlsm",sheet='Hoja1',range='I1:I601')
hist(pantorilla_m1$pantorilla_M,freq=FALSE,col="lightcyan",ylim=c(0,0.11),main="Histograma de la pantorilla en las mujeres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(pantorilla_m1$pantorilla_M),sd=sd(pantorilla_m1$pantorilla_M)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


rodilla_h1<-read_excel("rodilla.xlsm",sheet='Hoja1',range='K1:K323')
hist(rodilla_h1$rodilla_H,freq=FALSE,col="lightcyan",ylim=c(0,0.19),main="Histograma de la rodilla en los hombres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(rodilla_h1$rodilla_H),sd=sd(rodilla_h1$rodilla_H)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")


rodilla_m1<-read_excel("rodilla.xlsm",sheet='Hoja1',range='M1:M418')
hist(rodilla_m1$rodilla_M,freq=FALSE,col="lightcyan",ylim=c(0,0.19),main="Histograma de la rodilla en las mujeres",xlab="",ylab="Densidad")
curve(dnorm(x,mean=mean(rodilla_m1$rodilla_M),sd=sd(rodilla_m1$rodilla_M)), from=0,to=250,add=TRUE, col="blue", lwd=2)
legend("topleft",col=c("blue"),legend =c("Densidad normal estimada"),lwd=2, bty = "n")

