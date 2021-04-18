library(readxl)
library(ggplot2)
peso_h<-read_excel("peso.xlsm",sheet='Hoja4',range='K2:K726')
frame1<-data.frame(peso_h)
matriz1<-data.matrix(frame1)
media1<-mean(matriz1)
desviacion1<-sd(matriz1)
curve(dnorm(x,media1,desviacion1),xlim=c(0,200),col='green',xlab='altura',ylab='densidad normal',main='grafica')
error<-qnorm(0.975,mean=0,sd=1)
left<- media1-error*desviacion1
right<-media1+error*desviacion1
left
right
p1 <- ggplot(data = data.frame(x = c(left-50,right+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media1, sd =desviacion1)) + ylab("dnorm(80.99,(17.99)^2) para el peso de los hombres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p1+ geom_vline(xintercept = left, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)


altura_h<-read_excel("peso.xlsm",sheet='Hoja4',range='L2:L725')
frame2<-data.frame(altura_h)
matriz2<-data.matrix(frame2)
media2<-mean(matriz2)
desviacion2<-sd(matriz2)
curve(dnorm(x,media2,desviacion2),xlim=c(0,200),col='green',xlab='altura',ylab='densidad normal',main='grafica')
error<-qnorm(0.975,mean=0,sd=1)
left2<- media2-error*desviacion2
right2<-media2+error*desviacion2
left2
right2
p2 <- ggplot(data = data.frame(x = c(left2-50,right2+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media2, sd =desviacion2)) + ylab("dnorm(168.93,(7.74)^2) para la altura de los hombres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p2+ geom_vline(xintercept = left2, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right2, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



peso_m<-read_excel("peso.xlsm",sheet='hoja5',range='G2:G794')
frame3<-data.frame(peso_m)
matriz3<-data.matrix(frame3)
media3<-mean(matriz3)
desviacion3<-sd(matriz3)
curve(dnorm(x,media3,desviacion3),xlim=c(0,200),col='green',xlab='altura',ylab='densidad normal',main='grafica')
error<-qnorm(0.975,mean=0,sd=1)
left3<- media3-error*desviacion3
right3<-media3+error*desviacion3
left3
right3
p3 <- ggplot(data = data.frame(x = c(left3-50,right3+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media3, sd =desviacion3)) + ylab("dnorm(72.37,(17.96)^2) para el peso de las mujeres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p3+ geom_vline(xintercept = left3, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right3, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



altura_m<-read_excel("peso.xlsm",sheet='hoja5',range='H2:H796')
frame4<-data.frame(altura_m)
matriz4<-data.matrix(frame4)
media4<-mean(matriz4)
desviacion4<-sd(matriz4)
curve(dnorm(x,media4,desviacion4),xlim=c(0,200),col='green',xlab='altura',ylab='densidad normal',main='grafica')
error<-qnorm(0.975,mean=0,sd=1)
left4<- media4-error*desviacion4
right4<-media4+error*desviacion4
left4
right4
p4 <- ggplot(data = data.frame(x = c(left4-50,right4+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media4, sd =desviacion4)) + ylab("dnorm(80.99,(17.99)^2) para la altura de las mujeres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p4+ geom_vline(xintercept = left4, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right4, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



cintura_h<-read_excel("cintura.xlsm",sheet='Hoja1',range='G2:G471')
frame5<-data.frame(cintura_h)
matriz5<-data.matrix(frame5)
media5<-mean(matriz5)
desviacion5<-sd(matriz5)
error<-qnorm(0.975,mean=0,sd=1)
left5<-media5-error*desviacion5
right5<-media5+error*desviacion5
left5
right5
p5 <- ggplot(data = data.frame(x = c(left5-50,right5+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media5, sd =desviacion5)) + ylab("dnorm(100.13,(13.04)^2) para la cintura de los hombres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p5+ geom_vline(xintercept = left5, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right5, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

cintura_m<-read_excel("cintura.xlsm",sheet='Hoja1',range='I2:I607')
frame6<-data.frame(cintura_m)
matriz6<-data.matrix(frame6)
media6<-mean(matriz6)
desviacion6<-sd(matriz6)
error<-qnorm(0.975,mean=0,sd=1)
left6<-media6-error*desviacion6
right6<-media6+error*desviacion6
left6
right6
p6 <- ggplot(data = data.frame(x = c(left6-50,right6+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media6, sd =desviacion6)) + ylab("dnorm(98.44,(15.05)^2) para la cintura de las mujeres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p6+ geom_vline(xintercept = left6, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right6, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



pantorilla_h<-read_excel("pantorilla.xlsm",sheet='Hoja1',range='G2:G467')
frame7<-data.frame(pantorilla_h)
matriz7<-data.matrix(frame7)
media7<-mean(matriz7)
desviacion7<-sd(matriz7)
error<-qnorm(0.975,mean=0,sd=1)
left7<-media7-error*desviacion7
right7<-media7+error*desviacion7
left7
right7
p7<- ggplot(data = data.frame(x = c(left7-50,right7+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media7, sd =desviacion7)) + ylab("dnorm(35.51,(4.12)^2) para la pantorilla de los hombres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p7+ geom_vline(xintercept = left7, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right7, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



pantorilla_m<-read_excel("pantorilla.xlsm",sheet='Hoja1',range='I2:I601')
frame8<-data.frame(pantorilla_m)
matriz8<-data.matrix(frame8)
media8<-mean(matriz8)
desviacion8<-sd(matriz8)
error<-qnorm(0.975,mean=0,sd=1)
left8<-media8-error*desviacion8
right8<-media8+error*desviacion8
left8
right8
p8<- ggplot(data = data.frame(x = c(left8-50,right8+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media8, sd =desviacion8)) + ylab("dnorm(35.07,(4.92)^2) para la pantorilla de las mujeres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p8+ geom_vline(xintercept = left8, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right8, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



rodilla_h<-read_excel("rodilla.xlsm",sheet='Hoja1',range='K2:K323')
frame9<-data.frame(rodilla_h)
matriz9<-data.matrix(frame9)
media9<-mean(matriz9)
desviacion9<-sd(matriz9)
error<-qnorm(0.975,mean=0,sd=1)
left9<-media9-error*desviacion9
right9<-media9+error*desviacion9
left9
right9
p9<- ggplot(data = data.frame(x = c(left9-50,right9+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media9, sd =desviacion9)) + ylab("dnorm(51.54,(3.47)^2) para la rodilla de los hombres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p9+ geom_vline(xintercept = left9, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right9, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)



rodilla_m<-read_excel("rodilla.xlsm",sheet='Hoja1',range='M2:M418')
frame10<-data.frame(rodilla_m)
matriz10<-data.matrix(frame10)
media10<-mean(matriz10)
desviacion10<-sd(matriz10)
error<-qnorm(0.975,mean=0,sd=1)
left10<-media10-error*desviacion10
right10<-media10+error*desviacion10
left10
right10
p10<- ggplot(data = data.frame(x = c(left10-50,right10+50)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = media10, sd =desviacion10)) + ylab("dnorm(47.13,(3.04)^2) para la rodilla de las mujeres") +xlab("X")+
  scale_y_continuous(breaks = NULL)
p10+ geom_vline(xintercept = left10, linetype="dotted", color = "blue", size=1.5)+ geom_vline(xintercept = right10, linetype="dotted", color = "blue", size=1.5)+theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)

