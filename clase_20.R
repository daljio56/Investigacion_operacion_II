## clase 20

#  Primer ejercicio
#x1^2+4x1-x2=5
#x1^2+4x1-5=x2
x1<-seq(-6.5,2.5,0.1)
x2<-x1^2+4*x1-5
plot(x2~x1)
rest_1<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_1",length(x1)))
library(ggplot2)
plot<-ggplot()+geom_line(data=rest_1,aes(x=X_1,y=X_2))

#segunda restriccion

x1<-seq(-10,5,0.1)
x2<-(-2*x1+12)/3
rest_2<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_2",length(x1)))

data_total<-rbind(rest_1,rest_2)
plot<-ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo)) 
 
plot

# los ejes 
x1<-seq(-10,10,0.1)
x2<-rep(0,length(x1))
dato_eje_x<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_x",length(x1)))

x2<-seq(-10,10,0.1)
x1<-rep(0,length(x2))
dato_eje_y<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_y",length(x1)))

datos_ejes<-rbind(dato_eje_x,dato_eje_y)
plot<-ggplot()+geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))
plot
#funcion objetivo
ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo,color=Tipo)) +geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))


x1<-seq(-6,6,0.1)
# valor optimo
#Z=10.45
# Ingrese otro valor
Z<-1
x2<-(Z-(3*x1))/2
FO<-data.frame(X_1=x1,X_2=x2,Tipo=rep("funcion_objetivo",length(x1)))

data_total<-rbind(data_total,FO)
plot<-ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo,color=Tipo)) +geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))
plot

library(plotly)
ggplotly(plot)


###################
## Ejercicio  2
##################

#-x1^2+4x1-x2<=0
#x1^2+4x1-5=x2
x1<-seq(-1,5,0.1)
x2<-(-x1^2)+4*x1
plot(x2~x1)
rest_1<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_1",length(x1)))
library(ggplot2)
plot<-ggplot()+geom_line(data=rest_1,aes(x=X_1,y=X_2))
plot
#segunda restriccion
#2x1+3x2<=12
x1<-seq(-1,10,0.1)
x2<-(-2*x1+12)/3
rest_2<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_2",length(x1)))

data_total<-rbind(rest_1,rest_2)
plot<-ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo)) 

plot

# los ejes 
x1<-seq(-2,10,0.1)
x2<-rep(0,length(x1))
dato_eje_x<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_x",length(x1)))

x2<-seq(-5,6,0.1)
x1<-rep(0,length(x2))
dato_eje_y<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_y",length(x1)))

datos_ejes<-rbind(dato_eje_x,dato_eje_y)
plot<-ggplot()+geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))
plot
#funcion objetivo
ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo,color=Tipo)) +geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))


x1<-seq(-1,10,0.1)
Z=12
x2<-(Z-(2*x1))/1
FO<-data.frame(X_1=x1,X_2=x2,Tipo=rep("funcion_objetivo",length(x1)))
FO<-FO[FO$X_2>-5,]
FO<-FO[FO$X_2<6,]
data_total<-rbind(data_total,FO)
plot<-ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo,color=Tipo)) +geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))+scale_x_continuous(breaks = seq(-1, 10, 1)) +scale_y_continuous(breaks = seq(-5, 10, 1)) 
plot

library(plotly)
ggplotly(plot)


###################
## ejercicio  3
##################
library(ggplot2)
#primera restricción
#x1+x2<=6
x1<-seq(-3,7,0.1)
x2<-(6-x1)
rest_1<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_1",length(x1)))


#segunda restriccion
#x1-x2<=1
x1<-seq(-3,7,0.1)
x2<-(x1-1)
rest_2<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_2",length(x1)))

#tercera restriccion
#3x1+1x2>=6
x1<-seq(-3,7,0.1)
x2<-(6-3*x1)
rest_3<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_3",length(x1)))

#cuarta restriccion
#-3x1+2x2<=1
x1<-seq(-3,7,0.1)
x2<-(6+3*x1)/2
rest_4<-data.frame(X_1=x1,X_2=x2,Tipo=rep("Restriccion_4",length(x1)))

data_total<-rbind(rest_1,rest_2,rest_3,rest_4)





# los ejes 
x1<-seq(-2,10,0.1)
x2<-rep(0,length(x1))
dato_eje_x<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_x",length(x1)))

x2<-seq(-2,7,0.1)
x1<-rep(0,length(x2))
dato_eje_y<-data.frame(X_1=x1,X_2=x2,Tipo=rep("eje_y",length(x1)))

datos_ejes<-rbind(dato_eje_x,dato_eje_y)
plot<-ggplot()+geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))
plot
#funcion objetivo


data_total<-data_total[data_total$X_2<7 & data_total$X_2>(-2),] # se hace un filtro para que la informacion solo sea en unos rangos especificos del grafico
plot_final<-ggplot()+geom_line(data=data_total,aes(x=X_1,y=X_2,group=Tipo,color=Tipo)) +geom_line(data=datos_ejes,aes(x=X_1,y=X_2,group=Tipo))+scale_x_continuous(breaks = seq(-1, 10, 1)) +scale_y_continuous(breaks = seq(-5, 10, 1))  #grafico de las restricciones 



# funcion objetivo
# para la función objetivo se debe hacer una elipse para eso se utiliza el geom_path
# 26.66
valor_Z<-20 # tamano de la z este es el tamano de la elipse
lim_inf <-4-(sqrt(valor_Z/10)) # se calcula la tamano maximo de la elipse
lim_sup <-4+(sqrt(valor_Z/10)) # se calcula el tamano de la elipse
xprueba1<-seq(lim_inf,lim_sup,0.001) # se calcula los x que va a tener la elipse
yprueba1<-sqrt((valor_Z-10*(xprueba1-4)^2)/(20))+4 # se calcula los valores de y de la elipse
elipse_sup<-data.frame(X_1=xprueba1,X_2=yprueba1,Tipo=rep("Funcion_objetivo",length(xprueba1))) # se hace el data frame de la parte superior de la elipse
plot<-ggplot()+geom_line(data=elipse_sup,aes(x=X_1,y=X_2,group=Tipo,color=Tipo))
plot

xprueba1<-seq(lim_sup,lim_inf,-0.01) # como se utiliza el geom path se debe seguir la secuencia por eso se hace de mayor a menor para que la grafica se devuleva

yprueba1<-(-sqrt((valor_Z-10*(xprueba1-4)^2)/(20))+4) # se calcula los valores de x de la parte inferior de la elipse
elipse_inf<-data.frame(X_1=xprueba1,X_2=yprueba1,Tipo=rep("Funcion_objetivo",length(xprueba1))) # se calcula la parte inferior de la de la elipse 
elipse_inf<-rbind(elipse_inf,elipse_sup[1,]) # como es una ruta para que la elipse cierre se tiene que garantizar que el ultimo valor de la parte inferior se el primer de la parte superior
elipse_total<-rbind(elipse_sup,elipse_inf) # se colocan los datos totales

plot_final<-plot_final+geom_path(data=elipse_total,aes(x=X_1,y=X_2),linetype = 3)

library(plotly)
ggplotly(plot_final)

