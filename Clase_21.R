# Clase 21

##################
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

plot_final

# funcion objetivo
# para la función objetivo se debe hacer una elipse para eso se utiliza el geom_path
# 26.66
valor_Z<-26.67 # tamano de la z este es el tamano de la elipse
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
# Gradiente
# el gradiente de la funcion
gradiente_de_elipse<-function(X1,X2)
{
  vector_gradiente_1<-20*(X1-4)
  vector_gradiente_2<-40*(X2-4)
  vector_grad<-c(vector_gradiente_1,vector_gradiente_2)
  return(vector_grad)
}
############################################
######## puntos a probar ###################
############################################
punto_probar<-c(5.31,3.31)
############################################
############################################
gra<-gradiente_de_elipse(punto_probar[1],punto_probar[2])
gra<-gra/(sqrt(gra[1]^2+gra[2]^2))
gra_punt<-punto_probar+gra

datos_grad<-rbind(c(punto_probar[1],punto_probar[2]),c(gra_punt[1],gra_punt[2]))
datos_grad<-as.data.frame(datos_grad)
colnames(datos_grad)<-c("X_1","X_2")
plot_final<-plot_final+geom_line(data=datos_grad,aes(x=X_1,y=X_2))
ggplotly(plot_final)
## gradiente de la recta
gra_recta<-c(1,1)
gra_recta_pr<-gra_recta+punto_probar

datos_grad_rect<-rbind(c(punto_probar[1],punto_probar[2]),c(gra_recta_pr[1],gra_recta_pr[2]))
datos_grad_rect<-as.data.frame(datos_grad_rect)
colnames(datos_grad_rect)<-c("X_1","X_2")
plot_final<-plot_final+geom_line(data=datos_grad_rect,aes(x=X_1,y=X_2))
library(plotly)
ggplotly(plot_final)


##### correr hasta aqui
##### correr hasta aqui


##################################################################
##################################################################
#####      grafico de la superficie de respuesta en 3D      ######
##################################################################
##################################################################

x_1_gra<-seq(0,8,0.05)
x_2_gra<-seq(0,8,0.05)
matriz_Z<-matrix(nrow=length(x_2_gra),ncol=length(x_1_gra))
for(k in 1:length(x_1_gra))
{
  for( l in 1:length(x_2_gra) )
  {
    matriz_Z[l,k]<-(10*(x_1_gra[k]-4)^2)+(20*(x_2_gra[l]-4)^2)
    
  }
}
library(plotly)
plot_ly(z = ~matriz_Z,y=x_2_gra,x=x_1_gra) %>% add_surface() 

##################################################################
##################################################################
##################################################################

### sistemas de ecuaciones minimizar
matriz<-matrix(c(20,0,1,0,40,1,-1,-1,0),3)
vector<-c(80,160,5)
resultado<-solve(matriz,vector)
punto<-data.frame(X_1=resultado[1],X_2=resultado[2])
10*(resultado[1]-4)^2+20*(resultado[2]-4)^2
ggplotly(plot_final+geom_point(data=punto,aes(x=X_1,y=X_2)))

### sistemas de ecuaciones maximizar
matriz<-matrix(c(-20,0,-3,0,-40,-1,1,1,0),3)
vector<-c(-80,-160,-6)
resultado<-solve(matriz,vector)
punto<-data.frame(X_1=resultado[1],X_2=resultado[2])
ggplotly(plot_final+geom_point(data=punto,aes(x=X_1,y=X_2)))



###

matriz<-matrix(c(-20,0,3,3,0,-40,1,-2,3,1,0,0,3,-2,0,0),4)
vector<-c(-80,-160,-6,-6)
resultado<-solve(matriz,vector)
punto<-data.frame(X_1=resultado[1],X_2=resultado[2])


