## Clase 18

x1<-seq(-10,10,0.1)
X2_U<-(125-12*x1)/9
Tipo_U<-rep("Utilidad",length(x1))
data_U<-data.frame(Tipo=Tipo_U,X_1=x1,X_2=X2_U)
X2_E<-(40-5*x1)/3
Tipo_E<-rep("Empleados",length(x1))
data_E<-data.frame(Tipo=Tipo_E,X_1=x1,X_2=X2_E)
X2_I<-(120-5*x1)/7
Tipo_I<-rep("Inversion",length(x1))
data_I<-data.frame(Tipo=Tipo_I,X_1=x1,X_2=X2_I)
datos_total<-rbind(data_U,data_E,data_I)

library(ggplot2)
library(plotly)
plot<-ggplot(data=datos_total)+geom_line(aes(x=X_1,y=X_2,group=Tipo,colour=Tipo))
ggplotly(plot)
