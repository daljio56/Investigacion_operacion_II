## Este programa no va a correr porque dañe el archivo de distancias. 
# Este programa debe de estar en el directorio de trabajo. 
rm(list=ls())
source("Fun_AG.R") # Cargar las funciones del algoritmo genetico 
dis <- read.csv("C_Col.csv",header = FALSE,sep = ",") # leemos un archivo con la posicion de cada ciudad
c_ciu <- dim(dis)
Q_IND <- 100
matriz<-matrix(nrow = c_ciu[1],ncol= c_ciu[1]) # se guardan las distancias
pobini<-matrix(ncol = c_ciu[1],nrow = Q_IND) # matriz donde se va a guardar la poblacion incial




##### parte incial

library(ggplot2) 
library(plotly)
dis2<-dis # creamos una objeto con las posicion de las ciudades
dis2$ID<-seq(1:nrow(dis2)) # creamos un ID para cada ciudad
plot<-ggplot(data=dis2,aes(x=V2,y=V1,label = ID))+ geom_point(size = 0.5,color="red") + geom_text(hjust = 0.1, nudge_x = 0.1,size = 2.5) #graficamos las ciudades
ggplotly(plot) # imprimimos las ciudades

### imprimir las soluciones de los equipos
# escribir las soluciones de los equipos
solprueba1<-c(13,14,39,22,5,27,15,6,26,2,11,12,1,36,29,28,30,33,31,40,41,7,8,32,44,16,18,19,4,3,37,17,35,43,20,21,34,38,42,9,25,24,23,10)
solprueba2<-c(14,13,39,22,5,27,15,6,26,2,11,12,1,36,29,28,30,33,31,40,41,7,8,32,44,16,18,19,4,3,37,17,35,43,20,21,34,38,42,9,25,24,23,10)
solucionesgrupos<-rbind(solprueba1,solprueba2) # crean las soluciones en una matriz
recoridosgrupos<-c()
for(k in 1:nrow(solucionesgrupos))
{
  temp<-as.data.frame(dis[solucionesgrupos[k,],])#crear un data frame con los datos de los estudiantes
  temp$IDGRUPO<-rep(paste("Grupo_",k,sep=""),nrow(temp))
  temp$ID<-solucionesgrupos[k,]
  recoridosgrupos<-rbind(recoridosgrupos,temp)
}
plot3<-plot+geom_path(data=recoridosgrupos,aes(x=V2,y=V1,label = ID,group=IDGRUPO,color=IDGRUPO,alpha=0.4))# graficar las diferentes soluciones
ggplotly(plot3) # graficarla solucion


# se calcula la distancia euclidiana
for(j in 1:c_ciu[1]){
  for(i in 1:c_ciu[1]){
    matriz[j,i]<-sqrt(sum((dis[j,]-dis[i,])^2)) # se calcula la distancia euclidiana la ciudad i a la j
  }
}

# se determina la población incial y las funciones objetivos
Fun_Objetivos<-vector(length = Q_IND)
for(i in 1:Q_IND){
  pobini[i,]<-sample(c_ciu[1])
  #pobini[i,]<-greedy(matriz) # se genera la pob con un greedy
  Fun_Objetivos[i]<-fun_obj(pobini[i,],matriz) # se calcula la funcion objetivo para cada 
}

solini<-min(Fun_Objetivos)

# Parámetros del algoritmo génetico
maxiter<-200000
tc<-0.9
tm<-0.1
muestra<-6 # cantidad de individuos para el torneo. 

for (k in 1:maxiter){
  Padre1 <- pobini[torneo(Fun_Objetivos,muestra),]
  Padre2 <- pobini[torneo(Fun_Objetivos,muestra),]
  
  #se cruzan y solo se selecciona uno
  selec<-sample(2,1) # aleatorio, toma el valor de 1 o 2.
  if (runif(1)<tc){
    hijo<-pmx(Padre1,Padre2)[selec,]
  }else{
    hijo<-rbind(Padre1,Padre2)[selec,]
  }

  # se raliza la mutación. 
  
  if (runif(1)<tm){
   hijo<-opt2(hijo)
  }
  
  # Función objetivo del hijo. 
  FOhijo<-fun_obj(hijo,matriz)
  
  # Individuo malo. 
  FOPeor<-max(Fun_Objetivos) # Función objetivo más grande
  PosFOPeor<-which(Fun_Objetivos==max(Fun_Objetivos)) # posición del individuo más malo. 

  if(length(PosFOPeor)>1){ # Cuantos individuos malos hay. Solo se toma uno
     mu<-sample(length(PosFOPeor),1)
     PosFOPeor<-PosFOPeor[mu]
  }
  
  if (FOhijo < FOPeor){
    Fun_Objetivos[PosFOPeor]<-FOhijo
    pobini[PosFOPeor,]<-hijo
  }
}

solfin<-c(solini,min(Fun_Objetivos)) # La primera es la solución incial y la segundo la encontrada por el genético 
solini
solfin















############ graficar la solucion optima



solucion_optima_encontrada<-pobini[which.min(Fun_Objetivos),] #encontrar la solucion optima
recorrido_optimo<-as.data.frame(dis[solucion_optima_encontrada,]) # volverla data frame sus putnos
recorrido_optimo$ID<-solucion_optima_encontrada # darle un ID


plot2<-plot+geom_path(data=recorrido_optimo,aes(x=V2,y=V1,label = ID)) #grafica de los puntos 
ggplotly(plot2)



# probar soluciones






ggplotly(plot3+geom_path(data=recorrido_optimo,aes(x=V2,y=V1,label = ID)))



