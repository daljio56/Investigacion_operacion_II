####################################################
####################################################
###########                    ##################### 
###########  Sudoku Principal  #####################
###########                    ##################### 
####################################################
####################################################
# algoritmo genetico que resuelve sudoku

###########  Ingresar el sudoku  #####################

l_1<-c(0,7,2,3,8,5,4,0,0)
l_2<-c(0,3,9,0,1,6,0,0,0)
l_3<-c(1,0,0,2,7,0,3,0,6)
l_4<-c(7,8,0,0,0,0,6,4,0)
l_5<-c(5,0,0,0,0,0,0,0,7)
l_6<-c(0,9,4,0,0,0,0,3,1)
l_7<-c(4,0,1,0,6,3,0,0,8)
l_8<-c(0,0,0,9,2,0,1,6,0)
l_9<-c(0,0,8,5,4,1,2,7,0)
solucion_base<-c(l_1,l_2,l_3,l_4,l_5,l_6,l_7,l_8,l_9)

###########  Parametros generales del Algoritmo Genetico ########### 
pob_inicial<-30
k_soluciones<-4
tasa_mutacion<-0.1
Numero_generacion<-5000

###########  crear poblacion incial ########### 

Poblacion_algoritmo<-matrix(0,pob_inicial,tam_aleatorio)
Funcion_Objetivo_Vector<-rep(0,pob_inicial)
#crear data frame guardar imcumbete
generacion<-rep(0,(Numero_generacion+1))
Mejor_funcion<-rep(0,(Numero_generacion+1))
for(k in 1:pob_inicial)
{
  Poblacion_algoritmo[k,]<-round(runif(tam_aleatorio,1,9),0)
  Funcion_Objetivo_Vector[k]<-Sudoku_funcion_objetivo(Poblacion_algoritmo[k,],solucion_base)
}

Mejor_funcion[1]<-min(Funcion_Objetivo_Vector)
for(gen in 1:Numero_generacion)
{
  ########## selecion
  source("Sudoku_seleccion.R")
  Pos_padre_1<-Sudoku_seleccion(Funcion_Objetivo_Vector,k_soluciones,pob_inicial)
  Pos_padre_2<-Sudoku_seleccion(Funcion_Objetivo_Vector,k_soluciones,pob_inicial)
  Padre_1<-Poblacion_algoritmo[Pos_padre_1,]
  Padre_2<-Poblacion_algoritmo[Pos_padre_2,]
  
  #### crossover
  
  source("Sudoku_cruces.R")
  hijos<-Sudoku_cruces(Padre_1,Padre_2,solucion_base)
  Hijo_1<-hijos[[1]]
  Hijo_2<-hijos[[2]]
  Funcion_hijos<-rep(0,2)
  Funcion_hijos[1]<-Sudoku_funcion_objetivo(Hijo_1,solucion_base)
  Funcion_hijos[2]<-Sudoku_funcion_objetivo(Hijo_2,solucion_base)
  if(Funcion_hijos[1]<Funcion_hijos[2])
  {
    Hijo<-Hijo_1
    Funcion_hijo<-Funcion_hijos[1]
  }else{
    Hijo<-Hijo_2
    Funcion_hijo<-Funcion_hijos[2]
  }
  
  #### mutacion
  
  source("Sudoku_mutacion.R")
  
  Numero_mutacion<-runif(1,0,1)
  if(Numero_mutacion<tasa_mutacion)
  {
    Hijo<-Sudoku_mutacion(Hijo,tam_aleatorio)
    Funcion_hijo<-Sudoku_funcion_objetivo(Hijo,solucion_base)
  }
  
  ###### Operador elitismo ########
  
  source("Sudoku_Operador_Elitista.R")
  salida<-Sudoku_Operador_Elitista(Poblacion_algoritmo,Funcion_Objetivo_Vector,Hijo,Funcion_hijo)
  Poblacion_algoritmo<-salida[[1]]
  Funcion_Objetivo_Vector<-salida[[2]]
  
  ####### guardar informacion grafico ########
  generacion[gen+1]<-gen
  Mejor_funcion[gen+1]<-min(Funcion_Objetivo_Vector)
}

####### graficar convergencia #############
datos_proceso<-data.frame(Gen=generacion,Fun_Obj=Mejor_funcion)
library(ggplot2)
ggplot(data=datos_proceso)+geom_line(aes(x=Gen,y=Fun_Obj))



