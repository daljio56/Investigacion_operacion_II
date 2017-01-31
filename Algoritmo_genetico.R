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
tam_aleatorio<-length(solucion_base[solucion_base==0])
solucion_aleatoria<-round(runif(tam_aleatorio,1,9),0)
#solucion_base[solucion_base==0]<-solucion_aleatoria

contar_lineas<-function(solucion)
{
  total_repetidos<-0
  pos_ini<-1
  for(k in 1:9)
  {
    pos_fin<-pos_ini+8
    temp<-solucion[c(pos_ini:pos_fin)]
    temp_nrep<-length(temp[duplicated(temp)])
    total_repetidos<-total_repetidos+temp_nrep
    pos_ini<-1+pos_fin
  }
  return(total_repetidos)
}

contar_lineas(solucion_base)



contar_columnas<-function(solucion)
{
  base<-c(0:8)*9 # esta es la forma mas facil de crear la base con la que vamos a trabajar.
  total_repetidos<-0
  pos_ini<-1
  for(k in 1:9)
  {
    vector<-base+k
    #print(vector)
    temp<-solucion[vector]
    print(temp)
    temp_nrep<-length(temp[duplicated(temp)])
    total_repetidos<-total_repetidos+temp_nrep
    
  }
  return(total_repetidos)
}

contar_columnas(solucion_base)



contar_cuadros<-function(solucion)
{
  total_repetidos<-0
  for(k in 1:3)
  {
    for(l in 1:3)
    {
      temp_fila<-l+(2*(l-1))
      temp_columna<-k+(2*(k-1))
      inicio_temp_fila<-(temp_fila-1)
      vector_fila<-seq(inicio_temp_fila,temp_fila+1,1)*9
      vector_col<-seq(temp_columna,temp_columna+2,1)
      matriz_cuadro<-vector_fila%*%t(rep(1,3))+rep(1,3)%*%t(vector_col)
      #print(matriz_cuadro)
      vector_cuadro<-as.vector(t(matriz_cuadro))
      temp<-solucion[vector_cuadro]
      #print(temp)
      temp_nrep<-length(temp[duplicated(temp)])
      total_repetidos<-total_repetidos+temp_nrep
    }
  }
  return(total_repetidos)
}

contar_cuadros(solucion_base)

vector_col<-1:3
vector_fila<-c(0:2)*9
matriz_cuadro<-vector_fila%*%t(rep(1,3))+rep(1,3)%*%t(vector_col)

source("Sudoku_funcion_objetivo.R")

Sudoku_funcion_objetivo(solucion_aleatoria,solucion_base)

test<-solucion_base
test[test==0]<-solucion_aleatoria
test



#### crear poblacion incial
pob_inicial<-30
Poblacion_algoritmo<-matrix(0,pob_inicial,tam_aleatorio)
Funcion_Objetivo_Vector<-rep(0,pob_inicial)
for(k in 1:pob_inicial)
{
  Poblacion_algoritmo[k,]<-round(runif(tam_aleatorio,1,9),0)
  Funcion_Objetivo_Vector[k]<-Sudoku_funcion_objetivo(Poblacion_algoritmo[k,],solucion_base)
}

### selecion
k_soluciones<-4
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
tasa_mutacion<-0.05
Numero_mutacion<-runif(1,0,1)
if(Numero_mutacion<tasa_mutacion)
{
  Hijo<-Sudoku_mutacion(Hijo,tam_aleatorio)
  Funcion_hijo<-Sudoku_funcion_objetivo(Hijo,solucion_base)
}
source("Sudoku_Operador_Elitista.R")
Sudoku_Operador_Elitista(Poblacion_algoritmo,Funcion_Objetivo_Vector,Hijo,Funcion_hijo)





