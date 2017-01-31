Sudoku_cruces<-function(Padres_1,Padres_2,solucion_base)
{
  #funcion que entrega que recibe los padres y entrega los los hijos
  temp_Padres_1<-solucion_base
  temp_Padres_2<-solucion_base
  temp_Padres_1[temp_Padres_1==0]<-Padres_1 # Convertir el Padre 1 en posiciones de 81
  temp_Padres_2[temp_Padres_2==0]<-Padres_2 # Convertir el Padre 2 en posiciones de 81
  
  punto_corte<-round(runif(1,1,8),0)*9 # encontrar el punto aletorio donde hacer el corte
  Primercorte<-c(1,punto_corte) #hacer el primer corte
  Segundocorte<-c((punto_corte+1),81) # hacer el segundo corte
  Hijo_1<-c(temp_Padres_1[Primercorte[1]:Primercorte[2]],temp_Padres_2[Segundocorte[1]:Segundocorte[2]])#cruzar los padres
  Hijo_2<-c(temp_Padres_2[Primercorte[1]:Primercorte[2]],temp_Padres_1[Segundocorte[1]:Segundocorte[2]])#cruzar los padres
  posciones_originales<-which(solucion_base==0) # determinar las posicioens que se deben extraer del algoritmo
  Hijo_1<-Hijo_1[posciones_originales]
  Hijo_2<-Hijo_2[posciones_originales]
  salidas<-list(Hijo_1,Hijo_2)
  return(salidas)
}
