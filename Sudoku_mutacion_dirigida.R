Sudoku_mutacion_dirigida<-function(solucion_val,tamano_vector,numerero_swap)
{
  for(k in 1:numerero_swap)
  {
    # Funcion que hace la mutacion de los hijos
    pos_uno<-round(runif(1,1,tamano_vector),0)# elegir dos posicones aletorias
    pos_dos<-round(runif(1,1,tamano_vector),0)# elegir la segunda posicion aleatoria
    #print(pos_uno)
    #print(pos_dos)
    
    temp<-solucion_val[pos_uno] #almacenar el valor
    solucion_val[pos_uno]<-solucion_val[pos_dos]# actualizar el valor
    solucion_val[pos_dos]<-temp # actualizar el valor
    return(solucion_val)
    
  }
  
  
  
  
  
}