Sudoku_seleccion<-function(Vector_funcion_Objetivo,K_elementos,tamano_vector)
{
  #Sudoku_seleccion
  # esta funcion entrega las posiciones de los ganadores de los dos torneos
  Pos_torneo<-c()
  while(length(Pos_torneo)<K_elementos)
  {
    pos_ale<-round(runif(1,1,tamano_vector),0)
    if(pos_ale%in%Pos_torneo==FALSE)
    {
      Pos_torneo<-c(Pos_torneo,pos_ale)
    }else{
      
    }
  }
  Funcion_temp<-Vector_funcion_Objetivo[Pos_torneo]
  pos_torneo_min<-which.min(Funcion_temp)
  salida<-Pos_torneo[pos_torneo_min]
  return(salida)
  
  
}
  