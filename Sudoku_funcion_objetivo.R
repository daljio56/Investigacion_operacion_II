Sudoku_funcion_objetivo<-function(solucion_ale,solucion_bases)
{
  #Sudoku_funcion_objetivo
  # Recibe la solucion como un vector y entrega el numero de veces que esta repetida la solucion
  solucion_bases[solucion_bases==0]<-solucion_ale 
  solucion_probar<-solucion_bases
  ######### Primera parte hacer el contador de filas ##########
  
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
  
 
  
  ######### Segunda parte hacer el contador de columnas ##########
  
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
      #print(temp)
      temp_nrep<-length(temp[duplicated(temp)])
      total_repetidos<-total_repetidos+temp_nrep
      
    }
    return(total_repetidos)
  }
  
  ######## Tercera parte hacer el conatador de cuadros ##############
  
  
  
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
  
  salida<-contar_lineas(solucion_probar)+contar_columnas(solucion_probar)+contar_cuadros(solucion_probar)
  return(salida)
  
}