Sudoku_ver_solucion<-function(solucion)
{
  # funcion para ver la solucion del sudoku
  pos_ini<-1
  sudoku<-matrix(0,9,9)
  for(k in 1:9)
  {
    pos_fin<-pos_ini+8
    temp<-solucion[c(pos_ini:pos_fin)]
    sudoku[k,]<-temp
    pos_ini<-1+pos_fin
  }
  return(sudoku)
}



