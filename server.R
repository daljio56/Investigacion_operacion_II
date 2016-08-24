palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    valores_x2<-as.numeric(seq(-10,100,1))
    Z<-input$clusters
    c1<-3
    c2<-2
    Z_x2<-as.numeric(valores_x2)
    Z_x1<-Z-(c2/c1)*Z_x2
    datos_funcion<-data.frame(x_val=Z_x1,y_val=Z_x2,tipo="funcion")
    
    #acabado
    A_x2<-as.numeric(valores_x2)
    A_x1<-50-(0.5)*A_x2
    datos_acabado<-data.frame(x_val=A_x1,y_val=A_x2,tipo="acabado")
    
    #Carpinteria
    C_x2<-as.numeric(valores_x2)
    C_x1<-80-C_x2
    datos_carpinteria<-data.frame(x_val=C_x1,y_val=C_x2,tipo="carpinteria")
    
    #Demanda
    D_x2<-as.numeric(valores_x2)
    D_x1<-40
    datos_demanda<-data.frame(x_val=D_x1,y_val=D_x2,tipo="demanda")
    
    datos_totales<-rbind(datos_funcion,datos_acabado,datos_carpinteria,datos_demanda)
    ## ejes
    #eje x
    ejeX_x2<-as.numeric(valores_x2)
    ejeX_x1<-0
    datos_eje_x<-data.frame(x_val=ejeX_x1,y_val=ejeX_x2,tipo="eje_x")
    
    #eje y
    ejeY_x2<-0
    ejeY_x1<-seq(min(datos_totales$x_val)+10,max(datos_totales$x_val)+10,1)
    datos_eje_y<-data.frame(x_val=ejeY_x1,y_val=ejeY_x2,tipo="eje_y")
    
    datos_eje<-rbind(datos_eje_x,datos_eje_y)
    library(ggplot2)
    plot<-ggplot()+geom_line(data=datos_totales,aes(x=x_val,y=y_val,group=tipo,colour=tipo))+geom_line(data=datos_eje,aes(x=x_val,y=y_val,group=tipo))
    plot
  })
  
})
  