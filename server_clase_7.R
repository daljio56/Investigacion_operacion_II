

f<-function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlotly({
    
    #########################################################################################################
    #########################################################################################################
    #########################################################################################################
    #####################
    #####################                        diapositiva angela
    #####################
    #########################################################################################################
    #########################################################################################################
    #########################################################################################################
    library(ggplot2)
    library(plotly)
    
    # datos
    datos<-c()
    x1<-c(-1:7)
    x2<-5-x1
    tipo_1<-rep("R_1",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos<-rbind(datos,tipo_temp)
    
    
    x1<-c(-1:7)
    x2<-x1
    tipo_1<-rep("R_2",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos<-rbind(datos,tipo_temp)
    
    
    x1<-c(-1:7)
    x2<-(21-6*x1)/2
    tipo_1<-rep("R_3",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos<-rbind(datos,tipo_temp)
    #vectores
    x1<-c(-1:7)
    x2<-rep(0,length(x1))
    tipo_1<-rep("V_X1",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos<-rbind(datos,tipo_temp)
    #vectores_x2
    
    x2<-c(-1:11)
    x1<-rep(0,length(x2))
    tipo_1<-rep("V_X2",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos<-rbind(datos,tipo_temp)
    puntos<-data.frame(X1=0,X2=0)
    for(k in 0:6)
    {
      for(l in 0:9)
      {
        temp_punto<-c(k,l)
        puntos<-rbind(puntos,temp_punto)
      }
    }
    
    library(ggplot2)
    ggplot()+geom_line(data=datos,aes(x=X_1,y=X_2,group=Tipo,color=Tipo))+geom_point(data=puntos,aes(x=X1,y=X2))
    
    
    
    ## Primer corte
    
    datos_gomery<-c()
    # Gomery
    x1<-c(-1:7)
    x2<-7-2*x1
    tipo_1<-rep("G_3",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos_gomery<-rbind(datos_gomery,tipo_temp)
    
    x1<-c(-1:7)
    x2<-(18-5*x1)/2
    tipo_1<-rep("G_2",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos_gomery<-rbind(datos_gomery,tipo_temp)
    
    x1<-c(-1:7)
    x2<-(10-3*x1)
    tipo_1<-rep("G_1",length(x1))
    tipo_temp<-data.frame(X_1=x1,X_2=x2,Tipo=tipo_1)
    datos_gomery<-rbind(datos_gomery,tipo_temp)
    
    p<-ggplot()+geom_line(data=datos,aes(x=X_1,y=X_2,group=Tipo,color=Tipo))+geom_line(data=datos_gomery,aes(x=X_1,y=X_2,group=Tipo,color=Tipo),linetype="dotted",alpha=input$clusters)+scale_color_manual(values=c("#e41a1c", "#377eb8","#4daf4a","#984ea3","#ff7f00","#a65628","#000000","#000000"))+geom_point(data=puntos,aes(x=X1,y=X2),colour = "blue", size = 1,alpha=0.5)
    
    ggplotly(p)

  })
  
}