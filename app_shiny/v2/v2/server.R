

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  varr <- reactive({
    validate(need(input$var, message = F))
    input$var
  })
  
  
  output$summa<-renderPrint( summary(x[varr()]))
  
  output$manq<-renderPrint( funModeling::df_status(x[varr()]))
  
  output$densite<-renderPlot({
    ggplot(x, aes_string(x = varr())) +
      geom_density()
  })
  
  output$bplot<-renderPlot({ 
    ggplot(x, aes_string( y = varr())) +
      geom_boxplot(outlier.colour="red")})
  
  
  output$plot2 <- renderPlot({
    p <- ggplot(x, aes_string(x=input$var1, y=input$var2)) + geom_point()
    print(p)
  })
  
  output$correlation<-renderText({
    c <- cor(x[input$var1], x[input$var2], method="pearson")
    paste ("La valeur de la corrélation de Pearson entre ces 2 variables est de ", c)
  })
  
  
  
  Prix<- eventReactive(input$go, {
    val<- data.frame (t(c(as.numeric(input$CRIM), as.numeric(input$ZN), as.numeric(input$CHAS), as.numeric(input$NOX), as.numeric(input$RM), as.numeric(input$DIS), as.numeric(input$RAD), as.numeric(input$PTRATIO), as.numeric(input$B), as.numeric(input$LSTAT))))
    colnames(val)<-c("CRIM", "ZN", "CHAS", "NOX", "RM", "DIS", "RAD", "PTRATIO", "B", "LSTAT")
    val1<-predict.lm(reg1,newdata=val)
    val1
  })
  
  output$Pred <- renderText({
    paste ("La valeur médiane des maisons se trouvant dans une ville corespondant aux caractéristiques saisies est de ", round(Prix()*1000, 0), "$")
    
  })
  
})