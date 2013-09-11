library(cloneselector)

shinyServer(function(input, output) {
  
  data <- reactive({
    out = sample(100,10, rep=T)
    print(out)
    out
  })
  
  s2sim <- reactive({
   x = responseSelection2stage( input$t2NumbGeno,
                                input$t21NumbLoc,
                                input$t21NumbRep,
                                input$t21NumbSlG,
                                input$t22NumbLoc,
                                input$t22NumbRep,
                                input$t22NumbSlG,
                                input$t2GVar,
                                input$t2GxLVar,
                                input$t2GxY,
                                input$t2GxLxY,
                                input$t2Error)
   print(x)
   x
  })
  
  output$resPlot <- renderPlot({
    if(input$resSel=="oneLoc")
      responseSelectionSingleExp( input$tplotCap, 
                                  input$tnumSelG, 
                                  input$tgenoVar, 
                                  input$terroVar )
    
    if(input$resSel=="sevLoc")
      responseSelectionSeveralExp(input$tplotCap, 
                                  input$tnumSelG, 
                                  input$tgenoVar, 
                                  input$terroVar,
                                  input$tnumLocs,
                                  input$tGxLVari)
    if(input$resSel=="sevLocYears")
      responseSelectionSeveralExpYears(input$tplotCap, 
                                  input$tnumSelG, 
                                  input$tgenoVar, 
                                  input$terroVar,
                                  input$tnumLocs,
                                  input$tGxLVari,
                                  input$tnumYers,
                                  input$tGxYVari,
                                  input$tGxLxYVa)
    #if(input$resSel=="sevLocYears") s2plot()
      
  })
  
  output$resSim <- renderText({
    if(input$resSel=="sevLoc2") s2sim()
  })
  
  output$out <- renderText({
    data()
  })
})