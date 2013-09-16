#TODO move this to the package
source("responseToSelection.R")

shinyServer(function(input, output) {
  
  data <- reactive({
    out = sample(100,10, rep=T)
    print(out)
    out
  })
  
  output$resPlot <- renderPlot({
    #if(input$resSel=="oneLoc")
#       responseSelectionSingleExp( input$tplotCap, 
#                                   input$tnumSelG, 
#                                   input$tgenoVar, 
#                                   input$terroVar )
      
#    responseSelectionSingleExp( 200, 
#                                   100, 
#                                   .1, 
#                                  .1 )
    
    hist(data())
  })
  
  output$out <- renderText({
    data()
  })
})