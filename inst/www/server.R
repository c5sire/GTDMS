library(cloneselector)

trialsites = getSitesFull()

bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
  eventFunc <- exprToFunction(eventExpr, env, quoted)
  
  initialized <- FALSE
  invisible(observe({
    eventVal <- eventFunc()
    if (!initialized)
      initialized <<- TRUE
    else
      isolate(callback())
  }))
}



shinyServer(function(input, output, session) {
  
#   data <- reactive({
#     out = sample(100,10, rep=T)
#     #print(out)
#     print(out)
#   })
  values <- reactiveValues(markers = NULL)
  map <- createLeafletMap(session, 'map')
  
  # The cities that are within the visible bounds of the map
  sitesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(trialsites[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(trialsites,
           LATD >= latRng[1] & LATD <= latRng[2] &
             LOND >= lngRng[1] & LOND <= lngRng[2])
  })
  
  # The top N cities (by population) that are within the visible bounds
  # of the map
  topSitesInBounds <- reactive({
    sites <- sitesInBounds()
    sites <- head(sites[order(sites["ELEV"], decreasing=FALSE),],100)
  })
  
  radiusFactor <- 4000
  observe({
    map$clearShapes()
    sites <- topSitesInBounds()
    
    if (nrow(sites) == 0)
      return()
    
    map$addCircle(
      sites$LATD,
      sites$LOND,
      sqrt(sites$ELEV) * radiusFactor / max(5, input$map_zoom)^2,
      #sites$ELEV * radiusFactor / max(5, input$map_zoom)^2,
      row.names(sites),
      list(
        weight=1.2,
        fill=TRUE,
        color='red'
      )
    )
  })
  
  getClimatePlot <-function(lat, lon){
    x = sample(1:100)
    plotname  = "www/plot.png"
    png(plotname, height = 250, width =250)
    plot(x)
    dev.off()
    return("plot.png")
  }
  
  bindEvent(input$map_shape_click, function() {
    event <- input$map_shape_click
    map$clearPopups()
    
    sites <- topSitesInBounds()
    site <- sites[row.names(sites) == event$id,]
    values$selectedSite <- site
    content <- as.character(tagList(
      
      tags$strong(paste(site$FULLN,", ", site$CNTRY),sep=""),
      tags$p(),
      tags$table(border='0',
      tags$tr(tags$td(
        sprintf("Latitude: %s ", round(site$LATD,4)),
        tags$br(),
        sprintf("Longitude: %s ", round(site$LOND,4)),
        tags$br(),
        sprintf("Altitude: %s ", site$ELEV),
        tags$br(),
        sprintf("Abbrevation: %s ", site$SHORTN),
        tags$br(),
        sprintf("Admin 4: %s ", site$ADM4),
        tags$br(),
        sprintf("Admin 3: %s ", site$ADM3),
        tags$br(),
        sprintf("Admin 2: %s ", site$ADM2),
        tags$br(),
        sprintf("Admin 1: %s ", site$ADM1),
        tags$br(),
        sprintf("CIP region: %s ", site$CREG),
        tags$br(),
        sprintf("Continent: %s ", site$CONT)
      ))
      )              
      
      
      #prettyNum(city[[popCol()]], big.mark=',')
    ))
    map$showPopup(event$lat, event$lng, content, event$id,
                  options=list(maxHeight=300, maxWidth=300))
  })
  
  output$desc <- reactive({
    if (is.null(input$map_bounds))
      return(list())
    list(
      lat = mean(c(input$map_bounds$north, input$map_bounds$south)),
      lng = mean(c(input$map_bounds$east, input$map_bounds$west)),
      zoom = input$map_zoom,
      shownSites = nrow(topSitesInBounds()),
      totalSites = nrow(sitesInBounds())
    )
  })
  
  
          
  s2sim <- reactive({   
    out = "Funcion failed."
    out = c(input$t2NumbGeno,
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
    #print(paste(out))
    #print(class(out[1]))
 
    try({
    out = responseSelection2stage( input$t2NumbGeno,
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
    })
    out
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
      
  })
  
  output$resSim <- renderTable({
    if(input$resSel=="sevLoc2") s2sim()
  })
  
  output$out <- renderText({
    data()
  })
})