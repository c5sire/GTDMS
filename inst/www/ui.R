library(cloneselector)
library(shinyPlus)
library(ShinyDash)
library(leaflet)
initFbDb()



fldlabel_en = fAutocomplete(list(test = "test",cream='Cream',yellow='Yellow'))

mycols = list(
  list(dataIndex='pr_name', header='Name', locked=TRUE),
  list(dataIndex='pr_label_en', header='Label', field=fldlabel_en)#,
  #   list(dataIndex='pr_values', text='Values'),
  #   list(dataIndex='pr_default', text='Defaults'),
  #   list(dataIndex='pr_past', text='Past')
)

mydata = get.prefs()
mydata= mydata[,-c(3:5)]
mydata[,2] = "test"

sites = getSitesFull()


overviewCols = list(
  list(dataIndex='CROP', header='Crop', locked=TRUE),
  list(dataIndex='TRIALTYPE', header='Trial type', locked=TRUE),
  list(dataIndex='BEGINDATE', header='Begin date', locked=TRUE),
  list(dataIndex='ENDATE', header='End date', locked=TRUE),
  list(dataIndex='LEADER', header='Leader', locked=TRUE),
  list(dataIndex='COLLABORATORS', header='Collaborators', locked=TRUE),
  list(dataIndex='SITESHORT', header='Site short', locked=TRUE),
  list(dataIndex='COUNTRY', header='Country', locked=TRUE),
  list(dataIndex='ELEVATION', header='Elevation', locked=TRUE),
  list(dataIndex='PROJECTNAME', header='Project name', locked=TRUE),
  list(dataIndex='PROJECTEND', header='Project end', locked=TRUE)
)

overviewData = fbGetMinimalOverview()
#print(overviewData)



row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}

actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}



mySidebarPanel <- function(){
  
  sidebarPanel(
    
    
    conditionalPanel(condition="input.tabsetMenu == 'fieldbook'",
    fileInput('file1', 'Choose tab delimited or Excel File',
              accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                       'application/xlsx')),
      helpText("Reinhard")                    
    ),
    
    
    conditionalPanel(condition="input.tabsetMenu == 'preferences'",
       HTML("Set preferences"),                     
       radioButtons("mprefs","for:",
                    c("active crop"="actCrop",
                      "reference fieldbook"="actFB",
                      "active trial sites"="actSites",
                      "active templates"="actTmpls",
                      "active germplasm groups"="actGG",
                      "variable evaluations" = "varEvl")
       )
    ),
    conditionalPanel(condition="input.tabsetMenu == 'preferences' & input.mprefs=='actSites'",
      tags$hr(),
      checkboxGroupInput("actCntrs", "Select countries", getCountryList() , selected = getActiveCountries() )
    ),                     
    
    
    conditionalPanel(condition="input.tabsetMenu == 'tools'",
      HTML("Response to selection"),
      radioButtons("resSel","with:",
                   c("One location"="oneLoc",
                     "Several locations"="sevLoc",
                     "Several locations and years"="sevLocYears",
                     "Several locations in two steps"="sevLoc2")
      )
    ),
    conditionalPanel(condition="input.tabsetMenu == 'tools' && (input.resSel == 'oneLoc' | input.resSel == 'sevLoc' | input.resSel == 'sevLocYears')",
      numericInput("tplotCap","Plot capacity",200, min=100,max=10000, step=1),
      #selectInput("tplotCap","Plot capacity",c(100:10000), 200),
      numericInput("tnumSelG","Number of selected genotypes",11, min=1,max=1000, step=1),
      numericInput("tgenoVar","Genotypic variance",0.1, min=0.1,max=30, step=.1),                     
      numericInput("terroVar","Error variance",0.1, min=0.1,max=30, step=.1)
    ),
    conditionalPanel(condition="input.tabsetMenu == 'tools' && (input.resSel == 'sevLoc' | input.resSel == 'sevLocYears')",
     numericInput("tnumLocs","Number of locations",1, min=1,max=100, step=1),
     numericInput("tGxLVari","Genotype by location variance",.1, min=.1,max=30, step=.1)
    ),
    conditionalPanel(condition="input.tabsetMenu == 'tools' && input.resSel == 'sevLocYears'",
     numericInput("tnumYers","Number of years",1, min=1,max=100, step=1),
     numericInput("tGxYVari","Genotype by year variance",.1, min=.1,max=30, step=.1),
     numericInput("tGxLxYVa","Genotype by location by yearvariance",.1, min=.1,max=30, step=.1)
    ),
    conditionalPanel(condition="input.tabsetMenu == 'tools' && input.resSel == 'sevLoc2'",
     numericInput("t2NumbGeno","Number of genotypes for stage 1",100, min=3,max=100000, step=1),
     tags$hr(),
     "1st stage",
     numericInput("t21NumbLoc","Number of locations",1, min=1,max=100, step=1),
     numericInput("t21NumbRep","Number of replications",1, min=1,max=100, step=1),
     numericInput("t21NumbSlG","Number of selected genotypes",50, min=1,max=10000, step=1),
     tags$hr(),
     "2nd stage",
     numericInput("t22NumbLoc","Number of locations",1, min=1,max=100, step=1),
     numericInput("t22NumbRep","Number of replications",1, min=1,max=100, step=1),
     numericInput("t22NumbSlG","Number of selected genotypes",10, min=1,max=10000, step=1),
     tags$hr(),
     "Variance components",
     numericInput("t2GVar","G",0.1, min=.1,max=30, step=.1),
     numericInput("t2GxLVar","GxL",0.1, min=.1,max=30, step=.1),
     numericInput("t2GxY","GxY",0.1, min=.1,max=30, step=.1),
     numericInput("t2GxLxY","GxLxY",0.1, min=.1,max=30, step=.1),
     numericInput("t2Error","Error",0.1, min=.1,max=30, step=.1)
    )
    
  )
  
}
# 
# 
# minLat= round(min(sites$LATD)-2,0)
# maxLat= round(max(sites$LATD)+2,0)
# minLon= round(min(sites$LOND)-2,0)
# maxLon= round(max(sites$LOND)+2,0)
# latCen = minLat+(maxLat-minLat)/2
# lonCen = minLon+(maxLon-minLon)/2
# print(paste(minLat, minLon, maxLat, maxLon, latCen, lonCen))

tabOverview <- function(){
 bootstrapPage(
  leafletMap(
    "map", "100%", 300,
    initialTileLayer = "http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by somewhere')
    ,
    options=list(
      #center = c(latCen, lonCen),
      center = c(0,0),
      zoom = 1,
      maxBounds = list(list(-80, -165), list(90, 160))
      #maxBounds = list(list(minLat, minLon), list(maxLat, maxLon))
    )
   ),
  spreadsheetInput2("overviewdata",overviewData[,1:2], columns= overviewCols[1:2])
  #spreadsheetInput2("overviewdata",mydata, columns= mycols)
#  ,
  
#   tags$div(
#     class = "container",
#     
#     tags$p(tags$br()),
#     row(
#       col(4, tags$br()),
#       col(4, h2('Population of U.S. Cities'))
#     ),
#     row(
#       col(
#         4,
#         actionLink('randomLocation', 'Go to random location'),
#         checkboxInput('addMarkerOnClick', 'Add marker on click', FALSE)
#       ),
#       col(
#         4,
#         htmlWidgetOutput(
#           outputId = 'desc',
#           HTML(paste(
#             'The map is centered at <span id="lat"></span>, <span id="lng"></span>',
#             'with a zoom level of <span id="zoom"></span>.<br/>',
#             'Top <span id="shownCities"></span> out of <span id="totalCities"></span> visible cities are displayed.'
#           ))
#         )
#       )
#     )
#     ,
#     tags$hr(),
#     row(
#       col(
#         10,
#         selectInput('year', 'Year', c(2000:2010), 2010),
#         selectInput('maxCities', 'Maximum cities to display', choices=c(
#           5, 25, 50, 100, 200, 500, 2000, 5000, 10000, All = 100000
#         ), selected = 100)
#       ),
#       col(
#         10,
#         conditionalPanel(
#           condition = 'output.data',
#           h4('Visible cities')
#         ),
#         tableOutput('data')
#       ),
#       col(
#         10,
#         conditionalPanel(
#           condition = 'output.cityTimeSeries && output.cityTimeSeries.src',
#           h4(id='cityTimeSeriesLabel', class='shiny-text-output'),
#           plotOutput('cityTimeSeries', width='100%', height='200px')
#         ),
#         conditionalPanel(
#           condition = 'output.markers',
#           h4('Marker locations'),
#           actionLink('clearMarkers', 'Clear markers')
#         ),
#         tableOutput('markers')
#       )
#     )
#  )
  
  )
}  

tabFieldbook <- function(){
  "Fieldbook"
}

tabAnalysis <- function(){
  "Analysis"
}  
           
tabPreferences <-function(){
  
  'Prefs'
}           
           
tabTools <- function(){
  basicPage(
    plotOutput("resPlot"),
    tableOutput("resSim")
  )
}


tabHelp <-function(){
  #"Help"
  tags$head(tags$link(rel='stylesheet', type='text/css', href='gtdms.css'))
}           


shinyUI(
pageWithSidebar(
  headerPanel(title="Cloneselector 3.0", windowTitle="Cloneselector 3.0"),
  mySidebarPanel(),
  
  mainPanel(  
    tabsetPanel( 
      tabPanel("Overview",{
        tabOverview()
        #'Overview'
      }, value='overview') , 
      tabPanel("Fieldbook",{
        "y"
      }, value='fieldbook'), 
      tabPanel("Analysis",{
        tabAnalysis()  
      }, value='analysis'),
      tabPanel("Tools",{
        tabTools()
      }, value='tools'),
      tabPanel("Preferences",{
        tabPreferences()  
        #'prefs'
      }, value='preferences'),
      tabPanel("Help",{
        tabHelp()
        
      }, value = 'help')
      , 
      id = "tabsetMenu"
    
    ) #tabset

  )#main panel
  
)

)#shiny

