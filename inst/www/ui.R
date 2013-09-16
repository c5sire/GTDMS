library(cloneselector)
library(shinyPlus)

initFbDb()


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

tabOverview <- function(){
  "Overview"
}  

tabFieldbook <- function(){
  "Fieldbook"
}

tabAnalysis <- function(){
  "Analysis"
}  
           
tabPreferences <-function(){
    spreadsheetInput2("mydata",mydata, label="test",columns= mycols)
}           
           
tabTools <- function(){
  basicPage(
    plotOutput("resPlot"),
    tableOutput("resSim")
  )
}


tabHelp <-function(){
  "Help"
}           


shinyUI(
pageWithSidebar(
  headerPanel(title="Cloneselector 3.0", windowTitle="Cloneselector 3.0"),
  mySidebarPanel(),
  
  mainPanel(  
    tabsetPanel( 
      tabPanel("Overview",{
        tabOverview()
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

