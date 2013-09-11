shinyUI(pageWithSidebar(
  headerPanel("Conditional Panels"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("Content Panel 1")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     selectInput(
                       "plotType", "Plot Type",
                       c(Scatter = "scatter",
                         Histogram = "hist"))
    ),
    conditionalPanel(
      condition = "input.conditionedPanels==2 && input.plotType == 'hist'",
      selectInput(
        "breaks", "Breaks",
        c("Sturges",
          "Scott",
          "Freedman-Diaconis",
          "[Custom]" = "custom")),
      
      # Only show this panel if Custom is selected
      conditionalPanel(
        condition = "input.conditionedPanels==2 && input.breaks == 'custom'",
        sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
      )
    )
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Panel 1", value=1), 
      tabPanel("Panel 2", value=2)
      , id = "conditionedPanels"
    )
  )
))