#  (C) 2021  Bernhard Klingenberg bklingenberg@ncf.edu
library(shiny)
library(shinyWidgets)


navbarPage(
  title=HTML("<b style='color:black;'>The t Distribution</b>"),
  windowTitle="t Distribution",
  id="mytabs",
  tabPanel("Explore",
    sidebarLayout(
      sidebarPanel(
        helpText(HTML("The t distribution is a bell-shaped distribution centered at the value 0. For large degrees of freedom, it looks very similar to the 
                       standard normal distribution, but for small degrees of freedom, it has wider tails.")),
        helpText(HTML("Explore the shape of the t distribution by changing the degrees of freedom. Check the box to see how it compares to the standard normal distribution.")),
        #wellPanel(
          sliderInput(inputId ="df",
                     label = "Degrees of Freedom:",
                     min = 1, value = 4, step=1, max=40
          ),
          awesomeCheckbox(inputId="addNorm", label = "Show Standard Normal Curve"),
        #),
        tags$hr(),
        downloadButton("save", "Download Graph")
      ),
      mainPanel(
        plotOutput("graph")
      )
    ) #end sidebarLayout
  ), #end first tabPanel
  tabPanel("Find Probability",
    sidebarLayout(
      sidebarPanel(
        #wellPanel(
          numericInput(inputId ="df1", label = "Degrees of Freedom:",
                       min = 1, value = 4, step=1),
          awesomeCheckbox(inputId="addNorm1", label = "Show Standard Normal Curve"),
          tags$hr(),
        #),
        #wellPanel(
          selectInput("probabilities","Select Type of Probability:",
                    list("Lower Tail: P(X < x)"="bound1",
                         "Upper Tail: P(X > x)"="bound2",
                         "Two-Tailed: P(|X| > x)"="bound4",
                         "Interval: P(a < X < b)"="bound3"
                    ), selectize = TRUE
          ),
          conditionalPanel("input.probabilities != 'bound3'", 
            numericInput(inputId = "xval", label = "Specify x:", value = 1.645, step = .1)
          ),
          conditionalPanel("input.probabilities == 'bound3'", 
            fluidRow(
              column(6,numericInput(inputId = "xval1", label = "Value of a:", value = -1.645, step = .1)),
              column(6,numericInput(inputId = "xval2", label = "Value of b:", value = 1.645, step = .1))
            )
          ),
        tags$hr(),
        #)
        downloadButton("save1", "Download Graph")
      ),
      mainPanel(
        plotOutput("graph1"),
        br(),
        #maybe include box with results below graph
        conditionalPanel("input.probabilities=='bound1'", uiOutput("caption1"), br(), tableOutput("probtable1")),
        conditionalPanel("input.probabilities=='bound2'", uiOutput("caption2"), br(), tableOutput("probtable2")),
        conditionalPanel("input.probabilities=='bound3'", uiOutput("caption3"), br(), tableOutput("probtable3")),
        conditionalPanel("input.probabilities=='bound4'", uiOutput("caption4"), br(), tableOutput("probtable4"))
        
      )
    ) #end sidebarLayout
  ), #end 2nd tabpanel
  tabPanel("Find Percentile/Quantile",
    sidebarLayout(
      sidebarPanel(
         #wellPanel(
           numericInput(inputId ="df2", label = "Degrees of Freedom:",
                        min = 1, value = 4, step=1),
           awesomeCheckbox(inputId="addNorm2", label = "Show Standard Normal Curve"),
           tags$hr(),
         #),
         #wellPanel(
           selectInput("quan", "Select Type of Percentile:",
             list("Lower Tail"="bound1",
                  "Upper Tail"="bound2",
                  "Two-Tailed"="bound3"
             )
           ),
           conditionalPanel(condition = "input.quan == 'bound1'",
             numericInput("pl", "Probability in lower tail (in %):", value=95, min=0, max=100, step=0.5)),
           conditionalPanel(condition = "input.quan == 'bound2'",
             numericInput("pu", "Probability in upper tail (in %):", value=5, min=0, max=100, step=0.5)),
           conditionalPanel(condition = "input.quan == 'bound3'",
             numericInput("pm", "Central Probability (in %):", value=95, min=0, max=100, step=0.5)),
         #),
         tags$hr(),
         downloadButton("save2", "Download Graph")
       ),
       mainPanel(
          plotOutput("graph2"),
          br(),
          #maybe include box with results below graph
          conditionalPanel("input.quan=='bound1'", uiOutput("qcaption1"), br(), tableOutput("qprobtable1")),
          conditionalPanel("input.quan=='bound2'", uiOutput("qcaption2"), br(), tableOutput("qprobtable2")),
          conditionalPanel("input.quan=='bound3'", uiOutput("qcaption3"), br(), tableOutput("qprobtable3"))
       )
    ) #end sidebarLayout
  ) #end 3nd tabpanel
) #end navbar