# This is the ui file for t distribution
#  (C) 2024  Bernhard Klingenberg, artofstat.com
library(shiny)
library(shinyWidgets)


navbarPage(
  title=HTML("<b style='color:black;'>The t Distribution</b>"),
  #title=a(tags$b("The t Distribution"), href='http://www.artofstat.com'),
  header = tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .rounded-corners {
        border-radius: 8px;
      }
      .column1 {
        float: left;
        width: 100px;
        padding: 2px;
        margin-left: 15px;
        margin-bottom: 6px;
      }
      .column2 {
        float: left;
        width: 130px;
        margin-top: 4px;
        padding: 2px;
      }
      .image2 {
         margin-top: 5px;
      }
      .custom-hr {
      border: 0;
      border-top: 1px solid #808080;
      margin: 10px 0;
      }
      "))
  ),
  windowTitle="t Distribution",
  id="mytabs",
  tabPanel("Explore",
    sidebarLayout(
      sidebarPanel(
        helpText(HTML("The t distribution is a bell-shaped distribution centered at the value 0.
        Its shape depends on one parameter called the degrees of freedom (df).
        For large values of df, the t-distribution looks very similar to the
        standard normal distribution, but for small df values, it has wider tails.")),
        helpText(HTML("Explore the shape of the t distribution by changing the degrees of freedom with the slider below.")),
        sliderInput(inputId ="df",
                     label = "Degrees of Freedom:",
                     min = 1, value = 4, step=1, max=40
        ),
        h5(tags$b(tags$u("Options:"))),
        awesomeCheckbox(inputId="addNorm", label = "Show Standard Normal Distribution"),
        fluidRow(
          column(6, awesomeCheckbox("ownParam", HTML("Enter Value for df"))),
          column(6, downloadButton("save", "Download Graph", style = "font-size: 12px; padding: 2px 10px;"))
        ),
        conditionalPanel('input.ownParam',
          fluidRow(
            column(7, numericInputIcon("df0", "Degrees of Freedom:", value=4, step = 1, min=1))
          )
        ),
        tags$hr(class = "custom-hr"),
        #tags$br(),
        h5(tags$b("Available on mobile:")),
        div(class="row",
            div(class="column1",
                a(img(src="app-distributions.png", width = "85px", class = "rounded-corners"),
                  href='https://artofstat.com/mobile-apps',
                  target="_blank"),
            ),
            # div(class="column2",
            #     a(img(src="AppStoreLogoApple.png", width="125px"),
            #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone',
            #       target="_blank"),
            #     br(),
            #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"),
            #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata',
            #       target="_blank"
            #     ),
            # )
        ),
        tags$p(
          "More information ",
          tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
        ),
        # h5(tags$b("Check out our textbook:")),
        # a(img(src='textbookFullCover.png', width="150px"), href='http://www.artofstat.com')
      ),
      mainPanel(
        plotOutput("graph", height=400)
      )
    ) #end sidebarLayout
  ), #end first tabPanel
  tabPanel("Find Probability",
    sidebarLayout(
      sidebarPanel(
        #wellPanel(
          numericInputIcon(inputId ="df1", label = "Degrees of Freedom:",
                       min = 1, value = 4, step=1),
          h5("Choose which type of probability you want to calculate, and at what value of x:"),
          selectInput("prob", "Type of Probability:",
                      choices = list(
                        "Lower tail: P(X < x)" = "lower",
                        "Upper tail: P(X > x)" = "upper",
                        "Interval: P(x1 < X < x2)" = "int",
                        "Two-Tailed: P(X < x1) + P(X > x2)" = "two-tailed"
                      )
          ),
          fluidRow(
            column(6, 
                   conditionalPanel("input.prob == 'lower' | input.prob == 'upper'", numericInputIcon("x", "Value of x:", value=1.645, step=0.1)),
                   conditionalPanel("input.prob == 'int' | input.prob == 'two-tailed'", numericInputIcon("a", "Value of x1:", value=-1.645, step=0.1)) 
            ),
            column(6, conditionalPanel("input.prob == 'int' | input.prob == 'two-tailed'", numericInputIcon("b", "Value of x2:", value=1.645, step=0.1)))       
          ),
          fluidRow(
            column(6, awesomeCheckbox(inputId="addNorm1", label = "Standard Normal")),
            column(6, downloadButton("save1", "Download Graph", style = "font-size: 12px; padding: 2px 10px;"))
          ),
          tags$hr(class = "custom-hr"),
          #tags$br(),
          h5(tags$b("Available on mobile:")),
          div(class="row",
              div(class="column1",
                  a(img(src="app-distributions.png", width = "85px", class = "rounded-corners"),
                    href='https://artofstat.com/mobile-apps',
                    target="_blank"),
              ),
              # div(class="column2",
              #     a(img(src="AppStoreLogoApple.png", width="125px"),
              #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone',
              #       target="_blank"),
              #     br(),
              #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"),
              #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata',
              #       target="_blank"
              #     ),
              # )
          ),
          tags$p(
            "More information ",
            tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
          ),
          # h5(tags$b("Check out our textbook:")),
          # a(img(src='textbookFullCover.png', width="150px"), href='http://www.artofstat.com')
      ),
      mainPanel(
        plotOutput("graph1", height=350),
        br(),
        #maybe include box with results below graph
        conditionalPanel("input.prob=='lower'", uiOutput("caption1"), tableOutput("probtable1")),
        conditionalPanel("input.prob=='upper'", uiOutput("caption2"), tableOutput("probtable2")),
        conditionalPanel("input.prob=='int'", uiOutput("caption3"), tableOutput("probtable3")),
        conditionalPanel("input.prob=='two-tailed'", uiOutput("caption4"), tableOutput("probtable4"))
      )
    ) #end sidebarLayout
  ), #end 2nd tabpanel
  tabPanel("Find Percentile/Quantile",
    sidebarLayout(
      sidebarPanel(
         #wellPanel(
           numericInputIcon(inputId ="df2", label = "Degrees of Freedom:",
                        min = 1, value = 4, step=1),
           h5("Choose which type of percentile you want to calculate, and for which probability:"),
           selectInput("perc", "Type of Percentile:", list("Lower Tail" = "lower", "Upper Tail"="upper", "Two-Tailed" = "int")),
           conditionalPanel("input.perc == 'lower'", numericInputIcon("pl", "Probability in lower tail (in %):", value=95, min=0, max=100, step=0.5, icon = list(NULL, icon("percent")))),
           conditionalPanel("input.perc == 'int'", numericInputIcon("pm", "Central Probability (in %):", value=95, min=0, max=100, step=0.5,icon = list(NULL, icon("percent")))),
           conditionalPanel("input.perc == 'upper'", numericInputIcon("pu", "Probability in upper tail (in %):", value=5, min=0, max=100, step=0.5, icon = list(NULL, icon("percent")))),
           fluidRow(
             column(6, awesomeCheckbox(inputId="addNorm2", label = "Standard Normal")),
             column(6, downloadButton("save2", "Download Graph", style = "font-size: 12px; padding: 2px 10px;"))
           ),
           tags$hr(class = "custom-hr"),
           #tags$br(),
           h5(tags$b("Available on mobile:")),
           div(class="row",
               div(class="column1",
                   a(img(src="app-distributions.png", width = "85px", class = "rounded-corners"),
                     href='https://artofstat.com/mobile-apps',
                     target="_blank"),
               ),
               # div(class="column2",
               #     a(img(src="AppStoreLogoApple.png", width="125px"),
               #       href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757?platform=iphone',
               #       target="_blank"),
               #     br(),
               #     a(img(src="AppStoreLogoAndroid1.png",width="125px",class="image2"),
               #       href='https://play.google.com/store/apps/details?id=com.artofstat.exploredata',
               #       target="_blank"
               #     ),
               # )
           ),
           tags$p(
             "More information ",
             tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank")
           ),
           # h5(tags$b("Check out our textbook:")),
           # a(img(src='textbookFullCover.png', width="150px"), href='http://www.artofstat.com')
      ),
      mainPanel(
        plotOutput("graph2", height=350),
        br(),
        #maybe include box with results below graph
        conditionalPanel("input.perc=='lower'", uiOutput("qcaption1"), br(), tableOutput("qprobtable1")),
        conditionalPanel("input.perc=='upper'", uiOutput("qcaption2"), br(), tableOutput("qprobtable2")),
        conditionalPanel("input.perc=='int'", uiOutput("qcaption3"), br(), tableOutput("qprobtable3"))
      )
    ) #end sidebarLayout
  ) #end 3nd tabpanel
) #end navbar