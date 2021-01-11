#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggfortify)
#devtools::install_github("jacob-long/jtools")
library(jtools)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Multiple Regression and Correlation (MRC)"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("N",
                        "Sample Size:",
                        min = 10,
                        max = 500,
                        value = 100),
            sliderInput("Np",
                        "Number of Predictors:",
                        min=1,
                        max=5,
                        value=2),
            sliderInput("Mc",
                        "Average Correlation between Predictors:",
                        min=0,
                        max=1,
                        value=.1),
            sliderInput("Outls",
                        "Percent Outliers:",
                        min=0,
                        max=10,
                        value=0),
            radioButtons("dist", 
                         label = h3("Distribution of DV"),
                         choices = list("Normal" = 1, "Binary" = 2, "Random" = 3),
                         selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("Standard Plot", plotOutput("MRCPlot")),
             tabPanel("Model Summary", verbatimTextOutput("summary")),
             tabPanel("Bivariate Plots", plotOutput("biPlots")),
             tabPanel("Diagnostic Plots", plotOutput("Diags"))
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$summary <- renderUI({
      out <- lm
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
