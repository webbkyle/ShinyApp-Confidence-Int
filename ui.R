# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Confidence Interval Simulation"),
  
  # Sidebar with a slider input for the number of components
  sidebarLayout(
    sidebarPanel(
      numericInput("N","Number of Samples:", value = 100),
      numericInput("n","Number of Subsamples", value = 10),
      numericInput("trumu","True Mean", value = 5),
      numericInput("trusig","True Standard Deviation", value = 1),
      numericInput("truerr", "Theoretical Error Rate", value = 0.05, min = .01, max = .5, step = .01),
      actionButton("button", "Plot")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot1"),
      br(),
      br(),
      textOutput("Error")
    )
  )
))