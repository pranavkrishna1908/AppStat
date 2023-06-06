library(shiny)
library(tidyverse)
library(patchwork)
m = function(x){
  return(sin(1/(x/3+0.1)))
}
m_doubleprime = function(x){
  return(((6 * x + 1.8) * cos(3/(x + 0.3)) - 9 * sin(3/(x + 0.3)))/(x + 0.3)^4)
}


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Submission for visualising the asymtotically optimal local bandwidth"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "x_val",label = "Value of x",min = 0,max = 1,value = 0.75),
      sliderInput(inputId = "samplesize",label = "Sample Size",min = 2,max = 500,value = 30),
      sliderInput(inputId = "para1",label = "Parameter 1 for Beta distribution",min = 0.1,max = 10,value = 5),
      sliderInput(inputId = "para2",label = "Parameter 2 for Beta distribution",min = 0.1,max = 10,value = 5)),
    
    # Main panel for displaying outputs ----
    mainPanel(plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    shap1 = input$para1
    shap2 = input$para2
    nsamples = input$samplesize
    x_value = input$x_val
    y_value = dbeta(x_value, shap1, shap2)
    grid = 512
    sigma = 1
    sample = rbeta(nsamples, shap1, shap2)
    d <- density(sample, n = grid, from = 0, to = 1)
    #for plotting the density
    thing = rbeta(50000, shap1, shap2)
    #for locally optimal x
    piece1 = 1/(2*sqrt(pi))
    piece2 = (m_doubleprime(x_value)**2)
    piece3 = 1/dbeta(x_value, shap1,shap2)
    #local_optimal = ((((piece1/piece2)/(m_doubleprime(x_value))**2)/m(x_value))/nsamples)**0.2
    local_optimal = (sigma*(piece1 / piece2)*piece3 / nsamples)**(0.2)
    temp1 = matrix(0, nrow = nsamples, ncol = 2)
    temp1[,1] = sample
    temp1[,2] = rep(0, nsamples)
    temp1 = as.data.frame(temp1)
    temp2 = as.data.frame(thing)
    #temp2 = as.data.frame(as.matrix(c(seq(0,1,length.out = grid), dbeta(seq(0,1,length.out = grid), shap1,shap2)), nrow = grid, byrow = F))
     plot1 = ggplot(xlab = 'X value', ylab = "Density of the Beta distribution")+
      geom_point(data = temp1, mapping = aes(x = V1, y = V2))+
      geom_density(data = temp2, mapping = aes(x = thing))+
      geom_point(aes(x = x_value, y = y_value), colour = 'blue')+
      geom_vline(xintercept = x_value - local_optimal/2, colour = 'red')+
      geom_vline(xintercept = x_value + local_optimal/2, colour = 'red')+
       xlab('Samples')+ylab('Density')+ggtitle('Plot of Samples and the corresponding Kernel Density')
    temp3 = matrix(0, nrow = grid, ncol = 2)
    temp3[,1] = seq(0,1,length.out = grid)
    temp3[,2] = m(temp3[,1])
    temp3 = as.data.frame(temp3)
    plot2 = ggplot(data = temp3)+
      geom_point(aes(x = V1, y = V2))+
      geom_point(aes(x = x_value, y = m(x_value), colour = 'red'))+ggtitle('Plot of the Function m(x)')+
      xlab('X')+ylab('Y')
    print(plot1)
    plot2
    finallplot = plot1 / plot2
    finallplot
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
