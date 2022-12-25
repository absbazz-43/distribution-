library(shiny)
if (interactive()) {
  ui <- fluidPage(
    sidebarPanel(
      selectInput("distType", "Plot Type",
                  c(gamma = "gamma", exponential = "exp",binomial="binom",weibull="weibull")
      ),
      # Only show this panel if the plot type is a histogram
      conditionalPanel(
        condition = "input.distType == 'exp'",
        sliderInput("lambda", "lambda Count", min = 1, max = 50, value = 10)
      ),
      conditionalPanel(
        condition = "input.distType == 'gamma'",
        sliderInput("alphac","alpha",min=1,max=10,value = 1),
        sliderInput("betac","beta",min=1,max=5,value = 1)
      ),
      conditionalPanel(
        condition = "input.distType == 'binom'",
        sliderInput("p","probability",min=0,max=1,value = .25),
        sliderInput("n","trial",min=1,max=200,value = 5),
        textInput("x","occurence")
      ),
      conditionalPanel(
        condition = "input.distType == 'weibull'",
        sliderInput("lambda","scaleparameter",min=1,max=10,value = .5),
        sliderInput("beta","shapeparameter",min=0,max=5,value = .25),
        textInput("x","values")
      ),
      
      mainPanel(
        plotOutput("plot")
      )
    )
  )
  
  server <- function(input, output) {
    
    output$plot <- renderPlot({
      if (input$distType == "gamma") {
        curve(1/(gamma(input$alphac)*(input$betac^input$alphac))*x^(input$alphac-1)*exp(-x/input$betac),from = -10,to=20)
      } else if(input$distType=="binom") {
        x=0:input$x
        p=input$p
        n=input$n
        plot(x,(choose(n,x)*p^x*(1-p)^(n-x)) )
      }else if(input$distType=="weibull"){
        l=input$lambda
        b=input$beta
        g=input$x
        curve((l*b*(l*x)^(b-1)*exp(-(l*x)^b)),from = 0,to=x)
      }else {
        
        curve(input$lambda*exp(-input$lambda*x),from = 0,to=20)
      }
    })
  }
  
  shinyApp(ui, server)
}