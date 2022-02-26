library(shiny)

source("/Users/bricejjarvis/Desktop/Fall2021/STAT4365/Final Project/Final Project 1/pvalCalc.R")

ui = fluidPage(
  titlePanel("P-value Calculator"),
  sidebarLayout(
    sidebarPanel(
      #for all input
      selectInput("distr",label = h3("Select Distribution"),
            choices = list("Normal Distribution"="norm","t-distribution"="tdistr",
                            "F-distribution"="Fdistr","Chi-squared distribution"="chidistr")),
      radioButtons("althypo",label = h3("Alternative Hypothesis"),
              choices = list("left-tailed"="leftT","right-tailed"="rightT","two-tailed"="twoT")),
      numericInput("tval",label = h3("Test Statistic Value"),value = 1,step = .01),
      ##conditional panels
      #norm
      conditionalPanel(
        condition = "distr == norm",
          numericInput("mean",label = h3("Mean"),value = 0,step = .1),
          numericInput("sd",label = h3("SD"),value = 1,step = .1)
      ),
      #t,F, & chi
      conditionalPanel(
        condition = "distr == tdistr | Fdistr | chidistr",
          numericInput("df1",label = h3("DF"),value = 1,step = 1)
      ),
      #F
      conditionalPanel(
        condition = "distr == Fdistr",
          numericInput("df2",label = h3("DF2"),value = 1,step = 1)),
    ),
    mainPanel(plotOutput(outputId = "distrplot"))
  )
)

server = function(input,output){
  output$distrplot = renderPlot({
  pvalCalc(distr=input$distr,alt.hypo=input$althypo,test.stat=input$tval,
           mean=input$mean,sd=input$sd,df=input$df1,df2=input$df2)
  })
}

shinyApp(ui = ui, server = server)
