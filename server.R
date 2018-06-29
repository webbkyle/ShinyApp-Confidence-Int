library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram

createCI<-function(M,m,MU,SIG,err){
  CI = matrix(0,M,2)
  Xbar = c()
  for(j in 1:M){
    samp = rnorm(m, MU, SIG)
    xbar = mean(samp)
    sighat = sqrt(t(samp-xbar)%*%(samp-xbar)/(m-1))
    Xbar = append(Xbar, xbar)
    CI[j,] = c(xbar-qnorm(1-err/2)*(sighat/sqrt(m)), xbar+qnorm(1-err/2)*(sighat/sqrt(m)))
  }
  dat = cbind(Xbar,CI)
  return(dat)
}


shinyServer(function(input, output) {
  
plot_it <- reactiveValues(a=0)
ci <- eventReactive(input$button,{
  createCI(input$N,input$n,input$trumu,input$trusig,input$truerr)
})

observeEvent(input$button,{
  output$Plot1 <- renderPlot({
    df = data.frame(Sample = 1:input$N, Mean = ci()[,1])
    ggplot(df, aes(x=Sample, y=Mean)) +
      ggtitle('Some Text') +
      geom_errorbar(aes(ymin=ci()[,2], ymax=ci()[,3]), width=.1) +
      geom_point(color = 'dodgerblue') +
      geom_hline(yintercept=input$trumu, color = 'firebrick2')
  })
  output$Error <- renderText(paste0('Sampled Error: ',{sum(!(ci()[,2]<input$trumu & ci()[,3]>input$trumu))/input$N}))
})

})
