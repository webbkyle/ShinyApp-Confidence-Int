library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram

createCI<-function(M,m,MU,SIG,err){
  CI = matrix(0,M,2)
  Xbar = color = c()
  
  for(j in 1:M){
    samp = rnorm(m, MU, SIG)
    xbar = mean(samp)
    sighat = sqrt(t(samp-xbar)%*%(samp-xbar)/(m-1))
    Xbar = append(Xbar, xbar)
    t_low = qt(1-err/2, m-1)*(sighat/sqrt(m))
    t_high = qt(1-err/2, m-1)*(sighat/sqrt(m))
    CI[j,] = c(xbar-t_low, xbar+t_high)
    color = append(color, ifelse(MU < CI[j,1] | MU > CI[j,2], 
                   'red', 'dodgerblue'))
  }
  dat = data.frame("Xbar" = Xbar, "CI_low" = CI[,1], "CI_high" = CI[,2], "color" = color)
  return(dat)
}


shinyServer(function(input, output) {

ci <- eventReactive(input$button,{
  createCI(input$N, input$n, input$trumu, input$trusig, input$truerr)
})

observeEvent(input$button,{
  plot_ci <- reactiveValues(N = input$N, n = input$n,
                            trumu = input$trumu, trusig = input$trusig, truerr = input$truerr)
  ci = createCI(plot_ci$N, plot_ci$n, plot_ci$trumu, plot_ci$trusig, plot_ci$truerr)
  ci_ind = as.character((1-plot_ci$truerr)*100)
  mean_ind = as.character(plot_ci$trumu)
  sd_ind = as.character(plot_ci$trusig)
  sub_ind = as.character(plot_ci$n)
  
  output$Plot1 <- renderPlot({
    df = data.frame(Sample = 1:plot_ci$N, Mean = ci[,1])
    ggplot(df, aes(x=Sample, y=Mean)) +
      ggtitle(paste0(ci_ind,"% CI for Subsamples of size ",sub_ind," ~N(",mean_ind,",",sd_ind,")")) + 
      xlab("Sample Number") +
      theme(
        plot.title = element_text(size=20),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size=14)
      ) +
      geom_errorbar(aes(ymin=ci[,2], ymax=ci[,3]), width=.1) +
      geom_point(color = ci$color) +
      geom_hline(yintercept=input$trumu, color = 'firebrick2')
  })
  
  output$Error <- renderText(paste0('Empirical Error: ',round({sum(!(ci()[,2]<plot_ci$trumu & ci()[,3]>plot_ci$trumu))/plot_ci$N},4)))
})
  
})
