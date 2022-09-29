## server file for shiny app
# paired with ui.R and global.R 
#
#
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
	
	#second component
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    t_test(x1, x2)
  })
  
  output$distPlot <- renderPlot({
	x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    });
	
	output$ggPlot <- renderPlot({

	dft<-data.frame(x=1:5, y=10:14)
	ggplot(mtcars, aes(wt, mpg)) + geom_point(colour = "red", size = 3)
    });
	output$text1 <- renderText({paste("You have selected", a)})
	#output$text2 <- renderText({paste("the file path is: ", filepath)})
  session$onSessionEnded(stopApp)
}

