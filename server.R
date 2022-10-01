## server file for shiny app
# paired with ui.R and global.R 
#
#
# Define server logic required to draw a histogram ----
options(shiny.maxRequestSize=30*1024^2)
server <- function(input, output, session) {
    output$hist <- renderPlot({
            x1 <- rnorm(input$n1, input$mean1, input$sd1)
            x2 <- rnorm(input$n2, input$mean2, input$sd2)
            
            freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
        }, res = 96
    )
	
	#second component
    output$ttest <- renderText({
        x1 <- rnorm(input$n1, input$mean1, input$sd1)
        x2 <- rnorm(input$n2, input$mean2, input$sd2)
        
        t_test(x1, x2)
    })
    #second component, for input single file ---- not using now.
    output$rep_fname <- renderText({
        repfile <- input$drep_file
        ext <- tools::file_ext(repfile$datapath)

        req(repfile)
        validate(need(ext == "tsv", "Please upload a csv file"))
        #readDiannReport(repfile$datapath)
        paste0("The report file has been uploaded successfully. \n",repfile$name) 
    })
    
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "directory_select", roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  
    output$directory_name <- renderPrint({
         # x<-NULL 
        #if (!is.integer(input$directory_select)) {       
         #   parseDirPath(volumes, input$directory_select)
        #}
        #cat("the file direct is ",x,"\n")
        #cat(file.path(x,"report.tsv"))
        if(!is.integer(input$directory_select))
        {
            cat("Upload folder specified as ",parseDirPath(volumes, input$directory_select), "\n")
            if(file.exists(file.path(parseDirPath(volumes, input$directory_select),"report.tsv")))
            {   
                cat("\tReport file located\n")
                if(file.exists(file.path(parseDirPath(volumes, input$directory_select),"report.stats.tsv")))
                    cat("\tReport stats file located\n")
                else 
                    cat("\tReprot stats file DOES NOT exist\n!! Please double check......")
                #x<-read.csv(file.path(parseDirPath(volumes, input$directory_select),"report.tsv"),sep=",")
                #cat("class of ifle", class(x), " and dim;", dim(x))
            }
            else cat("\tReport file DOES NOT exist. Please double check\n")
        }
    })
    #render for summary section using file. not using now.
    output$rep_sum <- renderTable({
        repfile <- input$drep_file
        ext <- tools::file_ext(repfile$datapath)

        req(repfile)
        validate(need(ext == "tsv"|ext=="csv", "Please upload a tsv|csv file"))
        
        #read the file now. using 
        rep_df<-readDiannReport(repfile$datapath)
        rs<-c("Report File Name:", "File Size","Runs")
        cs<-c(repfile$name,repfile$size, length(unique(rep_df$Run)) )
        
        rs<-c(rs, "Protein Groups")
        cs<-c(cs, length(unique(rep_df$Protein.Group)))
        
        rs<-c(rs, "Proteins")
        cs<-c(cs, length(unique(rep_df$Protein.Ids)))
        
        rs<-c(rs, "Genes")
        cs<-c(cs, length(unique(rep_df$Genes)))
        
        data.frame(param=rs, value=cs)     
        
        }, 
        striped=T
        #bordered=T
    )
    output$rep_sum2 <- renderTable({
        if(!checkFileExist( input$directory_select,volumes,"report.tsv"))
                return()
        
        #read the file now. using 
        filename<-file.path(parseDirPath(volumes, input$directory_select),"report.tsv")
        rep_df<-readDiannReport(filename)
        rs<-c("Report File Name:", "File Size","Runs")
        cs<-c(basename(filename), fs::file_size(filename), length(unique(rep_df$Run)) )
        
        rs<-c(rs, "Protein Groups")
        cs<-c(cs, length(unique(rep_df$Protein.Group)))
        
        rs<-c(rs, "Proteins")
        cs<-c(cs, length(unique(rep_df$Protein.Ids)))
        
        rs<-c(rs, "Genes")
        cs<-c(cs, length(unique(rep_df$Genes)))
        
        data.frame(param=rs, value=cs)     
        
        }, 
        striped=T
        #bordered=T
    )
    output$QC_plot_1 <- renderPlot({
        #read data first
            if(!checkFileExist( input$directory_select,volumes,"report.stats.tsv"))
                return()
            #read file
            df_stat<-read.csv(file.path(parseDirPath(volumes, input$directory_select),"report.stats.tsv"),
                sep='\t'
            )
        #get the value the plot type from QCPlotType
            
        switch(input$QCPlotType,
            "total_quant"={
                barplot(df_stat$Total.Quantity, xlab="Run", ylab="Total Quantit", main="QC: Total Quantity")
            },
            "ms_sig"={
                op<-par(mfrow=c(1,2))
                barplot(df_stat$MS1.Signal, xlab="Run", ylab="Signal", main="QC: MS1 Signal" )
                barplot(df_stat$MS2.Signal,xlab="Run", ylab="Signal", main="QC: MS2 Signal")
                par(op)
            },
            "ms_sig_ratio"={
                plot(c(1,2),c(3,4),col=3)
            }
        )#end of switch
        
    });
    
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

