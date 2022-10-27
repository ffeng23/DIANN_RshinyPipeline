## server file for shiny app
# paired with ui.R and global.R 
#
#
# Define server logic required to do functions----
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
    #############################----
    #global variable to be used
    #=============================
        env<-listenv();
        env$active_rep_data<-NULL;  #this is the currently activate report data 
        env$all_rep_data<-NULL; #this is the one that is read from disk without processing
        env$volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
        shinyDirChoose(input, "directory_select", roots = env$volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
        env$filename<-""
    #end of global data declaration
    
    #watch for the changes of selection folder input to 
    observe({
        input$directory_select
        if(checkFileExist( input$directory_select,env$volumes,"report.tsv")){
                        #read the file now. using
                env$filename<-file.path(parseDirPath(env$volumes, input$directory_select),"report.tsv")
                
                #cat("filename is ", filename, ";  after reading fn is ", fn)
                env$all_rep_data<-readDiannReport(env$filename)
                env$active_rep_data<-env$all_rep_data
            }
        }
    )
    
    ########################################
    #               read files.
    #--------------------------------------------------------------
    #second component, for input single file ---- not using now.
    output$rep_fname <- renderText({
        repfile <- input$drep_file
        ext <- tools::file_ext(repfile$datapath)

        req(repfile)
        validate(need(ext == "tsv", "Please upload a csv file"))
        #readDiannReport(repfile$datapath)
        paste0("The report file has been uploaded successfully. \n",repfile$name) 
    })
    
      
  
    output$directory_name <- renderPrint({
         
        if(!is.integer(input$directory_select))
        {
            cat("Upload folder specified as ",parseDirPath(env$volumes, input$directory_select), "\n")
            if(file.exists(file.path(parseDirPath(env$volumes, input$directory_select),"report.tsv")))
            {   
                cat("\tReport file located\n")
                if(file.exists(file.path(parseDirPath(env$volumes, input$directory_select),"report.stats.tsv")))
                    cat("\tReport stats file located\n")
                else 
                    cat("\tReprot stats file DOES NOT exist\n!! Please double check......")
                #x<-read.csv(file.path(parseDirPath(volumes, input$directory_select),"report.tsv"),sep=",")
                #cat("class of ifle", class(x), " and dim;", dim(x))
            }
            else cat("\tReport file DOES NOT exist. Please double check\n")
        }
    })
    
    #########################################
    #                   Run Summary section 
    #########################################
        #render for summary section using file. <----not using now.-->
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
                #cat("doing before checking.")
                input$directory_select  #<- this is here so that the renderTable is observing its changes.
                #env$active_rep_data
                if(is.null(env$active_rep_data))
                        return()
                #cat("doing good so far\n")
                rep_df<-env$active_rep_data;
                rs<-c("Report File Name:", "File Size","Runs")
                cs<-c(basename(env$filename), fs::file_size(env$filename), length(unique(rep_df$Run)) )
                
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
        );
        
    #-----------------------------------------
    #               QC tab                       +
    ##########################\
        #reading stats file and all necessary data related to state should come in here.
        output$QC_plot_1 <- renderPlot({
            #read data first
                if(!checkFileExist( input$directory_select,env$volumes,"report.stats.tsv"))
                    return()
                #read file
            df_stat<-read.csv(file.path(parseDirPath(env$volumes, input$directory_select),"report.stats.tsv"),
                    sep='\t')
			df_stat$Run<-as.factor(c(1:dim(df_stat)[1]))
            #get the value the plot type from QCPlotType
                
            switch(input$QCPlotType,
                "total_quant"={
                    #barplot(df_stat$Total.Quantity, xlab="Run", ylab="Total Quantity", main="QC: Total Quantity")
					ggplot(data=df_stat, aes(x=Run, y=Total.Quantity,fill=Run))+
							geom_bar(stat="Identity")

                },
                "ms_sig"={
                    #op<-par(mfrow=c(1,2))
					
					s1<-ggplot(data=df_stat, aes(x=Run, y=MS1.Signal,fill=Run))+
							geom_bar(stat="Identity")
					s2<-ggplot(data=df_stat, aes(x=Run, y=MS2.Signal,fill=Run))+
							geom_bar(stat="Identity")
                    #barplot(df_stat$MS1.Signal, xlab="Run", ylab="Signal", main="QC: MS1 Signal" )
                    #barplot(df_stat$MS2.Signal,xlab="Run", ylab="Signal", main="QC: MS2 Signal")
                    #par(op)
					ggarrange(s1,s2,
						labels = c("A", "B"),
						ncol = 2, nrow = 1)
                },
                "ms_sig_ratio"={
					df_stat$MS.Sig.Ratio<-df_stat$MS1.Signal/df_stat$MS2.Signal
                    ggplot(data=df_stat, aes(x=Run, y=MS.Sig.Ratio,fill=Run))+
							geom_bar(stat="Identity")
                }
            )#end of switch
            
        },width=750, height=500);
        
    #-----------------------------------------
    #               PTM tab                       +
    ##########################\
        output$PTM_seq_table <- DT::renderDataTable({
            
            input$directory_select  #<- this is here so that the renderTable is observing its changes.
            input$ptms_selected
            #check data availability
            if(is.null(env$active_rep_data))
                    return()
            #check ptm selected 
            if(length(input$ptms_selected)<1)
                return()
            #cat("length of selection,", length(input$ptms_selected),"\n")
            mseq<-env$active_rep_data$Modified.Sequence
            index=c()
            for(i in 1:length(input$ptms_selected)){
                temp<-grep(x=mseq, pattern=input$ptms_selected[i], fixed=T)
                index<-union(index,temp)
            }
                datatable(env$active_rep_data[index,c("Protein.Ids", "Run", "Modified.Sequence","Protein.Names")])
            
        });
        
        ### to print out a summary of PTMs in the sequencies
        output$PTM_sum <- renderPrint({
            input$directory_select  #<- this is here so that the renderTable is observing its changes.
            
            #read data first
            if(is.null(env$active_rep_data))
                    return()
            
                #read file
                cat("PTM analysis summary: \n")
                mseq<-env$active_rep_data$Modified.Sequence
                mseq.index<-grep(pattern="\\(.+\\)", x=mseq)
                #cat("after paste0")
                cat(paste0("\t", length(mseq.index)," peptides found with PTM(s)\n"))
                #now get unique PTMs
                mseq.ptms.index<-gregexpr(text=mseq, pattern="\\([^()]+\\)")
                num.ptms<-sum(as.numeric(lapply(mseq.ptms.index,FUN=function(x) {sum(x>0)})))
                cat("\t", num.ptms , " PTMs in total\n")
                ptms<-lapply(mseq, FUN=extract_PTMs_each)
                ptms.unlist<-unlist(ptms)
                ptms.unlist<-ptms.unlist[ptms.unlist!=""]
                ptms.unlist<-unique(ptms.unlist)
                cat("\t", length(ptms.unlist), " unique PTMs\n")
                #print(lapply(mseq.ptms.index,FUN=function(x) {sum(x>0)}))
        });
        
        output$PTM_unique_table <- renderTable({
            input$directory_select  #<- this is here so that the renderTable is observing its changes.
            #read data first
                if(is.null(env$active_rep_data))
                    return()
                mseq<-env$active_rep_data$Modified.Sequence
                
                ptms<-lapply(mseq, FUN=extract_PTMs_each)
                ptms.unlist<-unlist(ptms)
                ptms.unlist<-ptms.unlist[ptms.unlist!=""]
                ptms.unlist<-unique(ptms.unlist)
                data.frame(ptms=ptms.unlist)
        });
        
        output$PTM_select <- renderUI({
            input$directory_select  #<- this is here so that the renderTable is observing its changes.
            #read data first
                if(is.null(env$active_rep_data))
                    return()
                mseq<-env$active_rep_data$Modified.Sequence
                
                ptms<-lapply(mseq, FUN=extract_PTMs_each)
                ptms.unlist<-unlist(ptms)
                ptms.unlist<-ptms.unlist[ptms.unlist!=""]
                ptms.unlist<-unique(ptms.unlist)
                checkboxGroupInput("ptms_selected", "Choose to show modified sequences", ptms.unlist)
        })
    ###########++++++++++++++++++++++++
    #=              For testing
    #=================================
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
    
	output$text1 <- renderText({paste("You have selected", a)})
	#output$text2 <- renderText({paste("the file path is: ", filepath)})
    
    #-------------------------------------------------
    #-            end of testing section             +
    #============================-|
    
    #### +++++++++++++++++++++++++
    ##     the last command to close page
    ###############################
	
  session$onSessionEnded(stopApp)
}#end bracket

