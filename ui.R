###UI file , ui.R for shiny
#
#paired with server.R and global.R 
# Define UI for app that draws a histogram ----
#------------
# now it was turned into rows(sections)
#


ui <- fluidPage(

########################################
###           section for defining styles (CSS)
##########################################
            ##1)###tag for (hr line sty;e)###3
            
            tags$head(
                tags$style(HTML("hr {border-top: 1px solid #000000;}"))
              ),
              
#############################
#     End of style definition section
##########################


            ##########################
            ##    title section                  +
            ##########################
            titlePanel(
                  windowTitle = "DIANN Output Processing",
                  title = tags$head(tags$link(rel="icon", 
                                              href="senseibio.png", 
                                              type="image/x-icon")
                )),
                    
                    
            ################################
            #             header 
            ################################
            sidebarLayout(

                # Sidebar panel for inputs ----
                sidebarPanel(
                            h3("DIANN Output Processing",style = "color:blue"),
                            "A product of ", 
                            span("Sensei Bio", style = "color:blue"),
                            p(paste0("version 1.0 ", date()))
                            
                ),

                # Main panel for displaying outputs ----
                mainPanel(
                  img(src='senseibio.png', align = "center", height=30, width=60 ),
                    #headerPanel(title="DIANN Output Processing", windowTitle = title)
                    width = 5
                )
            ),
            hr(),

            ################################
            #             file reading/directory selection 
            ################################
            sidebarLayout(
                  mainPanel(
                      #tableOutput("contents")
                      p("Please specify the folder holding the DIANN output report ",style="text-align:right"),
                      
                      width=3
                    ),
                    sidebarPanel(
                        shinyDirButton('directory_select', 'Select a directory', title='Select a directory'),
                        br(),
                        verbatimTextOutput("directory_name"),
                        #textOutput('directory_name')
                      #fileInput(inputId="drep_file", label="Choose a CSV /TSV File", accept = c(".csv",".tsv"))
                      #checkboxInput("header", "Header", TRUE)
                    )
            ),
            
          hr(),

        ################################
        #             tabs (summary, QC, PTM, Stats ) 
        ################################
        tabsetPanel(
        
        #here we show file summary about the report
        tabPanel("Run Summary",
            br(),
                sidebarLayout(
                        mainPanel(
                                        p("Project summary:", style="text-align:right"), width=4
                        
                        ),
                        sidebarPanel(
                                tableOutput("rep_sum2")
                           )
            ),
            br(),
            sidebarLayout(
                        mainPanel(
                                        p("Run summary:", style="text-align:right"), width=4
                        
                        ),
                        sidebarPanel(
                                #tableOutput("rep_sum")
                           )
            ),
       ),    #end of summary
       
       
       ####=============
       ##    do QC plots        +
       ##################
        tabPanel("QC", 
                sidebarLayout(
                        sidebarPanel(
                                    selectInput("QCPlotType", "Select QC category:",
                                                c( "Total Quanity" = "total_quant",
                                                    "MS Signal" = "ms_sig",
                                                  "MS signal ratio (MS1/MS2)" = "ms_sig_ratio"
                                                 )),
                                    #tableOutput("data")
                                    
                                  ),
                           mainPanel(
                                        p("QC Plot:", style="text-align:center"),# width=2
                                        plotOutput("QC_plot_1")
                        )
                 )          
        ),   #end of QC
        
        tabPanel("Data Filtering",
            br(),
            
            sidebarLayout(
                  sidebarPanel(
                      #tableOutput("contents")
                      p("available data sets ",style="text-align:center"),
                      uiOutput("Get_Dataset_select", style="text-align:center"),   #this is the check box group defined by the server side code based on available data
                      #verbatimTextOutput("directory_name"),
                      width=3
                    ),
                 mainPanel(
                        #section one show filter controls
                        #dropdown list of available fields
                        # and conditions
                        #     numeric
                        #     text
                        column(6,  align="right",
                            div(style="display: inline-block;vertical-align:top; text-align:left;",
                                uiOutput("Get_Field_available")
                            ),
                            div(style="display: inline-block;vertical-align:top; width: 90px;text-align:left;",
                                textInput("value_input",label="Field value","" )
                            ),
                            br(),
                            div(style="display: inline-block;vertical-align:top; width: 130px;text-align:left;",
                                uiOutput("Get_Field_condition",label="value conditions",choices=c("> (more than)",">= (not less than)", "= (equal to)","<= (not more than)","< (less than)"))
                            ),
                            div(style="display: inline-block;vertical-align:bottom; width: 130px;text-align:left;",
                            #    selectInput("character_coditions",label="search criterion",choices=c("exact match", "partial match"))
                            #),
                            
                                checkboxInput("reverse_select",label="Reverse Selection",value=FALSE)
                            ),    
                            #section2, show data tables
                            br(),
                            actionButton("data_filter_button","Generate Data")
                        
                        ) #column   
                    ) #end of main panel
            ), ##end of sidebar layout
            hr(),
            DT::dataTableOutput("Dataset_seq_table")
       ),
        ################+++++++++++++++++
        #                        Tab of PTM                                  +
        #################################
        tabPanel("PTM", 
                #tableOutput("table")
                #h1("Will show summary of PTMs!!"),
                verbatimTextOutput("PTM_sum"),
                #tableOutput("PTM_unique_table"),
                uiOutput("PTM_select"),
                DT::dataTableOutput("PTM_seq_table")
                
        ), ##<---   end of PTM
        
        
        ################+++++++++++++++++
        #                        Tab of Statistical analysis   +
        #################################
        tabPanel("Statistical Analysis", 
                #tableOutput("table")
                h1("Will show some analysis here!!"),
                
        ) ##<--- end of statistical analysis.
        
      )
   
  #end sidebarLayout
)