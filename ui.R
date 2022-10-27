###UI file , ui.R for shiny
#
#paired with server.R and global.R 
# Define UI for app that draws a histogram ----
#------------
# now it was turned into rows(sections)
#
ui <- fluidPage(
                titlePanel(
                      windowTitle = "DIANN Output Processing",
                      title = tags$head(tags$link(rel="icon", 
                                                  href="senseibio.png", 
                                                  type="image/x-icon")
                    )),
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
  sidebarLayout(
          mainPanel(
              #tableOutput("contents")
              p("Please specify the folder holding the DIANN output report ",style="text-align:right"),
              
              verbatimTextOutput("directory_name"),
              width=4
            ),
            sidebarPanel(
                shinyDirButton('directory_select', 'Select a directory', title='Select a directory'),
                #textOutput('directory_name')
              #fileInput(inputId="drep_file", label="Choose a CSV /TSV File", accept = c(".csv",".tsv"))
              #checkboxInput("header", "Header", TRUE)
            )
  ),
   
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
                                                c( "Total quanity" = "total_quant",
                                                    "MS signal" = "ms_sig",
                                                  "MS quantity signal ratio" = "ms_sig_ratio"
                                                 )),
                                    #tableOutput("data")
                                    
                                  ),
                           mainPanel(
                                        p("QC Plot:", style="text-align:center"),# width=2
                                        plotOutput("QC_plot_1")
                        )
                 )          
        ),   #end of QC
        
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