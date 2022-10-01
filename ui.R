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
       ), 
        #do QC plots     
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
                 
        ), 
        tabPanel("Statistical Analysis", 
                #tableOutput("table")
                h1("Will show some analysis here!!")
        )
      ),
   #section of file and report summary
  fluidRow(
    #column 1
    column(4, 
      "DIANN Report Summary",
      numericInput("n1", label = "n", value = 1000, min = 1),
      numericInput("mean1", label = "µ", value = 0, step = 0.1),
      numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
        #column 2
    column(4, 
      "Distribution 2",
      numericInput("n2", label = "n", value = 1000, min = 1),
      numericInput("mean2", label = "µ", value = 0, step = 0.1),
      numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
    ),
        #column 3
    column(4,
      "Frequency polygon",
      numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
      sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
    )
  ),
  fluidRow(
    column(9, plotOutput("hist")),
    column(3, verbatimTextOutput("ttest"))
  ),
  
  #########
  #another section with 
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
	  plotOutput(outputId = "ggPlot"),
	  textOutput("text1")
	  #textOutput("text2")

    )
  )
  #end sidebarLayout
)