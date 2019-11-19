shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadExome', 'Download'),
        br(),
        selectInput('PROJ', 'Project',
          choices=projcts,
            selected=projcts,
              multiple=TRUE
       ),
        checkboxGroupInput('TISS', 'Tissue',
          choices=tissues,
            selected=tissues
       ),
        checkboxGroupInput('TYPE', 'Type',
          choices=samptyp,
            selected=samptyp
       ),
        selectInput('GROUP', '1st Sort:',
          choices=colchoi,
            selected="proj_name"
       ),
        selectInput('SECONDARY', '2nd Sort:',
          choices=seconda,
            selected="TOTAL_READS"
       ),
        selectInput('COLR', 'Colour by:',
          choices=colchoi,
            selected="proj_name"
       ),
        selectInput('SHAPE', 'Shape by:',
          choices=colchoi,
            selected="tissue_type"
       ),
        textInput(inputId="SAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('names', 'Show Names:',
          choices=namecho,
            selected="none"
       ),
        checkboxInput(inputId="dispUR", label="Uniq Reads", value=TRUE),
        checkboxInput(inputId="dispBA", label="Mean Bait Cvg", value=TRUE),
        checkboxInput(inputId="dispME", label="Mean Exon Cvg", value=TRUE),
        checkboxInput(inputId="dispCA", label="Callability", value=TRUE),
        checkboxInput(inputId="dispMI", label="Mean Insert Size", value=TRUE),
        checkboxInput(inputId="dispHS", label="HS Lib Size", value=TRUE),
        checkboxInput(inputId="dispDR", label="Dupl Rate", value=TRUE),
        checkboxInput(inputId="dispPU", label="Purities", value=TRUE),
        checkboxInput(inputId="dispFO", label="Frac Overlap", value=TRUE),
        checkboxInput(inputId="dispAT", label="AT Dropout", value=TRUE),
        checkboxInput(inputId="dispGC", label="GC Dropout", value=TRUE),
        sliderInput("tCov", "Tumour Coverage:", min=0, max=500, value = 80),
        sliderInput("nCov", "Normal Coverage:", min=0, max=500, value = 30),
        sliderInput("tDup", "Duplicate Rate:", min=0, max=100, value = 50),
        sliderInput("tCal", "Callability:", min=0, max=100, value = 50),
        sliderInput("tIns", "Mean Insert Size:", min=0, max=500, value = 150),
        sliderInput("tDen", "Passed Filter Reads (T):", min=0, max=500, value = 148),
        sliderInput("nDen", "Passed Filter Reads (N):", min=0, max=500, value = 44),
        width=2
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
      plotOutput("totalReadsPlot", height="30vh", dblclick="totalReadsPlot_dbclick",
       brush=brushOpts(id="totalReadsPlot_brush", resetOnNew=TRUE)
      )
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispUR == true", plotOutput("PFUniqReadsPlot", height="30vh")),
      conditionalPanel(condition="input.dispUR == true")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispBA == true", plotOutput("MeanBaitPlot", height="30vh")),
      conditionalPanel(condition="input.dispBA == true")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispME == true", plotOutput("MeanTargPlot", height="30vh")),
      conditionalPanel(condition="input.dispME == true")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispCA == true", plotOutput("CallabilityPlot", height="30vh")),
      conditionalPanel(condition="input.dispCA == true")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispMI == true", plotOutput("totalMIFull", height="30vh")),
      conditionalPanel(condition="input.dispMI == true")
		),
    ### Plot 6 End ###
    ### Plot 7 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispHS == true", plotOutput("HSLibSizePlot", height="30vh")),
      conditionalPanel(condition="input.dispHS == true")
		),
    ### Plot 7 End ###
    ### Plot 8 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispDR == true", plotOutput("DupRatePlot", height="30vh")),
      conditionalPanel(condition="input.dispDR == true")
		),
    ### Plot 8 End ###
    ### Plot 9 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispPU == true", plotOutput("PuriPlot", height="30vh")),
      conditionalPanel(condition="input.dispPU == true")
		),
    ### Plot 9 End ###
    ### Plot 10 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispFO == true", plotOutput("FracOverPlot", height="30vh")),
      conditionalPanel(condition="input.dispFO == true")
		),
    ### Plot 10 End ###
    ### Plot 11 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispAT == true", plotOutput("ATDropPlot", height="30vh")),
      conditionalPanel(condition="input.dispAT == true")
		),
    ### Plot 11 End ###
    ### Plot 12 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispGC == true", plotOutput("GCDropPlot", height="50vh")),
      conditionalPanel(condition="input.dispGC == true")
		),
    ### Plot 12 End ###
    fluidRow(
      verbatimTextOutput("ExomeFail")
    ),
    fluidRow(
      DT::dataTableOutput("exome_table")
    ),
	width=10
   )
  )
 )
)