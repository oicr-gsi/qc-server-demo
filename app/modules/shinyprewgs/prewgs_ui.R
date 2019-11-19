shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadPreWGS', 'Download'),
        br(),
        selectInput('pgRUNd', 'Run ID:',
          choices=flowwgs,
            selected=flowwgs,
              multiple=TRUE
       ),
        selectInput('pgGROUP', '1st Sort:',
          choices=pgcolchoi,
            selected="project"
       ),
        selectInput('pgSECONDARY', '2nd Sort:',
          choices=pgseconda,
            selected="BAMQC_TOTALREADS"
       ),
        selectInput('pgCOLR', 'Colour by:',
          choices=pgcolchoi,
            selected="project"
       ),
        selectInput('pgSHAPE', 'Shape by:',
          choices=pgcolchoi,
            selected="project"
       ),
        textInput(inputId="pgSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('pgnames', 'Show Names:',
          choices=namechoPG,
            selected="none"
       ),
        sliderInput("pgRpsp", "Reads Per Start Point:", min=0, max=20, value = 5),
        sliderInput("pgIns", "Insert Size Mean:", min=0, max=500, value = 150),
        sliderInput("pgDen", "Passed Filter Reads:", min=0, max=0.5, value = 0.01),
        width=4
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
      plotOutput("totalReadsPlotPreWgs", height="30vh")
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
      plotOutput("totalUnPreWgs", height="30vh")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
      plotOutput("totalSecPreWgs", height="30vh")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
      plotOutput("totalONTPreWgs", height="30vh")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
      plotOutput("totalRPSPPreWgs", height="30vh")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
      plotOutput("totalMIPreWgs", height="30vh")
		),
    ### Plot 6 End ###
    fluidRow(
      verbatimTextOutput("preWgsSeqFail")
    ),
    fluidRow(
      DT::dataTableOutput("preWgs_table")
    ),
	width=8
   )
  )
 )
)