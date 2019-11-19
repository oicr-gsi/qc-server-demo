shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadPreExome', 'Download'),
        br(),
        selectInput('pRUNd', 'Run ID:',
          choices=flowdna,
            selected=flowdna,
              multiple=TRUE
       ),
        selectInput('pGROUP', '1st Sort:',
          choices=pcolchoi,
            selected="project"
       ),
        selectInput('pSECONDARY', '2nd Sort:',
          choices=pseconda,
            selected="BAMQC_TOTALREADS"
       ),
        selectInput('pCOLR', 'Colour by:',
          choices=pcolchoi,
            selected="project"
       ),
        selectInput('pSHAPE', 'Shape by:',
          choices=pcolchoi,
            selected="project"
       ),
        textInput(inputId="pSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('penames', 'Show Names:',
          choices=namechoPE,
            selected="none"
       ),
        sliderInput("pdRpsp", "Reads Per Start Point:", min=0, max=20, value = 5),
        sliderInput("ptIns", "Insert Size Mean:", min=0, max=500, value = 150),
        sliderInput("ptDen", "Passed Filter Reads:", min=0, max=0.5, value = 0.01),
        width=4
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
      plotOutput("totalReadsPlotPre", height="30vh")
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
      plotOutput("totalUnPre", height="30vh")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
      plotOutput("totalSecPre", height="30vh")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
      plotOutput("totalONTPre", height="30vh")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
      plotOutput("totalRPSPPre", height="30vh")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
      plotOutput("totalMIPre", height="30vh")
		),
    ### Plot 6 End ###
    fluidRow(
      verbatimTextOutput("preExomeSeqFail")
    ),
    fluidRow(
      DT::dataTableOutput("preexome_table")
    ),
	width=8
   )
  )
 )
)