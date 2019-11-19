shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadPreRNA', 'Download'),
        br(),
        selectInput('pRUNr', 'Run ID:',
          choices=flowrna,
            selected=flowrna,
              multiple=TRUE
       ),
        selectInput('prGROUP', '1st Sort:',
          pcolchoiR,
            selected="project"
       ),
        selectInput('prSECONDARY', '2nd Sort:',
          psecondaR,
            selected="total_reads"
       ),
        selectInput('prCOLR', 'Colour by:',
          choices=pcolchoiR,
            selected="run"
       ),
        selectInput('prSHAPE', 'Shape by:',
          choices=pcolchoiR,
            selected="project"
       ),
        textInput(inputId="prSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('prnames', 'Show Names:',
          choices=namechoPR,
            selected="none"
       ),
        sliderInput("prRpsp", "Reads Per Start Point:", min=0, max=50, value = 5),
        sliderInput("prRrna", "Ribosomal RNA Contamination:", min=0, max=100, value = 50),
        sliderInput("prDen", "Passed Filter Reads:", min=0, max=0.5, value = 0.01),
        width=4
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
			plotOutput("ptotalReadsPlotRNA", height="30vh")
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
			plotOutput("pPFUniqReadsRNAPlot", height="30vh")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
			plotOutput("pRSPRNAPlot", height="30vh")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
			plotOutput("pFiveThreeRNAPlot", height="30vh")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
			plotOutput("pPctCorrRSRNAPlot", height="30vh")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
			plotOutput("pPctCodingRNAPlot", height="30vh")
		),
    ### Plot 6 End ###
    ### Plot 7 Begin ###
		fluidRow(
			plotOutput("pPctRiboRNAPlot", height="30vh")
		),
    ### Plot 7 End ###
    fluidRow(
      verbatimTextOutput("preRNASeqFail")
    ),
    fluidRow(
      DT::dataTableOutput("prerna_table")
    ),
	width=8
   )
  )
 )
)