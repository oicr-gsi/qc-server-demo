shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadRNA', 'Download'),
        br(),
        selectInput('rPROJ', 'Project',
          choices=projctsR,
            selected=projctsR,
              multiple=TRUE
       ),
        checkboxGroupInput('rTISS', 'Tissue',
          choices=tissuesR,
            selected=tissuesR
       ),
        checkboxGroupInput('rTYPE', 'Type',
          choices=samptypR,
            selected=samptypR
       ),
        selectInput('rGROUP', '1st Sort:',
          colchoiR,
            selected="proj_name"
       ),
        selectInput('rSECONDARY', '2nd Sort:',
          secondaR,
            selected="total_reads"
       ),
        selectInput('rCOLR', 'Colour by:',
          choices=colchoiR,
            selected="proj_name"
       ),
        selectInput('rSHAPE', 'Shape by:',
          choices=colchoi,
            selected="tissue_type"
       ),
        textInput(inputId="rSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('rnames', 'Show Names:',
          choices=namechoR,
            selected="none"
       ),
        checkboxInput(inputId="dispURr", label="Uniq Reads", value=TRUE),
        checkboxInput(inputId="dispRSr", label="Reads/Start", value=TRUE),
        checkboxInput(inputId="dispFTr", label="5'to 3' Bias ", value=TRUE),
        checkboxInput(inputId="dispCRr", label="Correct RS", value=TRUE),
        checkboxInput(inputId="dispPCr", label="Pct Coding", value=TRUE),
        checkboxInput(inputId="dispPRr", label="Pct rRNA", value=TRUE),
        checkboxInput(inputId="dispDVr", label="DV200", value=TRUE),
        checkboxInput(inputId="dispRIr", label="RIN", value=TRUE),
        sliderInput("rRpsp", "Reads Per Start Point:", min=0, max=500, value = 20),
        sliderInput("rRrna", "Ribosomal RNA Contamination:", min=0, max=100, value = 50),
        sliderInput("rDen", "Passed Filter Reads:", min=0, max=500, value = 160),
        width=2
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
			plotOutput("totalReadsPlotRNA", height="30vh", dblclick="totalReadsPlotRNA_dbclick",
       brush=brushOpts(id="totalReadsPlotRNA_brush", resetOnNew=TRUE)
      )
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispURr == true", plotOutput("PFUniqReadsRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispURr == true")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispRSr == true", plotOutput("RSPRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispRSr == true")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispFTr == true", plotOutput("FiveThreeRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispFTr == true")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispCRr == true", plotOutput("PctCorrRSRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispCRr == true")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispPCr == true", plotOutput("PctCodingRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispPCr == true")
		),
    ### Plot 6 End ###
    ### Plot 7 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispPRr == true", plotOutput("PctRiboRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispPRr == true")
		),
    ### Plot 7 End ###
    ### Plot 8 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispDVr == true", plotOutput("DV200RNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispDVr == true")
		),
    ### Plot 8 End ###
    ### Plot 9 Begin ###
		fluidRow(
			conditionalPanel(condition="input.dispRIr == true", plotOutput("RINRNAPlot", height="30vh")),
      conditionalPanel(condition="input.dispRIr == true")
		),
    ### Plot 9 End ###
    fluidRow(
      verbatimTextOutput("RNASeqFail")
    ),
    fluidRow(
      DT::dataTableOutput("rna_table")
    ),
	width=10
   )
  )
 )
)