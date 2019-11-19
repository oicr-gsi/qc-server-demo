shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        downloadLink('downloadsWGS', 'Download'),
        br(),
        selectInput('wgRUN', 'Run ID:',
          choices=flowWG,
            selected=flowWG,
              multiple=TRUE
       ),
        selectInput('wgGROUP', '1st Sort:',
          colchoiWG,
            selected="run"
       ),
        selectInput('wgSECONDARY', '2nd Sort:',
          secondaWG,
            selected="BAMQC_TOTALREADS"
       ),
        selectInput('wgCOLR', 'Colour by:',
          choices=colchoiWG,
            selected="run"
       ),
        selectInput('wgSHAPE', 'Shape by:',
          choices=colchoiWG,
            selected="proj_id"
       ),
        textInput(inputId="wgSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('wgnames', 'Show Names:',
          choices=namechoWG,
            selected="none"
       ),
        sliderInput("wgINSM", "Insert Mean:", min=0, max=500, value = 150),
        sliderInput("wgTFRAC", "Tumour Fraction:", min=0, max=1, value = 0.2),
        width=4
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
			plotOutput("wgtotalReads", height="30vh")
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
			plotOutput("wgMeanCoverage", height="30vh")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
			plotOutput("wgMeanInsert", height="30vh")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
			plotOutput("wgTumFrac", height="30vh")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
			plotOutput("wgPloidy", height="30vh")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
			plotOutput("wgSubFrac", height="30vh")
		),
    ### Plot 6 End ###
    ### Plot 7 Begin ###
		fluidRow(
			plotOutput("wgFracGenSub", height="30vh")
		),
    ### Plot 7 End ###
    ### Plot 8 Begin ###
		fluidRow(
			plotOutput("wgFracCNASub", height="30vh")
		),
    ### Plot 8 End ###
    fluidRow(
      verbatimTextOutput("wgsFail")
    ),
    fluidRow(
      DT::dataTableOutput("swgs_table")
    ),
	width=8
   )
  )
 )
)