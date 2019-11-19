shinyUI(
 fluidPage(
	sidebarLayout(
	  sidebarPanel(
        submitButton("Update"),
        br(),
        selectInput('mdRUNr', 'Run ID:',
          choices=flowcfm,
            selected=flowcfm,
              multiple=TRUE
       ),
        selectInput('mdGROUP', '1st Sort:',
          colchoiMD,
            selected="project"
       ),
        selectInput('mdSECONDARY', '2nd Sort:',
          secondaMD,
            selected="total_reads"
       ),
        selectInput('mdCOLR', 'Colour by:',
          choices=colchoiMD,
            selected="project"
       ),
        selectInput('mdSHAPE', 'Shape by:',
          choices=colchoiMD,
            selected="tissue_type"
       ),
        textInput(inputId="mdSAMP",
          label="Search Sample:",
            value=""
       ),
        selectInput('mdnames', 'Show Names:',
          choices=namechoMD,
            selected="none"
       ),
        width=3
      ),	

    mainPanel(
    ### Plot 1 Begin ###
		fluidRow(
			plotOutput("covWinMD", height="30vh")
		),
    ### Plot 1 End ###
    ### Plot 2 Begin ###
		fluidRow(
			plotOutput("pctPFAlignMD", height="30vh")
		),
    ### Plot 2 End ###
    ### Plot 3 Begin ###
		fluidRow(
			plotOutput("pctDupMD", height="30vh")
		),
    ### Plot 3 End ###
    ### Plot 4 Begin ###
		fluidRow(
			plotOutput("CpGrelH", height="30vh")
		),
    ### Plot 4 End ###
    ### Plot 5 Begin ###
		fluidRow(
			plotOutput("CpGGoGe", height="30vh")
		),
    ### Plot 5 End ###
    ### Plot 6 Begin ###
		fluidRow(
			plotOutput("ATDropMD", height="30vh")
		),
    ### Plot 6 End ###
    ### Plot 7 Begin ###
		fluidRow(
			plotOutput("PCTThalia", height="30vh")
		),
    ### Plot 7 End ###
    ### Plot 8 Begin ###
		fluidRow(
			plotOutput("BetaThaliana", height="30vh")
		),
    ### Plot 8 End ###
    ### Plot 9 Begin ###
		fluidRow(
			plotOutput("plotFloatMD", height="30vh")
		),
		fluidRow(
      selectInput('mdFLOAT', 'Change Plot:', choices=floatinMD, selected="PCT_PF_READS_ALIGNED"),
			submitButton("Update Graph")
		),
    ### Plot 9 End ###
	width=9
   )
  )
 )
)