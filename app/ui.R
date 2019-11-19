shinyUI(
    navbarPage(theme="styles.css",
        "QC Metrics Dashboard",
        tabPanel("About",
            helpText(
                h2("QC Metrics Dashboard"),
                h3("Overview"),
                "This is the landing page for interactive visualization and reporting of QC stats for OICR's Genomics program ",
                h3("Modules"),
                hr(),
                h4("EXOME"),
                "Exome Metrics",
                hr(),
                h4("RNASEQ"),
                "RNASEQ Metrics",
                hr(),
                h4("PRE-WGS"),
                "PreQC WGS Metrics"
            )
        ),
        navbarMenu("Modules",
            tabPanel("EXOME",
                source("modules/shinyexome/exome_ui.R")
            ),
            tabPanel("PRE-EXOME",
                source("modules/shinypreexome/preexome_ui.R")
            ),
            tabPanel("RNASEQ",
                source("modules/shinyrna/rnaseq_ui.R")
            ),
            tabPanel("PRE-RNA",
                source("modules/shinyprerna/prerna_ui.R")
            ),
            tabPanel("sWGS",
                source("modules/shinyswgs/swgs_ui.R")
            ),
            tabPanel("PRE-WGS",
                source("modules/shinyprewgs/prewgs_ui.R")
            ),
            tabPanel("CFMEDIP",
                source("modules/shinymedip/medipseq_ui.R")
            )
        )
    )
)
