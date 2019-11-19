shinyServer(function(input, output) {
    source("modules/shinyexome/exome_server.R", local=TRUE)
    source("modules/shinyrna/rnaseq_server.R", local=TRUE)
    source("modules/shinyprewgs/prewgs_server.R", local=TRUE)
    source("modules/shinymedip/medipseq_server.R", local=TRUE)
    source("modules/shinypreexome/preexome_server.R", local=TRUE)
    source("modules/shinyprerna/prerna_server.R", local=TRUE)
    source("modules/shinyswgs/swgs_server.R", local=TRUE)
})
