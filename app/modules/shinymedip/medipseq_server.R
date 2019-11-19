# subset function
md_subset <- reactive({
    
    # add a colours column for colouring
    md$colour <- as.character(md[, input$mdCOLR])

	# first sort
 	md$sortby <- as.character(md[, input$mdGROUP])

	# second sort
	md$sortby2 <- as.numeric(md[, input$mdSECONDARY])

	# shaping
 	md$shapeby <- as.character(md[, input$mdSHAPE])

	# float column
 	md$floatMD <- md[, input$mdFLOAT]

	# sort
	md$samples <- factor(md$samples, levels=md[order(md$sortby, md$sortby2, decreasing=TRUE),]$samples)

    # subset by Run
    md <- md[md$runid %in% input$mdRUNr,]

})

# search function
md_highlight <- reactive({

 if (!is.null(input$mdSAMP)) {
  md_highlight <- md_subset()[md_subset()[,"samples"] %in% input$mdSAMP,]
 }

})

### plot 1 Begin ###
output$covWinMD <- renderPlot({
 windowHeight <- max(md$count1) * 1.75

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot) +
 	geom_point(stat="identity", aes(x=samples, y=count1, colour=factor(colour)), size=3, shape=15) +
	geom_point(stat="identity", aes(x=samples, y=count10, colour=factor(colour)), size=3, shape=16) +
	geom_point(stat="identity", aes(x=samples, y=count50, colour=factor(colour)), size=3, shape=17) +
	geom_point(stat="identity", aes(x=samples, y=count100, colour=factor(colour)), size=3, shape=18) +
 	labs(x="all libraries", y="Log10 Coverage") +
 	scale_y_continuous(limits=c(0.0,windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=count1), colour="red", size=5)
	}
    plot + ggtitle("Log10 # Windows at 1, 10, 50, 100x")
})
### Plot 1 end ###

### plot 2 Begin ###
output$pctPFAlignMD <- renderPlot({
 
 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=PCT_PF_READS_ALIGNED)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="PCT_PF_READS_ALIGNED") +
 	scale_y_continuous(limits=c(0, 105)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=PCT_PF_READS_ALIGNED), colour="red", size=5)
	}
    plot + ggtitle("PCT_PF_READS_ALIGNED")
})
### Plot 2 end ###

### plot 3 Begin ###
output$pctDupMD <- renderPlot({

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=PERCENT_DUPLICATION)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="% Duplication") +
 	scale_y_continuous(limits=c(0, 105)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=PERCENT_DUPLICATION), colour="red", size=5)
	}
    plot + ggtitle("% Duplication")
})
### Plot 3 end ###

### plot 4 Begin ###
output$CpGrelH <- renderPlot({
 windowHeight <- max(md$enrichment.score.relH) * 1.75

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=enrichment.score.relH)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="CpG relH Enrichment") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=enrichment.score.relH), colour="red", size=5)
	}
    plot + ggtitle("CpG relH Enrichment")
})
### Plot 4 end ###

### plot 5 Begin ###
output$CpGGoGe <- renderPlot({
 windowHeight <- max(md$enrichment.score.GoGe) * 1.75

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=enrichment.score.GoGe)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="CpG GoGe Enrichment") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=enrichment.score.GoGe), colour="red", size=5)
	}
    plot + ggtitle("CpG GoGe Enrichment")
})
### Plot 5 end ###

### plot 6 Begin ###
output$ATDropMD <- renderPlot({
 windowHeight <- max(md$AT_DROPOUT) * 1.25

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=AT_DROPOUT)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="AT_DROPOUT") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=AT_DROPOUT), colour="red", size=5)
	}
    plot + ggtitle("AT DROPOUT")
})
### Plot 6 end ###

### plot 7 Begin ###
output$PCTThalia <- renderPlot({
 windowHeight <- max(md$PCT_THALIANA) * 1.25

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=PCT_THALIANA)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="PCT_THALIANA") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=PCT_THALIANA), colour="red", size=5)
	}
    plot + ggtitle("Percent Thaliana")
})
### Plot 7 end ###

### plot 8 Begin ###
output$BetaThaliana <- renderPlot({
 windowHeight <- max(md$THALIANA_BETA) * 1.25

 # set plotting data
 md_plot <- md_subset()

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=THALIANA_BETA)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="THALIANA_BETA") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=THALIANA_BETA), colour="red", size=5)
	}
    plot + ggtitle("Thaliana Beta")
})
### Plot 8 end ###

### plot 9 Begin ###
output$plotFloatMD <- renderPlot({

 # set plotting data
 md_plot <- md_subset()

 # set window height
 windowHeight <- max(md_plot$floatMD) * 1.75

 # plot
 plot <- ggplot(md_plot, aes(x=samples, y=floatMD)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3) +
 	labs(x="all libraries", y=as.character(input$mdFLOAT)) +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.box="horizontal")

    if (input$mdnames == "samples") {
    	plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$mdnames == "miso_id") {
		plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
	}

	if (!is.null(input$mdSAMP)) {
		plot <- plot + geom_point(data=md_highlight(), aes(x=samples, y=floatMD), colour="red", size=5)
	}
    plot + ggtitle(as.character(input$mdFLOAT))
})
### Plot 9 end ###