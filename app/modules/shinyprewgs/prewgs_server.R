# subset function
pgf_subset <- reactive({
    
 # add a colours column for colouring
 pg$colour <- as.character(pg[, input$pgCOLR])

 # add sortby column
 pg$sortby <- as.character(pg[, input$pgGROUP])

 # add secondary sorting column
 pg$sortby2 <- as.numeric(pg[, input$pgSECONDARY])

 # add shaping column
 pg$shapeby <- as.character(pg[, input$pgSHAPE])

 # sorting
 pg$samples <- factor(pg$samples, levels=pg[order(pg$sortby, pg$sortby2, decreasing=TRUE),]$samples)

 # subset by Run
 pg <- pg[pg$run %in% input$pgRUNd,]

})

# download button
output$downloadPreWgs <- downloadHandler(
   filename = function() {
     paste('data-preexome-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(pgf_subset(), con)
   }
 )

# search function
pg_highlight <- reactive({

 if (!is.null(input$pgSAMP)) {
  pgf_highlight <- pgf_subset()[pgf_subset()[,"samples"] %in% input$pgSAMP,]
 }

})

# failed samples function
pg_fail <- reactive({

	pgf_fail <- list(
			failed_rpsp=as.character(subset(pgf_subset(), BAMQC_READSPERSTARTPOINT > input$pgRpsp)$samples),
			failed_insr=as.character(subset(pgf_subset(), BAMQC_INSERTMEAN < input$pgIns)$samples),
			failed_ptden=as.character(subset(pgf_subset(), BAMQC_TOTALREADS < input$pgDen)$samples)
		)

})

### plot 1 Begin ###
output$totalReadsPlotPreWgs <- renderPlot({
 windowHeight <- max(pg$BAMQC_TOTALREADS) * 1.75

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_TOTALREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="# Reads x 10^6") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_TOTALREADS), colour="red", size=5)
	}

 plot + ggtitle("Total Reads (Passed Filter)")
})
### Plot 1 end ###

### plot 2 Begin ###
output$totalUnPreWgs <- renderPlot({

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_UNMAPREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_UNMAPREADS), colour="red", size=5)
	}

 plot + ggtitle("Unmapped Reads (%)")
})
### Plot 2 end ###

### plot 3 Begin ###
output$totalSecPreWgs <- renderPlot({

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_SECONDARYREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_SECONDARYREADS), colour="red", size=5)
	}

 plot + ggtitle("Secondary Reads (%)")
})
### Plot 3 end ###

### plot 4 Begin ###
output$totalONTPreWgs <- renderPlot({

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_READSONTARGET)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_READSONTARGET), colour="red", size=5)
	}

 plot + ggtitle("On Target Reads (%)")
})
### Plot 4 end ###

### plot 5 Begin ###
output$totalRPSPPreWgs <- renderPlot({

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_READSPERSTARTPOINT)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction") +
 	scale_y_continuous(limits=c(1, 20)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$pgRpsp, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_READSPERSTARTPOINT), colour="red", size=5)
	}

 plot + ggtitle("Reads per Start Point (Fraction)")
})
### Plot 5 end ###

### plot 6 Begin ###
output$totalMIPreWgs <- renderPlot({

 # set plotting data and sort
 pgf_plot <- pgf_subset()

 # plot
 plot <- ggplot(pgf_plot, aes(x=samples, y=BAMQC_INSERTMEAN)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction") +
 	scale_y_continuous(limits=c(0, 400)) +
	scale_shape_manual(values=1:nlevels(as.factor(pgf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$ptIns, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="bottom")

    if (input$pgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$pgnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pgSAMP)) {
	  plot <- plot + geom_point(data=pg_highlight(), aes(x=samples, y=BAMQC_INSERTMEAN), colour="red", size=5)
	}

 plot + ggtitle("Mean Insert Size")
})
### Plot 6 end ###

# failed samples
output$preWgsSeqFail <- renderPrint({
    print(pg_fail())
})

# output the data table
output$preWgs_table <- DT::renderDataTable(DT::datatable({
	pgf_plot <- pgf_subset()
	pgf_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))