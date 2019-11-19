# subset function
rf_subset <- reactive({
    
 # add a colours column for colouring
 r$colour <- as.character(r[, input$rCOLR])

 # first sort
 r$sortby <- as.character(r[, input$rGROUP])

 # second sort
 r$sortby2 <- as.numeric(r[, input$rSECONDARY])

 # shaping
 r$shapeby <- as.character(r[, input$rSHAPE])

 # sort
 r$samples <- factor(r$samples, levels=r[order(r$sortby, r$sortby2, decreasing=TRUE),]$samples)

 # subset by Project
 r <- r[r$proj_name %in% input$rPROJ,]

 # subset by sample type
 r <- r[r$sample_type %in% input$rTYPE,]

 # subset by tissue type
 r <- r[r$tissue_type %in% input$rTISS,]

})

# download button
output$downloadRNA <- downloadHandler(
   filename = function() {
     paste('data-rna-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(rf_subset(), con)
   }
 )

# search function
r_highlight <- reactive({

 if (!is.null(input$rSAMP)) {
  rf_highlight <- rf_subset()[rf_subset()[,"samples"] %in% input$rSAMP,]
 }

})

# failed samples function
r_fail <- reactive({

	rf_fail <- list(
			failed_rpsp=as.character(subset(rf_subset(), reads_start_point > input$rRpsp)$samples),
			failed_rrna=as.character(subset(rf_subset(), rrna_contam > input$rRrna)$samples),
			failed_rden=as.character(subset(rf_subset(), total_reads < input$rDen)$samples)
		)

})

# rangesR function
rangesR <- reactiveValues(x = NULL, y = NULL)

### plot 1 Begin ###
output$totalReadsPlotRNA <- renderPlot({
 windowHeight <- max(r$total_reads) * 1.75

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=total_reads)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="# Reads (10^6)") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$rDen, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=total_reads), colour="red", size=5)
	  }
    plot + ggtitle("Total Reads (Passed Filter)")
})
### Plot 1 end ###

### plot 2 Begin ###
output$PFUniqReadsRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=pct_uniq_reads)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=pct_uniq_reads), colour="red", size=5)
	  }
 plot + ggtitle("Unique Reads (Passed Filter)")
})
### Plot 2 end ###

### plot 3 Begin ###
output$RSPRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=reads_start_point)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Ratio") +
 	scale_y_continuous(limits=c(0, 500)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$rRpsp, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=reads_start_point), colour="red", size=5)
	  }
 plot + ggtitle("Reads per Start Point")
})
### Plot 3 end ###

### plot 4 Begin ###
output$FiveThreeRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=X5to3_bias)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Ratio") +
	scale_y_continuous(limits=c(0, 50)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=X5to3_bias), colour="red", size=5)
  	}
 plot + ggtitle("5 to 3 Prime Bias")
})
### Plot 4 end ###

### plot 5 Begin ###
output$PctCorrRSRNAPlot <- renderPlot({
 windowHeight <- max(r$pct_correct_rs) * 1.75

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=pct_correct_rs)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=pct_correct_rs), colour="red", size=5)
	  }
 plot + ggtitle("% Correct Read Strand")
})
### Plot 5 end ###

### plot 6 Begin ###
output$PctCodingRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=pct_coding)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

  	if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=pct_coding), colour="red", size=5)
  	}
 plot + ggtitle("% Coding")
})
### Plot 6 end ###

### plot 7 Begin ###
output$PctRiboRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=rrna_contam)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$rRrna, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=rrna_contam), colour="red", size=5)
	  }
 plot + ggtitle("% Ribosomal RNA")
})
### Plot 7 end ###

### plot 7 Begin ###
output$DV200RNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=dv200)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="DV200") +
	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=dv200), colour="red", size=5)
    }
 plot + ggtitle("DV200")
})
### Plot 7 end ###

### plot 8 Begin ###
output$RINRNAPlot <- renderPlot({

 # set plotting data
 rf_plot <- rf_subset()

 # plot
 plot <- ggplot(rf_plot, aes(x=samples, y=rin)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="RIN") +
	scale_y_continuous(limits=c(0, 15)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=rangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="bottom")

    if (input$rnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$rnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$rSAMP)) {
	    plot <- plot + geom_point(data=r_highlight(), aes(x=samples, y=rin), colour="red", size=5)
	  }
 plot + ggtitle("RIN")
})
### Plot 8 end ###

# failed samples
output$RNASeqFail <- renderPrint({
    print(r_fail())
})

# output the data table
output$rna_table <- DT::renderDataTable(DT::datatable({
	rf_plot <- rf_subset()
	rf_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))

# observe double click and re-plot
observeEvent(input$totalReadsPlotRNA_dbclick, {
   brush <- input$totalReadsPlotRNA_brush
   if (!is.null(brush)) {
    rangesR$x <- c(brush$xmin, brush$xmax)
    rangesR$y <- c(brush$ymin, brush$ymax)
   } else {
    rangesR$x <- NULL
    rangesR$y <- NULL
   }
 })