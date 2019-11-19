# subset function
prf_subset <- reactive({
    
 # add a colours column for colouring
 pr$colour <- as.character(pr[, input$prCOLR])

 # first sort
 pr$sortby <- as.character(pr[, input$prGROUP])

 # second sort
 pr$sortby2 <- as.numeric(pr[, input$prSECONDARY])

 # shaping
 pr$shapeby <- as.character(pr[, input$prSHAPE])

 # sort
 pr$samples <- factor(pr$samples, levels=pr[order(pr$sortby, pr$sortby2, decreasing=TRUE),]$samples)

 # subset by Run
 pr <- pr[pr$run %in% input$pRUNr,]

})

# download button
output$downloadPreRNA <- downloadHandler(
   filename = function() {
     paste('data-prerna-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(prf_subset(), con)
   }
 )

# search function
pr_highlight <- reactive({

 if (!is.null(input$prSAMP)) {
  prf_highlight <- prf_subset()[prf_subset()[,"samples"] %in% input$prSAMP,]
 }

})

# failed samples function
pr_fail <- reactive({

	prf_fail <- list(
			failed_rpsp=as.character(subset(prf_subset(), reads_start_point > input$prRpsp)$samples),
			failed_rrna=as.character(subset(prf_subset(), rrna_contam > input$prRrna)$samples),
			failed_prden=as.character(subset(prf_subset(), total_reads < input$prDen)$samples)
		)

})

# prangesR function
prangesR <- reactiveValues(x = NULL, y = NULL)

### plot 1 Begin ###
output$ptotalReadsPlotRNA <- renderPlot({
 windowHeight <- max(pr$total_reads) * 1.75

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=total_reads)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="# Reads (10^6)") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=total_reads), colour="red", size=5)
	  }
    plot + ggtitle("Total Reads (Passed Filter)")
})
### Plot 1 end ###

### plot 2 Begin ###
output$pPFUniqReadsRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=pct_uniq_reads)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=pct_uniq_reads), colour="red", size=5)
	  }
 plot + ggtitle("Unique Reads (Passed Filter)")
})
### Plot 2 end ###

### plot 3 Begin ###
output$pRSPRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=reads_start_point)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Ratio") +
 	scale_y_continuous(limits=c(0, 10)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=reads_start_point), colour="red", size=5)
	  }
 plot + ggtitle("Reads per Start Point")
})
### Plot 3 end ###

### plot 4 Begin ###
output$pFiveThreeRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=X5to3_bias)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Ratio") +
	scale_y_continuous(limits=c(0, 50)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=X5to3_bias), colour="red", size=5)
  	}
 plot + ggtitle("5 to 3 Prime Bias")
})
### Plot 4 end ###

### plot 5 Begin ###
output$pPctCorrRSRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=pct_correct_rs)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=pct_correct_rs), colour="red", size=5)
	  }
 plot + ggtitle("% Correct Read Strand")
})
### Plot 5 end ###

### plot 6 Begin ###
output$pPctCodingRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=pct_coding)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

  	if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=pct_coding), colour="red", size=5)
  	}
 plot + ggtitle("% Coding")
})
### Plot 6 end ###

### plot 7 Begin ###
output$pPctRiboRNAPlot <- renderPlot({

 # set plotting data
 prf_plot <- prf_subset()

 # plot
 plot <- ggplot(prf_plot, aes(x=samples, y=rrna_contam)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
  scale_shape_manual(values=1:nlevels(as.factor(prf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=prangesR$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="bottom")

    if (input$prnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$prnames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$prSAMP)) {
	    plot <- plot + geom_point(data=pr_highlight(), aes(x=samples, y=rrna_contam), colour="red", size=5)
	  }
 plot + ggtitle("% Ribosomal RNA")
})
### Plot 7 end ###

# failed samples
output$preRNASeqFail <- renderPrint({
    print(pr_fail())
})

# output the data table
output$prerna_table <- DT::renderDataTable(DT::datatable({
	prf_plot <- prf_subset()
	prf_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))