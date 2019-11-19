# subset function
df_subset <- reactive({
    
 # add a colours column for colouring
 d$colour <- as.character(d[, input$COLR])

 # add sortby column
 d$sortby <- as.character(d[, input$GROUP])

 # add secondary sorting column
 d$sortby2 <- as.numeric(d[, input$SECONDARY])

 # add shaping column
 d$shapeby <- as.character(d[, input$SHAPE])

 # sorting
 d$samples <- factor(d$samples, levels=d[order(d$sortby, d$sortby2, decreasing=TRUE),]$samples)

 # subset by Project
 d <- d[d$proj_name %in% input$PROJ,]

 # subset by sample type
 d <- d[d$sample_type %in% input$TYPE,]

 # subset by tissue type
 d <- d[d$tissue_type %in% input$TISS,]

})

# search function
d_highlight <- reactive({

 if (!is.null(input$SAMP)) {
  df_highlight <- df_subset()[df_subset()[,"samples"] %in% input$SAMP,]
 }

})

# failed samples function
d_fail <- reactive({

	df_fail <- list(
			failed_covg=as.character(subset(df_subset(), (MEAN_TARGET_COVERAGE < input$tCov & sample_type == "Tumour") | (MEAN_TARGET_COVERAGE < input$nCov & sample_type != "Tumour"))$samples),
			failed_dupe=as.character(subset(df_subset(), PCT_EXC_DUPE > input$tDup)$samples),
			failed_call=as.character(subset(df_subset(), (callability < input$tCal & sample_type == "Tumour"))$samples),
			failed_insr=as.character(subset(df_subset(), BAMQC_INSERTMEAN < input$tIns)$samples),
			failed_cden=as.character(subset(df_subset(), (PF_READS < input$tDen & sample_type == "Tumour") | (PF_READS < input$nDen & sample_type != "Tumour"))$samples)
		)

})

# download button
output$downloadExome <- downloadHandler(
   filename = function() {
     paste('data-exome-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(df_subset(), con)
   }
 )

# ranges function
ranges <- reactiveValues(x = NULL, y = NULL)

### Plot 1 Begin ###
output$totalReadsPlot <- renderPlot({
 windowHeight <- max(d$TOTAL_READS) * 1.125

 # set plotting data and sort
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=TOTAL_READS)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3) +
 	labs(x="all libraries", y="# Reads x 10^6") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	 scale_colour_manual(values=qc_palette) +
	 geom_hline(yintercept=input$tDen, linetype="dotted", color="red", size=0.5) +
	 geom_hline(yintercept=input$nDen, linetype="dotted", color="blue", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=TOTAL_READS), colour="red", size=5)
	}
 plot + ggtitle("Total Reads (Passed Filter)")
})
### Plot 1 end ###

### Plot 2 Begin ###
output$PFUniqReadsPlot <- renderPlot({
 windowHeight <- max(d$PCT_PF_UQ_READS) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=PCT_PF_UQ_READS)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=PCT_PF_UQ_READS), colour="red", size=5)
	}
 plot + ggtitle("Percent Uniq Reads (Passed Filter)")
})
### Plot 2 end ###

### Plot 3 Begin ###
output$MeanBaitPlot <- renderPlot({
 windowHeight <- max(d$MEAN_BAIT_COVERAGE) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=MEAN_BAIT_COVERAGE)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Mean Bait Coverage") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=MEAN_BAIT_COVERAGE), colour="red", size=5)
	}
 plot + ggtitle("Mean Bait Coverage")
})
### Plot 3 end ###

### Plot 4 Begin ###
output$MeanTargPlot <- renderPlot({
 windowHeight <- max(d$MEAN_TARGET_COVERAGE) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=MEAN_TARGET_COVERAGE)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Mean Target Coverage") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$tCov, linetype="dotted", color="red", size=0.5) +
	geom_hline(yintercept=input$nCov, linetype="dotted", color="blue", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	 plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=MEAN_TARGET_COVERAGE), colour="red", size=5)
	}
 plot + ggtitle("Mean Target Coverage")
})
### Plot 4 end ###

### Plot 5 Begin ###
output$CallabilityPlot <- renderPlot({

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=callability)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$tCal, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=callability), colour="red", size=5)
	}
 plot + ggtitle("Callability (14x/8x)")
})
### Plot 5 end ###

### Plot 6 Begin ###
output$totalMIFull <- renderPlot({

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=BAMQC_INSERTMEAN)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Mean") +
 	scale_y_continuous(limits=c(0, 500)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$tIns, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=BAMQC_INSERTMEAN), colour="red", size=5)
	}
 plot + ggtitle("Mean Insert Size")
})
### Plot 6 end ###

### Plot 7 Begin ###
output$HSLibSizePlot <- renderPlot({
 windowHeight <- max(d$HS_LIBRARY_SIZE) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=HS_LIBRARY_SIZE)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="HS Library Size") +
 	scale_y_continuous(limits=c(0,windowHeight)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=HS_LIBRARY_SIZE), colour="red", size=5)
	}
 plot + ggtitle("HS Library Size")
})
### Plot 7 end ###

### Plot 8 Begin ###
output$DupRatePlot <- renderPlot({
 windowHeight <- max(d$PCT_EXC_DUPE) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=PCT_EXC_DUPE)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$tDup, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=PCT_EXC_DUPE), colour="red", size=5)
	}
 plot + ggtitle("Duplicate Rate")
})
### Plot 8 end ###

### Plot 9 Begin ###
output$PuriPlot <- renderPlot({

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=purity)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=purity), colour="red", size=5)
	}
 plot + ggtitle("Purity")
})
### Plot 9 end ###

### Plot 10 Begin ###
output$FracOverPlot <- renderPlot({
 windowHeight <- max(d$PCT_EXC_OVERLAP) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=PCT_EXC_OVERLAP)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction") +
 	scale_y_continuous(limits=c(0, 1)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=PCT_EXC_OVERLAP), colour="red", size=5)
	}
 plot + ggtitle("Fraction Excluded due to Overlap")
})
### Plot 10 end ###

### Plot 11 Begin ###
output$ATDropPlot <- renderPlot({
 windowHeight <- max(d$AT_DROPOUT) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=AT_DROPOUT)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0, 50)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=AT_DROPOUT), colour="red", size=5)
	}
 plot + ggtitle("AT Dropout %")
})
### Plot 11 end ###

### Plot 12 Begin ###
output$GCDropPlot <- renderPlot({
 windowHeight <- max(d$GC_DROPOUT) * 1.125

 # set plotting data
 df_plot <- df_subset()

 # plot
 plot <- ggplot(df_plot, aes(x=samples, y=GC_DROPOUT)) +
 	geom_point(stat="identity", aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Percent (%)") +
 	scale_y_continuous(limits=c(0,50)) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=ranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.box="horizontal",
			 legend.position="bottom") +
	 guides(colour=guide_legend(nrow=5))

    if (input$names == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$names == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$SAMP)) {
	  plot <- plot + geom_point(data=d_highlight(), aes(x=samples, y=GC_DROPOUT), colour="red", size=5)
	}
 plot + ggtitle("GC Dropout %")
})
### Plot 12 end ###

# failed samples
output$ExomeFail <- renderPrint({
    print(d_fail())
})

# output the data table
output$exome_table <- DT::renderDataTable(DT::datatable({
	df_plot <- df_subset()
	df_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))

# observe double click and re-plot
observeEvent(input$totalReadsPlot_dbclick, {
   brush <- input$totalReadsPlot_brush
   if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
   } else {
    ranges$x <- NULL
    ranges$y <- NULL
   }
 })