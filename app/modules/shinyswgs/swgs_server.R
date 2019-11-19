# subset function
wgs_subset <- reactive({
    
 # add a colours column for colouring
 wgs$colour <- as.character(wgs[, input$wgCOLR])

 # first sort
 wgs$sortby <- as.character(wgs[, input$wgGROUP])

 # second sort
 wgs$sortby2 <- as.numeric(wgs[, input$wgSECONDARY])

 # shaping
 wgs$shapeby <- as.character(wgs[, input$wgSHAPE])

 # sort
 wgs$samples <- factor(wgs$samples, levels=wgs[order(wgs$sortby, wgs$sortby2, decreasing=TRUE),]$samples)

 # subset by Run
 wgs <- wgs[wgs$run %in% input$wgRUN,]

})

# download button
output$downloadsWGS <- downloadHandler(
   filename = function() {
     paste('data-swgs-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(wgs_subset(), con)
   }
 )

# search function
wg_highlight <- reactive({

 if (!is.null(input$wgSAMP)) {
  wgs_highlight <- wgs_subset()[wgs_subset()[,"samples"] %in% input$wgSAMP,]
 }

})

# failed samples function
wg_fail <- reactive({

	wgs_fail <- list(
			failed_insm=as.character(subset(wgs_subset(), MEAN_INSERT_SIZE > input$wgINSM)$samples),
			failed_tfra=as.character(subset(wgs_subset(), Tumor_Fraction < input$wgTFRAC)$samples)
		)

})

# ranges function
wgranges <- reactiveValues(x = NULL, y = NULL)

### plot 1 Begin ###
output$wgtotalReads <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=BAMQC_TOTALREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Total Reads") +
 	scale_y_continuous(limits=c(0, 10)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=BAMQC_TOTALREADS), colour="red", size=5)
	  }
    plot + ggtitle("Total Reads")
})
### Plot 1 end ###

### plot 2 Begin ###
output$wgMeanCoverage <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=MEAN_COVERAGE)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Mean Coverage") +
 	scale_y_continuous(limits=c(0, 5)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=MEAN_COVERAGE), colour="red", size=5)
	  }
 plot + ggtitle("Mean Coverage")
})
### Plot 2 end ###

### plot 3 Begin ###
output$wgMeanInsert <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=MEAN_INSERT_SIZE)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Mean Insert Size") +
 	scale_y_continuous(limits=c(0, 400)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=MEAN_INSERT_SIZE), colour="red", size=5)
	  }
 plot + ggtitle("Mean Insert Size")
})
### Plot 3 end ###

### plot 4 Begin ###
output$wgTumFrac <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=Tumor_Fraction)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Tumor Fraction") +
	scale_y_continuous(limits=c(0, 1)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=Tumor_Fraction), colour="red", size=5)
  	}
 plot + ggtitle("Tumor Fraction")
})
### Plot 4 end ###

### plot 5 Begin ###
output$wgPloidy <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=Ploidy)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Ploidy") +
 	scale_y_continuous(limits=c(0, 10)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=Ploidy), colour="red", size=5)
	  }
 plot + ggtitle("Ploidy")
})
### Plot 5 end ###

### plot 6 Begin ###
output$wgSubFrac <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=Subclone_Fraction)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Subclone Fraction") +
 	scale_y_continuous(limits=c(0, 1)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="none")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

  	if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=Subclone_Fraction), colour="red", size=5)
  	}
 plot + ggtitle("Subclone Fraction")
})
### Plot 6 end ###

### plot 7 Begin ###
output$wgFracGenSub <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=Fraction_Genome_Subclonal)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction Genome Subclonal") +
 	scale_y_continuous(limits=c(0, 1)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="bottom")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=Fraction_Genome_Subclonal), colour="red", size=5)
	  }
 plot + ggtitle("Fraction Genome Subclonal")
})
### Plot 8 end ###

output$wgFracCNASub <- renderPlot({

 # set plotting data
 wgs_plot <- wgs_subset()

 # plot
 plot <- ggplot(wgs_plot, aes(x=samples, y=Fraction_CNA_Subclonal)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction CNA Subclonal") +
 	scale_y_continuous(limits=c(0, 1)) +
  scale_shape_manual(values=1:nlevels(as.factor(wgs_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 coord_cartesian(xlim=wgranges$x) +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
		   legend.position="bottom")

    if (input$wgnames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$wgnames == "miso_id") {
			plot <- plot + geom_text(aes(label=miso_id), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	  if (!is.null(input$wgSAMP)) {
	    plot <- plot + geom_point(data=wg_highlight(), aes(x=samples, y=Fraction_CNA_Subclonal), colour="red", size=5)
	  }
 plot + ggtitle("Fraction CNA Subclonal")
})
### Plot 8 end ###

# failed samples
output$wgsFail <- renderPrint({
    print(wg_fail())
})

# output the data table
output$swgs_table <- DT::renderDataTable(DT::datatable({
	wgs_plot <- wgs_subset()
	wgs_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))