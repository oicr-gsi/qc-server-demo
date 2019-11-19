# subset function
pdf_subset <- reactive({
    
 # add a colours column for colouring
 pd$colour <- as.character(pd[, input$pCOLR])

 # add sortby column
 pd$sortby <- as.character(pd[, input$pGROUP])

 # add secondary sorting column
 pd$sortby2 <- as.numeric(pd[, input$pSECONDARY])

 # add shaping column
 pd$shapeby <- as.character(pd[, input$pSHAPE])

 # sorting
 pd$samples <- factor(pd$samples, levels=pd[order(pd$sortby, pd$sortby2, decreasing=TRUE),]$samples)

 # subset by Run
 pd <- pd[pd$run %in% input$pRUNd,]

})

# download button
output$downloadPreExome <- downloadHandler(
   filename = function() {
     paste('data-preexome-', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(pdf_subset(), con)
   }
 )

# search function
pd_highlight <- reactive({

 if (!is.null(input$pSAMP)) {
  pdf_highlight <- pdf_subset()[pdf_subset()[,"samples"] %in% input$pSAMP,]
 }

})

# failed samples function
pd_fail <- reactive({

	pdf_fail <- list(
			failed_rpsp=as.character(subset(pdf_subset(), BAMQC_READSPERSTARTPOINT > input$pdRpsp)$samples),
			failed_insr=as.character(subset(pdf_subset(), BAMQC_INSERTMEAN < input$ptIns)$samples),
			failed_ptden=as.character(subset(pdf_subset(), BAMQC_TOTALREADS < input$ptDen)$samples)
		)

})

### plot 1 Begin ###
output$totalReadsPlotPre <- renderPlot({
 windowHeight <- max(pd$BAMQC_TOTALREADS) * 1.75

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_TOTALREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="# Reads x 10^6") +
 	scale_y_continuous(limits=c(0, windowHeight)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_TOTALREADS), colour="red", size=5)
	}

 plot + ggtitle("Total Reads (Passed Filter)")
})
### Plot 1 end ###

### plot 2 Begin ###
output$totalUnPre <- renderPlot({

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_UNMAPREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_UNMAPREADS), colour="red", size=5)
	}

 plot + ggtitle("Unmapped Reads (%)")
})
### Plot 2 end ###

### plot 3 Begin ###
output$totalSecPre <- renderPlot({

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_SECONDARYREADS)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_SECONDARYREADS), colour="red", size=5)
	}

 plot + ggtitle("Secondary Reads (%)")
})
### Plot 3 end ###

### plot 4 Begin ###
output$totalONTPre <- renderPlot({

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_READSONTARGET)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="%") +
 	scale_y_continuous(limits=c(0, 100)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_READSONTARGET), colour="red", size=5)
	}

 plot + ggtitle("On Target Reads (%)")
})
### Plot 4 end ###

### plot 5 Begin ###
output$totalRPSPPre <- renderPlot({

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_READSPERSTARTPOINT)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction") +
 	scale_y_continuous(limits=c(1, 20)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$pdRpsp, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="none")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_READSPERSTARTPOINT), colour="red", size=5)
	}

 plot + ggtitle("Reads per Start Point (Fraction)")
})
### Plot 5 end ###

### plot 6 Begin ###
output$totalMIPre <- renderPlot({

 # set plotting data and sort
 pdf_plot <- pdf_subset()

 # plot
 plot <- ggplot(pdf_plot, aes(x=samples, y=BAMQC_INSERTMEAN)) +
 	geom_point(stat="identity" , aes(colour=factor(colour), shape=factor(shapeby)), size=3 ) +
 	labs(x="all libraries", y="Fraction") +
 	scale_y_continuous(limits=c(0, 400)) +
	scale_shape_manual(values=1:nlevels(as.factor(pdf_plot$shapeby))) +
	scale_colour_manual(values=qc_palette) +
	geom_hline(yintercept=input$ptIns, linetype="dotted", color="red", size=0.5) +
	 theme_bw() +
	 theme(axis.text.x=element_blank(),
		   axis.title.x=element_blank(),
		   panel.grid.major=element_blank(),
		   axis.ticks.x=element_blank(),
			 legend.position="bottom")

    if (input$penames == "samples") {
      plot <- plot + geom_text(aes(label=samples), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
    } else if (input$penames == "groupID") {
			plot <- plot + geom_text(aes(label=groupID), hjust=0, vjust=0, size=4, check_overlap=TRUE, angle=90)
		}

	if (!is.null(input$pSAMP)) {
	  plot <- plot + geom_point(data=pd_highlight(), aes(x=samples, y=BAMQC_INSERTMEAN), colour="red", size=5)
	}

 plot + ggtitle("Mean Insert Size")
})
### Plot 6 end ###

# failed samples
output$preExomeSeqFail <- renderPrint({
    print(pd_fail())
})

# output the data table
output$preexome_table <- DT::renderDataTable(DT::datatable({
	pdf_plot <- pdf_subset()
	pdf_plot
}, options=list(lengthMenu=c(5, 25, 50, 100), pageLength=50)))