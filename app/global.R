options(shiny.sanitize.errors = FALSE)
library(shiny)
library(ggplot2)
library(RColorBrewer)
library(randomcoloR)
library(DT)

# color palette
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
# hard-coding in max 300 colours for now, since colourizing varies between modules
# (can colourize by run, project, sample, etc.)
n <- 300
qc_palette = distinctColorPalette(n)

# rounding function
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

# Exome data
d <- subset(read.table("data/global_data_dna.txt", header=TRUE, sep="\t"), proj_name != "NA")
exome_order <- readLines("www/column_order_exome")
d <- round_df(d[exome_order], 3)
d$proj_name <- paste0(d$proj_id, ":", d$proj_name)
d <- d[order(d$proj_name), ]
batches <- c(as.character(unique(d$batch))); names(batches) <- batches; batches <- as.list(batches)
projcts <- c(as.character(unique(d$proj_name))); names(projcts) <- projcts; projcts <- as.list(projcts)
tissues <- c(as.character(unique(d$tissue_type))); names(tissues) <- tissues; tissues <- as.list(tissues)
samptyp <- c(as.character(unique(d$sample_type))); names(samptyp) <- samptyp; samptyp <- as.list(samptyp)
seconda <- c("TOTAL_READS", "PCT_PF_UQ_READS", "MEAN_TARGET_COVERAGE", "callability", "purity", "HS_LIBRARY_SIZE", "PCT_EXC_DUPE", "PCT_EXC_OVERLAP", "AT_DROPOUT", "GC_DROPOUT")
colchoi <- c("proj_name", "centre", "sample_type", "tissue_type", "run")
namecho <- c("samples", "miso_id", "none")

# RNA-Seq data
r <- subset(read.table("data/global_data_rna.txt", header=TRUE, sep="\t"), proj_name != 9999)
rna_order <- readLines("www/column_order_rna")
r <- round_df(r[rna_order], 3)
r$proj_name <- paste0(r$proj_id, ":", r$proj_name)
r <- r[order(r$proj_name), ]
batchesR <- c(as.character(unique(r$batch))); names(batchesR) <- batchesR; batchesR <- as.list(batchesR)
projctsR <- c(as.character(unique(r$proj_name))); names(projctsR) <- projctsR; projctsR <- as.list(projctsR)
tissuesR <- c(as.character(unique(r$tissue_type))); names(tissuesR) <- tissuesR; tissuesR <- as.list(tissuesR)
samptypR <- c(as.character(unique(r$sample_type))); names(samptypR) <- samptypR; samptypR <- as.list(samptypR)
secondaR <- c("total_reads", "pct_uniq_reads", "reads_start_point", "X5to3_bias", "pct_correct_rs", "pct_coding", "rrna_contam", "dv200", "RIN")
colchoiR <- c("proj_name", "centre", "sample_type", "tissue_type", "run")
namechoR <- c("samples", "miso_id", "none")

# Pre-Exome data
pd <- subset(read.table("data/preqc_data_dna.txt", header=TRUE, sep="\t"), project != "NA")
preexome_order <- readLines("www/column_order_preexome")
pd <- round_df(pd[preexome_order], 3)
pd <- pd[order(pd$run, decreasing=TRUE), ]
pprojcts <- c(as.character(unique(pd$project))); names(pprojcts) <- pprojcts; pprojcts <- as.list(pprojcts)
flowdna <- c(as.character(unique(pd$run))); names(flowdna) <- flowdna; flowdna <- as.list(flowdna)
pseconda <- c("BAMQC_TOTALREADS", "BAMQC_INSERTMEAN", "BAMQC_INSERTSD", "BAMQC_READSPERSTARTPOINT")
pcolchoi <- c("project", "run")
namechoPE <- c("samples", "groupID", "none")

# Pre-WGS data
pg <- subset(read.table("data/preqc_data_wgs.txt", header=TRUE, sep="\t"), project != "NA")
prewgs_order <- readLines("www/column_order_prewgs")
pg <- round_df(pg[prewgs_order], 3)
pg <- pg[order(pg$run, decreasing=TRUE), ]
pgprojcts <- c(as.character(unique(pg$project))); names(pgprojcts) <- pgprojcts; pgprojcts <- as.list(pgprojcts)
flowwgs <- c(as.character(unique(pg$run))); names(flowwgs) <- flowwgs; flowwgs <- as.list(flowwgs)
pgseconda <- c("BAMQC_TOTALREADS", "BAMQC_INSERTMEAN", "BAMQC_INSERTSD", "BAMQC_READSPERSTARTPOINT")
pgcolchoi <- c("project", "run")
namechoPG <- c("samples", "groupID", "none")

# Pre-RNA data
pr <- subset(read.table("data/preqc_data_rna.txt", header=TRUE, sep="\t"), project != 9999)
prerna_order <- readLines("www/column_order_prerna")
pr <- round_df(pr[prerna_order], 3)
pr <- pr[order(pr$run, decreasing=TRUE), ]
pprojctsR <- c(as.character(unique(pr$project))); names(pprojctsR) <- pprojctsR; pprojctsR <- as.list(pprojctsR)
flowrna <- c(as.character(unique(pr$run))); names(flowrna) <- flowrna; flowrna <- as.list(flowrna)
psecondaR <- c("total_reads", "pct_uniq_reads", "reads_start_point", "X5to3_bias", "pct_correct_rs", "pct_coding", "rrna_contam")
pcolchoiR <- c("project", "run")
namechoPR <- c("samples", "groupID", "none")

# MEDIPS data
md <- subset(read.table("data/preqc_data_cfm.txt", header=TRUE, sep="\t"), project != 9999)
md$samples <- paste0(md$samples, "-", do.call(rbind, strsplit(as.character(md$runid), '_', fixed=TRUE))[,2])
medip_order <- readLines("www/column_order_cfm")
md <- round_df(md[medip_order], 3)
projctsMD <- c(as.character(unique(md$project))); names(projctsMD) <- projctsMD; projctsMD <- as.list(projctsMD)
tissuesMD <- c(as.character(unique(md$tissue_type))); names(tissuesMD) <- tissuesMD; tissuesMD <- as.list(tissuesMD)
samptypMD <- c(as.character(unique(md$sample_type))); names(samptypMD) <- samptypMD; samptypMD <- as.list(samptypMD)
flowcfm <- c(as.character(unique(md$runid))); names(flowcfm) <- flowcfm; flowcfm <- as.list(flowcfm)
secondaMD <- c("enrichment.score.relH", "enrichment.score.GoGe", "PERCENT_DUPLICATION")
colchoiMD <- c("project", "centre", "sample_type", "tissue_type", "runid")
floatinMD <- colnames(md)
namechoMD <- c("samples", "miso_id", "none")

# sWGS data
wgs <- subset(read.table("data/global_data_wgs.txt", header=TRUE, sep="\t"), proj_id != 9999)
wgs_order <- readLines("www/column_order_swgs")
wgs <- round_df(wgs[wgs_order], 3)
projctsWG <- c(as.character(unique(wgs$proj_id))); names(projctsWG) <- projctsWG; projctsWG <- as.list(projctsWG)
tissuesWG <- c(as.character(unique(wgs$tissue_type))); names(tissuesWG) <- tissuesWG; tissuesWG <- as.list(tissuesWG)
samptypWG <- c(as.character(unique(wgs$sample_type))); names(samptypWG) <- samptypWG; samptypWG <- as.list(samptypWG)
flowWG <- c(as.character(unique(wgs$run))); names(flowWG) <- flowWG; flowWG <- as.list(flowWG)
secondaWG <- c("BAMQC_TOTALREADS", "MEAN_COVERAGE", "MEAN_INSERT_SIZE", "Tumor_Fraction", "Subclone_Fraction", "Fraction_Genome_Subclonal", "Fraction_CNA_Subclonal")
colchoiWG <- c("proj_id", "centre", "sample_type", "tissue_type", "run")
floatinWG <- colnames(wgs)
namechoWG <- c("samples", "miso_id", "none")

# set some maxes
d$MEAN_TARGET_COVERAGE[d$MEAN_TARGET_COVERAGE > 300 ] <- 300
d$purity <- d$purity * 100
r$reads_start_point[r$reads_start_point > 500] <- 500
r$X5to3_bias[r$X5to3_bias > 50] <- 50
pd$BAMQC_READSPERSTARTPOINT[pd$BAMQC_READSPERSTARTPOINT > 20] <- 20
pr$reads_start_point[pr$reads_start_point > 10] <- 10
pr$X5to3_bias[pr$X5to3_bias > 50] <- 50
md$PERCENT_DUPLICATION <- md$PERCENT_DUPLICATION * 100
md$PCT_PF_READS_ALIGNED <- md$PCT_PF_READS_ALIGNED * 100
md$count1 <- log10(md$count1)
md$count10 <- log10(md$count10)
md$count50 <- log10(md$count50)
md$count100 <- log10(md$count100)
wgs$BAMQC_TOTALREADS <- wgs$BAMQC_TOTALREADS / 1000000
