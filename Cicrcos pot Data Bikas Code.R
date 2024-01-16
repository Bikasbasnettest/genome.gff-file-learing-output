setwd("E:/Bikas Tenesse Tech University")
require(readr)
library(reader)
Biku<-read_delim("E:/Bikas Tenesse Tech University/Test_Data.gff", delim = "\t", col_names = FALSE)
str(Biku)
colnames(Biku)
require(circlize)
View(Biku)
circos_data <- Biku[, c("X1", "X4", "X5")]
circos_data
colnames(circos_data) <- c("Chromosome", "Start", "End")
circos_data <- circos_data[order(circos_data$Chromosome, circos_data$Start), ]
circos.par(gap.degree = 1e-11, track.margin = c(0, 0))
par(mar = c(1, 1, 1, 1))  
chordDiagram(circos_data, annotationTrack = "grid", preAllocateTracks = 1)
##
library(circlize)
library(readr)
library(dplyr)
colnames(Biku) <- c("chr", "source", "feature", "start", "end", "strand", "score", "frame", "attributes")
Biku$start <- as.integer(Biku$start)
Biku$end <- as.integer(Biku$end)

# ###3To select the specific elemet say (RETRO5 and Pot2)
highlighted_elements <- Biku %>% filter(feature %in% c("RETRO5", "Pot2"))

####3a basic circos plot
chordDiagram(Biku[, c("chr", "start", "end")], transparency = 0.5)

highlight(chord = TRUE, h = highlighted_elements$chr, from = highlighted_elements$start, 
          to = highlighted_elements$end, col = c("red", "blue"), track.height = 0.2)

#fo better arrangement of the outer elements 
chordDiagram(Biku, annotationTrack = "grid", preAllocateTracks = 1)
#add the labels and axis
circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
#print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
              cex=0.6)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)
