setwd("E:/Bikas Tenesse Tech University")
require(readr)
library(reader)
Biku<-read_delim("E:/Bikas Tenesse Tech University/Reduced Test Data.gff", delim = "\t", col_names = FALSE)
str(Biku)
colnames(Biku)
require(circlize)
#View(Biku)
circos_data <- Biku[, c("X1","X3", "X4", "X5")]
circos_data
# Extract relevant columns from the data
circos_data <- Biku[, c(1,3, 4, 5)]

# Rename columns
colnames(circos_data) <- c("chr","elements", "start", "end")

# Create circos plot
chordDiagram(circos_data)
chordDiagram(circos_data, annotationTrack = "grid", preAllocateTracks = 1)
# Assuming circos_data is already defined
chordDiagram(circos_data, annotationTrack = "grid", preAllocateTracks = 1, col = c("red", "green"))
CD<-chordDiagram(circos_data, annotationTrack = "grid", preAllocateTracks = 1, col = c("red", "green"))
CD
#library(ggplot2)
#ggsave(filename = "Cord diagram without data level.jpg", plot = CD,
#       width = 35, height = 30, dpi = 1500, units = "cm")

circos.trackPlotRegion(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
#print labels 
  circos.text(mean(xlim), ylim[1] + 2.5, sector.name, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
              cex=0.6)
  circos.axis(h = "top", labels.cex = 0.1, major.tick.percentage = 0.2, 
              sector.index = sector.name, track.index = 2)
}, bg.border = NA)
##for the extraction of the graphics in high dpi
dev.copy(jpeg, 'Retro plot.jpeg', width=8, height=8, units="in", res=1500)
dev.off()
####

# Assuming circos_data is already defined
CD <- chordDiagram(circos_data, annotationTrack = "grid", preAllocateTracks = 1)

# Extract unique elements and their corresponding colors
unique_elements <- unique(circos_data$elements)
element_colors <- CD$sector.index$sector.col

# Add legend for elements
legend_title <- "Gene Anotation Legend"

# Adding legend
legend(x = "topright", legend = unique_elements, fill = element_colors, title = legend_title, cex = 0.8)

# Save the plot
dev.copy(jpeg, 'Retro_plot_with_matching_legend.jpeg', width=8, height=8, units="in", res=1500)
dev.off()
