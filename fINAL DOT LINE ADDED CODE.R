setwd("E:/Bikas Tenesse Tech University")
require(readr)
library(reader)
Biku<-read_delim("E:/Bikas Tenesse Tech University/Reduced Test Data.gff", delim = "\t", col_names = FALSE)
str(Biku)
colnames(Biku)
library(circlize)

circos_data <- Biku[, c("X1","X3", "X4", "X5")]
circos_data
CPD<- data.frame(
  chr = circos_data$X1,
  X3=circos_data$X3,
  start = circos_data$X4,  
  end = circos_data$X5
)
head(CPD)
library(circlize)
circos.par("track.height" = 3)
library(circlize)

# Assuming CPD$chr contains the sector names
circos.clear()  
# Example specific data points that cauess issues
print(CPD[CPD$chr == 'Chr4' & CPD$track == 1, c('chr', 'start', 'end')])
print(CPD[CPD$chr == 'Chr5' & CPD$track == 1, c('chr', 'start', 'end')])
summary(CPD)
#if you want to disply only the points 
xlim = c(min(CPD$start), max(CPD$end) + 1000)
circos.initialize(CPD$chr, xlim = xlim)
circos.track(ylim = c(0, 2000000000), panel.fun = function(start, end) {
  circos.text(CELL_META$xcenter, CELL_META$cell.ylim[50] + mm_y(150), CELL_META$sector.index)
  circos.axis(labels.cex = 0.6)
}, track.height = 0.1)
col = rep(c("#FF0000", "#00FF00"), 200)
circos.trackPoints(CPD$chr, CPD$start, CPD$end, col = col, pch = 20, cex = 1.5)



###3# for the fullfillment of the element on the plot of the circos plot

# Print specific data points that shows devation on the track 
print(CPD[CPD$chr == 'Chr4' & CPD$track == 1, c('chr', 'start', 'end', 'X3')])
print(CPD[CPD$chr == 'Chr5' & CPD$track == 1, c('chr', 'start', 'end', 'X3')])
summary(CPD)
###33 to add the points and lengedn on the plots
xlim = c(min(CPD$start), max(CPD$end) + 1000)
circos.initialize(CPD$chr, xlim = xlim)
#Define a function to create legend
legend_fun <- function(col, names, title) {
  circos.legend(at = "top", 
                side = "outside", 
                seg.width = 1, 
                name = names, 
                col = col, 
                title.col = "black", 
                title.position = "flush")
}
# Create a vector of unique colors for each X3 value
unique_colors <- rainbow(length(unique(CPD$X3)))

# Map X3 values to colors
X3_colors <- setNames(unique_colors, unique(CPD$X3))

# Track 1: Add text labels within the plotting region
circos.track(ylim = c(0, 100000000), panel.fun = function(start, end) {
  circos.text(CELL_META$xcenter, CELL_META$cell.ylim[50] + mm_y(150), CELL_META$sector.index)
  circos.axis(labels.cex = 0.6)
}, track.height = 0.1)

# Track 2: Plot data points with colors based on X3
col = X3_colors[CPD$X3]
circos.trackPoints(CPD$chr, CPD$start, CPD$end, col = col, pch = 20, cex = 1.1)

# Add a legend
legend("topright", 
       legend = names(X3_colors),
       fill = unique_colors,
       title = "Associated gene on respective chromosome",
       cex = 0.55, inset = c(-0.30, -0.14))
bgcol = rep(c("blue", "orange"), 4)

circos.trackHist(CPD$chr, CPD$start, bin.size = 0.2, bg.col = bgcol, col = NA, track.height = 0.2)
