setwd("E:/Bikas Tenesse Tech University")
require(readr)
library(reader)
library(circlize)
Biku <- read_delim("E:/Bikas Tenesse Tech University/Test_Data.gff", delim = "\t", col_names = FALSE)
circos_data <- Biku[, c("X1","X3", "X4", "X5")]
selected_elements <- c("RETRO5", "Pot2")
filtered_data <- circos_data[circos_data$X3 %in% selected_elements, ]
CPD <- data.frame(
  chr = filtered_data$X1,
  X3 = filtered_data$X3,
  start = filtered_data$X4,
  end = filtered_data$X5
)
circos.par("track.height" = 3)
# Initialize circos plot
xlim = c(min(CPD$start), max(CPD$end) + 1000)
circos.initialize(CPD$chr, xlim = xlim)
# Define a function to create legend
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
unique_colors <- c("red", "green")

X3_colors <- setNames(unique_colors, selected_elements)

circos.track(ylim = c(0, 2e22), panel.fun = function(start, end) {
  circos.text(CELL_META$xcenter, CELL_META$cell.ylim[500] + mm_y(1500), CELL_META$sector.index)
  circos.axis(labels.cex = 0.4)
}, track.height = 0.1)
col = rep(c("skyblue", "orange"), 200)
# Track 2: Plot data points with colors based on X3
col = X3_colors[CPD$X3]
circos.trackPoints(CPD$chr, CPD$start, CPD$end, col = col, pch = 20, cex = 1.1)

# Add a legend
legend("topright", 
       legend = names(X3_colors),
       fill = unique_colors,
       title = "Associated gene on respective chromosome",
       cex = 0.55, inset = c(-0.0001, -0.00001), bty = "n",
       ncol = 1)

