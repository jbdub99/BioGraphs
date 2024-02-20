install.packages("readxl")
install.packages("gplots")
install.packages("dplyr")
install.packages("RColorBrewer")
library(readxl)
library(gplots)
library(dplyr)
library(RColorBrewer)


#Set your working directory with data here
setwd("/your/directory/here")

#Set your color scheme here
myCol <- colorRampPalette(c("dodgerblue", "black", "yellow"))(100)
myBreaks <- seq(-3, 3, length.out=101)

#Load file with data
data <- read_xlsx("your/file/here.xlsx", col_names=TRUE) %>% as.matrix()

#Specify the range of your expression data. (rows, columns)
mat <- data[1:16,c(2:4)]

#Convert matrix to a numerical matrix incase it was read as a string or character object
mat_num <- matrix(as.numeric(unlist(mat)), ncol=ncol(mat))

#Setting row names to be gene ID + gene description if available
row.names(mat_num) <- paste(data[1:nrow(mat),1], data[1:nrow(mat),2], sep="_")

#Set Column names 
colnames(mat_num) <- colnames(mat)

#Take Log2 values of expression data for easier comparison
mat_num <- signif(log2(mat_num+1), digits =4)

#Generate heatmap
pdf("heatmap.pdf", height = 12, width = 20)
heatmap.2(mat_num, key.xlab = "Value", key.xtickfun=function() {
  breaks <- parent.frame()$breaks
  return(list(
    at=parent.frame()$scale01(c(breaks[1],
                                breaks[length(breaks)])),
    labels=c(as.character(breaks[1]),
             as.character(breaks[length(breaks)]))
  ))
},lhei=c(1.5,10),colsep=1:ncol(mat_num),rowsep=1:nrow(mat_num),dendrogram="both", sepcolor="black", cexRow=.75, sepwidth=c(0.001, 0.001),trace="none", density.info="none",Colv = TRUE,Rowv = TRUE,col=brewer.pal(9,"YlOrRd"), margins = c(10,30),labRow=as.expression(lapply(rownames(mat_num), function(a) bquote(italic(.(a))))))
dev.off()

