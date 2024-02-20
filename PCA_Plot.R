install.packages("ggplot2")
install.packages("ggfortify")
install.packages("ggrepel")
install.packages("ggforce")
library(ggplot2)
library(ggfortify)
library(ggrepel)
library(ggforce)

#Filter low expressed genes from tmm file

#Set your working directory
setwd("/your/directory/here)

#Load file with gene counts
count <- read.csv("RSEM.gene.TMM.EXPR.matrix",sep = "\t",header = T,row.names=1)


#Remove rows if count is < zero in 50% of sample
rem <- function(x){
  
  x <- as.matrix(x)
  
  x <- t(apply(x,1,as.numeric))
  
  r <- as.numeric(apply(x,1,function(i) sum(i == 0) ))
  # YOU CAN CHANGE THIS TO A HIGHER VALUE
  remove <- which(r > dim(x)[2]*0.5)
  
  return(remove)
  
}

remove <- rem(count)

countdata <- count[-remove,]

write.table(countdata, "remove100.txt")


#Store your TMM expression file in the data object, storing the first row as column headings, and first column as row names
data <- read.table("remove100.txt", head=TRUE, row.names=1)

#Transform data to proper orientation for PCA analysis
data_t <- t(data)

#Store rownames as an object 
rows <- c(rownames(data_t))

#Pca analysis
data.pca <- prcomp(data_t, center = TRUE, scale.=TRUE)

#Take a peek and get PC1 and PC2 % values for the axes -- probably a way to do this with code to save them for use as xlab / ylab

pcs <- summary(data.pca)
pcone <- sd(pcs$x[, 1])
pctwo <- sd(pcs$x[, 2])

#Enter your sample group names and how many replicates there are in each group
samples <- c(
  rep("A_1", 3), rep("A_2", 3), rep("A_3", 3),
  rep("B_1", 3), rep("B_2", 3), rep("B_1", 3),
)



#Create groups for your samples to be used in the PCA graph
A <- samples(C(1,9))
B <- samples(c(10,18))


# Convert PCA output into a data frame
data.plottable <- data.pca$x
  
)

# Plot PC1 vs PC2, coloring and shaping based on Plant_Genotype
library(ggplot2)

pdf("PCA_Plot.pdf", width = 6.5, height = 4)
ggplot(data.plottable, aes(x = PC1, y = PC2, shape = Group, color = Group)) +
  geom_point(size = 5) +
  xlab(pcone) +
  ylab(pctwo) +
  labs(title = " ") +
  theme(plot.title = element_text(size = 50, hjust = 0.5)) +
  theme_classic()
dev.off()


