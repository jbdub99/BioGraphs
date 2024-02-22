library(ggplot2)
library(tidyr)
library(ggrepel)

#Load data
setwd("")
data <- read.table("",sep = "\t",header = T, row.names=1)


#Remove rows if gene expression is less than .5 in 50% of samples
rem <- data[apply(data, MARGIN = 1, function(x) sum(x<=1)) < ncol(data)/2,]


pruned_data <- data[  row.names(data) %in% row.names(rem), ]

head(pruned_data)

dim(pruned_data)


#center columns so samples are centered on same mean
scaled1 <- scale(pruned_data)


#transform to be able to center so genes have same variance
t_scaled1 <- t(scaled1)


#center on genes
scaled2 <- scale(t_scaled1)


#capture sample names
rows <- c(rownames(scaled2)) %>% as.data.frame()


#perform principle components analysis to get values
datapca <- prcomp(scaled2, center=TRUE, scale.=TRUE)


#look at the first 2 pc values to see the % variance they explain
#Store the values of PC1 and PC2 for graph axes
summary(datapca)
pc1 <- round(summary(datapca)$importance[2,1]*100,digits=2)
pc2 <- round(summary(datapca)$importance[2,2]*100,digits=2)
pc1
pc2

#grab the first two principle components values
data <- as.data.frame(datapca$x)
data <- data[,1:2]


#separate sample names into a table with each information in a new column
test <- separate(rows, ., into=c("A","B","C"))


#combine the separated sample names with the PCA data
test1 <- cbind(test, data)

head(test1)


##This following section of splitting by Chill hours is still a work in progress
##I know I can do it dynamically instead of manually, just haven't learned yet

#separate the data into subsets based on chill hours
data_subset <- split(test1, test1$Fungi, drop=FALSE)

summary(data_subset)

#assign each subset to its own dataframe, Change 'X' to be the seperator of column names
df <- data_subset$"x" %>% as.data.frame()

#Plot the PCA plots

pdf(file = "")
ggplot(fung, aes(x=PC1, y=PC2)) +
  geom_point(aes(colour=Genotype, shape=Timepoint, size=5)) +
  #geom_text_repel(aes(label=row.names(Chill400)), size = 3, box.padding = .25, point.padding = .5, max.overlaps = 50) +
  xlab(pc1) +
  ylab(pc2) +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(size="none")
dev.off()

