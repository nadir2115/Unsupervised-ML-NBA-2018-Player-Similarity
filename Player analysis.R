#Unsupervised Machine Learning project to study NBA player similarity- Nadir Nibras

# clear workspace variables
rm(list = ls()); 
# clear window (same as ctrl+L. )
cat("\014")   
# close all plots
graphics.off() 


library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(readr)
library(datasets)
library(corrplot)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)
library(rstudioapi) # load it

# set directory to R script folder
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# load data ---------------------------------------------------------------
traditionalStats<-read.csv("36 minutes.csv")
advancedStats<- read.csv("advanced.csv")
miscStats<- read.csv("extra.csv")

# Data pre-processing -----------------------------------------------------

# Merge 1st 2 datasets
mergedData<-merge(traditionalStats,advancedStats);                      

# Fix mismatches for merging
mergedData$Player=gsub(".", "", mergedData$Player, fixed = TRUE); 
miscStats$Player=gsub(".", "", miscStats$Player, fixed = TRUE);
miscStats$Player=gsub(" Jr", "", miscStats$Player, fixed = TRUE);
miscStats$Player=gsub(" III", "", miscStats$Player, fixed = TRUE);
miscStats$Player=gsub(" IV", "", miscStats$Player, fixed = TRUE);
miscStats$Player=gsub(" II", "", miscStats$Player, fixed = TRUE);
mergedData$Player=gsub("Walt Lemon Jr", "Walter Lemon", mergedData$Player, fixed = TRUE);
mergedData$Player=gsub("Vince Hunter", "Vincent Hunter", mergedData$Player, fixed = TRUE);
mergedData$Player=gsub("Nene Hilario", "Nene", mergedData$Player, fixed = TRUE);
mergedData$Player=gsub("Taurean Waller-Prince", "Taurean Prince", mergedData$Player, fixed = TRUE);
mergedData$Player=gsub("Wesley Iwundu", "Wes Iwundu", mergedData$Player, fixed = TRUE);

# Check for number of mis-matches
mergedData$Player[!(mergedData$Player %in% miscStats$Player)]
miscStats$Player[!(miscStats$Player %in% mergedData$Player)]

# Merge misc dataset
mergedData<-merge(mergedData,miscStats); 

# Remove redundant columns
mergedData<- mergedData[c(1,3:49,54:57,62)] 

# Feature extraction
mergedData$MPG<-mergedData$MP/mergedData$G;
mergedData$A2TO<-mergedData$AST/mergedData$TOV

# Remove null values
mergedData[is.na(mergedData)] <- 0 

# Select threshold for subsetting
cutoff<-28.5 

# Subset data using Minutes per game
mergedData <- subset(mergedData, MPG >=cutoff );

# Subset Daraframe using relevant features
mainData<- mergedData[c(9:55)]; #relevant overall features
# mergedData<- mergedData[c(8:20,23,26,28,30:33, 36, 39:41, 45, 49:53,55)]; #offense features
# mergedData<- mergedData[c(21,24,25,27,34,37,38,42,46)]; #defense features

# Normalize data
mainData <- normalize(mainData, method= "standardize") 

# Feature Correlation plot
o=corrplot(cor(mainData),method='number')

# PCA ---------------------------------------------------------------------

res.cov <- cov(mainData);
round(res.cov,2);

# Find eigenvectors
eig<-eigen(res.cov)$vectors;

# Select 1st 2 eigenvectors
eigenv2<-eig[,(1:2)];

# Convert to matrix for PCA calculations
statsMatrix<-data.matrix(mainData, rownames.force = NA);

# Matrix multiplication
PrincipalComponents2<-statsMatrix%*%eigenv2;

# Convert back to statsDF for ggplot
statsDF<-data.frame(PrincipalComponents2); 

# PCA Plot
ggplot(statsDF,aes(statsDF$X1, statsDF$X2)) +
  labs(x="PC1",y="PC2")+
  geom_point(data=mergedData,aes(col =Pos, size= VORP))+
  geom_text_repel(data=mergedData, aes(label=Player), size=3+mergedData$VORP/max(mergedData$VORP))

# k-means clustering ---------------------------------------------------

# Replace index with player name (used for plot)
row.names(mainData) <- mergedData$Player 

# Clarify distance measures
res.dist <- get_dist(mainData, stand = TRUE, method = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Determinie optimal clusters through different methods
fviz_nbclust(mainData, kmeans, method = "wss")
fviz_nbclust(mainData, kmeans, method = "silhouette")
fviz_nbclust(mainData, kmeans, method = "gap_stat")

# Kmeans based on best cluster-number
km.res <- kmeans(mainData,8, nstart = 25)

# Visualize Kmeans clusters
fviz_cluster(km.res, mainData, ellipse = TRUE, ellipse.alpha= 0.1,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
             main= FALSE, xlab= FALSE, ylab = FALSE)

# Further cluster analysis
Clusters=data.frame(sort(km.res$cluster));
km.res;

# hierarchial clustering --------------------------------------------------

res.hc <- hclust(res.dist, method = "ward.D2" )

# Visualize using factoextra
fviz_dend(res.hc, k = 8, # Cut in 8 groups
          cex = 0.5, # label size
           horiz= TRUE, rect = TRUE # Add rectangle around groups
          )