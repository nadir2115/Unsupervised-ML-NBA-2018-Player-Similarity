#Unsupervised learning on NBA players- Nadir Nibras

rm(list = ls()); # clear workspace variables
cat("\014")   # it means ctrl+L. clear window
graphics.off() # close all plots


library(dplyr)
library(cluster)
library(factoextra)
library(fpc)
library(ggplot2)
library(readr)
library(datasets)
library(corrplot)
library(stats)
library(ggrepel)
library(textir) 
library(BBmisc)

# load data ---------------------------------------------------------------

trad<-read_csv("C:/Users/nadir/Documents/Machine Learning/NBA/36 minutes.csv")
advanced<- read_csv("C:/Users/nadir/Documents/Machine Learning/NBA/advanced.csv")
extra<- read_csv("C:/Users/nadir/Documents/Machine Learning/NBA/extra.csv")

# Data pre-processing -----------------------------------------------------

#merging 1st 2 datasets
data<-merge(trad,advanced);                      

#Fixing mismatches for merging
data$Player=gsub(".", "", data$Player, fixed = TRUE); 
extra$Player=gsub(".", "", extra$Player, fixed = TRUE);
extra$Player=gsub(" Jr", "", extra$Player, fixed = TRUE);
extra$Player=gsub(" III", "", extra$Player, fixed = TRUE);
extra$Player=gsub(" IV", "", extra$Player, fixed = TRUE);
extra$Player=gsub(" II", "", extra$Player, fixed = TRUE);
data$Player=gsub("Walt Lemon Jr", "Walter Lemon", data$Player, fixed = TRUE);
data$Player=gsub("Vince Hunter", "Vincent Hunter", data$Player, fixed = TRUE);
data$Player=gsub("Nene Hilario", "Nene", data$Player, fixed = TRUE);
data$Player=gsub("Taurean Waller-Prince", "Taurean Prince", data$Player, fixed = TRUE);
data$Player=gsub("Wesley Iwundu", "Wes Iwundu", data$Player, fixed = TRUE);

#Checking for number of mis-matches
data$Player[!(data$Player %in% extra$Player)]
extra$Player[!(extra$Player %in% data$Player)]

#merging final misc datasets
data<-merge(data,extra); 

#removing redundant columns
data<- data[c(1,3:49,54:57,62)] 

#Fature extraction
data$MPG<-data$MP/data$G;
data$A2TO<-data$AST/data$TOV

#removing null values
data[is.na(data)] <- 0 

#Selecting threshold for subsetting
cutoff<-28.5 

#subsetting data using MPG
newdata <- subset(data, MPG >=cutoff );
impdata<- newdata[c(9:55)];
# impdata<- newdata[c(8:20,23,26,28,30:33, 36, 39:41, 45, 49:53,55)]; #offense features
# impdata<- newdata[c(21,24,25,27,34,37,38,42,46)]; #defense features

#normalizing data
impdata <- normalize(impdata, method= "standardize") 

#Correlation plot
o=corrplot(cor(impdata),method='number')

# PCA ---------------------------------------------------------------------

res.cov <- cov(impdata);
round(res.cov,2);

# find eigenvectors
eig<-eigen(res.cov)$vectors;

# select 1st 2 eigenvectors
pc2<-eig[,(1:2)];

# converting table to matrix
datamat<-data.matrix(impdata, rownames.force = NA);

# matrix multiplication
pcad2<-datamat%*%pc2;

# convert back to dataframe for ggplot
dataFrame<-data.frame(pcad2); 

# PCA Plot
ggplot(dataFrame,aes(dataFrame$X1, dataFrame$X2)) +
  labs(x="PC1",y="PC2")+
  geom_point(data=newdata,aes(col =Pos, size= VORP))+
  geom_text_repel(data=newdata, aes(label=Player), size=3+newdata$VORP/max(newdata$VORP))

# k-means clustering ---------------------------------------------------

# Replacing index with player name (used for plot)
row.names(impdata) <- newdata$Player 

# Clarify distance measures
res.dist <- get_dist(impdata, stand = TRUE, method = "euclidean")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Determinie optimal clusters through different methods
fviz_nbclust(impdata, kmeans, method = "wss")
fviz_nbclust(impdata, kmeans, method = "silhouette")
fviz_nbclust(impdata, kmeans, method = "gap_stat")

# Kmeans based on best cluster-number
km.res <- kmeans(impdata,8, nstart = 25)

# Visualize in clusters
fviz_cluster(km.res, data =impdata, ellipse = TRUE, ellipse.alpha= 0.1,
             palette = "jco",repel = TRUE, ggtheme = theme_minimal(), 
             main= FALSE, xlab= FALSE, ylab = FALSE)
Clusters=data.frame(sort(km.res$cluster));


# hierarchial clustering --------------------------------------------------

# Hierarchical clustering using Ward's method
res.hc <- hclust(res.dist, method = "ward.D2" )

# Visualize using factoextra
fviz_dend(res.hc, k = 8, # Cut in 8 groups
          cex = 0.5, # label size
           horiz= TRUE, rect = TRUE # Add rectangle around groups
          )

