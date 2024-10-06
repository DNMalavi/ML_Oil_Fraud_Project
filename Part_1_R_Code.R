
# Part 1: Unsupervised Learning code: PCA & K-Means

# Load Installed Packages from the R Library for the Analysis

warning = FALSE  
suppressWarnings(suppressMessages({    
  library(caret)    
  library(ggplot2)    
  library(dplyr)   
  library(readxl)   
  library(readr)    
  library(janitor)  
  library(FactoMineR)
  library(MASS)
  library(factoextra)
  library(rmarkdown)
  library(knitr) 
  library(officedown)
  library(quarto)
  library(gtsummary)
  library(pander)
  library(kernlab)}))

# Load and Inspect Hyperspectral Imaging Data



# Load HSI spectra data
hsi<-read_excel("HSI.xlsx")
# Check dimensions
dim(hsi)
#We have 183 observations and 228 variables


# Check a few of the column names
colnames(hsi[,c(1:4)])
#Check for any missing values
anyNA(hsi)# There are no missing values


#Considering that we are conducting a binary classification, we will remove some columns

hsi<-hsi[,-c(1,3)]
table(hsi$class_1)
#convert class_1 to a factor
hsi$class_1<-as.factor(hsi$class_1)
#There are two classes: The oils are either pure/authentic or adulterated


#Check whether the data is normalized. We want a value between 0 and 1
print(paste('The max value is', max(hsi[,-c(1:4)]), 
            'and the min value is', min(hsi[,-c(1:4)])))

# Load and Inspect Raman Spectroscopy Data


#Load Raman spectra data
raman<-read_excel("Raman.xlsx")
#Check dimensions
dim(raman)
#We have 183 observations and 1404 variables


# Bind the data to have the same columns as HSI data
raman<-cbind(hsi[,c(1,2)],raman[,-c(1:3)])
colnames(raman[,c(1:3)])#check whether the changes have been effected
table(raman$class_1)#Check the class distribution
class(raman$class_1)#ensure class_1 is a factor


# Define the normalization function to have values of 0 and 1
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
raman<-min_max_normalize(raman[,-c(1:2)])
print(paste('The max value is', max(raman[,-c(1:2)]), 'and the min value is', min(raman[,-c(1:2)])))
anyNA(raman)
dim(raman)

# Load and Inspect FTIR Spectroscopy Data


#Load FTIR spectra data
ftir<-read_excel("FTIR.xlsx")
#Check dimensions
dim(ftir)
#We have 183 observations and 919 variables


#Bind the data to have the same columns as HSI data
ftir<-cbind(hsi[,c(1,2)],ftir[,-c(1:3)])
colnames(ftir[,c(1:3)])#check whether the changes have been effected
table(ftir$class_1)#Check the class distribution
class(ftir$class_1)#ensure class_1 is a factor
print(paste('The max value is', max(ftir[,-c(1:2)]), 
            'and the min value is', min(ftir[,-c(1:2)])))#The data is OK
dim(ftir)

# Load and Inspect UV-Vis Spectroscopy Data


#Load Uv-Vis spectra data
uv_vis<-read_excel("UVVIS.xlsx")
#Check dimensions
dim(uv_vis)


#Bind the data to have the same columns as HSI data
uv_vis<-cbind(hsi[,c(1,2)],uv_vis[,-c(1:4)])
colnames(uv_vis[,c(1:3)])#check whether the changes have been effected
table(uv_vis$class_1)#Check the class distribution
class(uv_vis$class_1)#ensure class_1 is a factor
print(paste('The max value is', max(uv_vis[,-c(1:2)]), 
            'and the min value is', min(uv_vis[,-c(1:2)])))#The data is OK
dim(uv_vis)


# Define the normalization function to have values of 0 and 1
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
uv_vis<-min_max_normalize(uv_vis[,-c(1:2)])
print(paste('The max value is', max(uv_vis[,-c(1:2)]), 
            'and the min value is', min(uv_vis[,-c(1:2)])))
anyNA(uv_vis)
dim(uv_vis)
#There are 183 observations and 121 covariates

# Load and Inspect GC-MS Data


# Load GC-MS data
gc_ms<-read_excel("GC_MS.xlsx")
#Check dimensions
dim(gc_ms)
gc_ms$class_1<-factor(gc_ms$class_1)#convert to factor


# Define the normalization function to have values of 0 and 1
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
gc<-min_max_normalize(gc_ms[,-c(1:2)])
print(paste('The max value is', max(gc), 'and the min value is', min(gc)))
anyNA(gc_ms)
gc_ms<-cbind(gc_ms[,c(1,2)],gc)

# Unsupervised Learning

## Exploratory Data Analysis by Principal Component Analysis (PCA)

# Run PCA
hsi_pca<-PCA(hsi[,-c(1,2)],graph = F)#HSI data
hsi_pca$eig[1:10,]#extracting the first 5 components' eigenvalues


raman_pca<-PCA(raman[,-c(1,2)],graph = F)#Raman data
raman_pca$eig[1:10,]#extract the first 5 components' eigenvalues


ftir_pca<-PCA(ftir[,-c(1,2)],graph = F)#FTIR data
ftir_pca$eig[1:10,]#extract the first 5 components' eigenvalues


uv_vis_pca<-PCA(uv_vis[,-c(1,2)],graph = F)#UV-Vis data
uv_vis_pca$eig[1:10,]#extract the first 5 components' eigenvalues


gc_ms_pca<-PCA(gc_ms[,-c(1,2)],graph = F)#GC-MS data
gc_ms_pca$eig[1:7,]

## Scree Plots

# HSI
s1<-fviz_eig(hsi_pca, addlabels = TRUE, ylim = c(0, 90),xlim=c(1,5), 
             main = 'HSI Scree Plot',barfill = "chocolate1",hjust = 0.5,ncp = 9,
             ggtheme = theme_bw(),xlab = "PCs", ylab = "% variance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())
print(s1)

# Raman
s2<-fviz_eig(raman_pca, addlabels = TRUE, ylim = c(0, 60),xlim=c(1,5),
             main = 'Raman Scree Plot',barfill = "greenyellow",
             hjust = 0.5,ncp = 20,
             ggtheme = theme_bw(),xlab = "PCs",ylab = "% variance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())
print(s2)

#FTIR
s3<-fviz_eig(ftir_pca, addlabels = TRUE, ylim = c(0, 90),xlim=c(1,5),
             main = 'FTIR Scree Plot',
             barfill = "dodgerblue1",hjust = 0.5,
             ncp = 7,ggtheme = theme_bw(),xlab = "PCs", ylab = "% variance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())
print(s3)

#Uv-Vis
s4<-fviz_eig(uv_vis_pca, addlabels = TRUE, ylim = c(0, 90),
             xlim=c(1,5),main = 'UV-Vis Scree Plot',
             barfill = "goldenrod4",hjust = 0.5,ncp = 15,
             ggtheme = theme_bw(),xlab = "PCs", ylab = "% variance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())
print(s4)


#Patch together the scree plots
gridExtra::grid.arrange(s1,s2,s3,s4, nrow =2)

#GC-MS

s5<-fviz_eig(gc_ms_pca, addlabels = TRUE, ylim = c(0, 40),
             main = 'GC-MS',barfill = "lightblue",hjust = 0.5,
             ncp = 8,ggtheme = theme_bw(),xlab = "PCs", ylab = "% variance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid = element_blank())

print(s5)

#Visualization of PC Scores

#HSI Data
hsi_new<-as.data.frame(hsi_pca$ind$coord) #Extract the PCs
colnames(hsi_new)<-c("PC_1","PC_2", "PC_3","PC_4","PC_5")
hsi_new<-cbind(hsi[,c(1,2)],hsi_new)#Bind with the dependent variables
head(hsi_new)


#Raman Data
raman_new<-as.data.frame(raman_pca$ind$coord) #Extract the PCs
colnames(raman_new)<-c("PC_1","PC_2", "PC_3","PC_4","PC_5")
raman_new<-cbind(hsi[,c(1,2)],raman_new)#Bind with the dependent variables
head(raman_new)


#FTIR Data
ftir_new<-as.data.frame(ftir_pca$ind$coord) #Extract the PCs
colnames(ftir_new)<-c("PC_1","PC_2", "PC_3","PC_4","PC_5")
ftir_new<-cbind(hsi[,c(1,2)],ftir_new)#Bind with the dependent variables
head(ftir_new)


#Uv_Vis Data
uvvis_new<-as.data.frame(uv_vis_pca$ind$coord) #Extract the PCs
colnames(uvvis_new)<-c("PC_1","PC_2", "PC_3","PC_4","PC_5")
uvvis_new<-cbind(hsi[,c(1,2)],uvvis_new)#Bind with the dependent variables
head(uvvis_new)


#GC-MS Data
gc_new<-as.data.frame(gc_ms_pca$ind$coord) #Extract the PCs
colnames(gc_new)<-c("PC_1","PC_2", "PC_3","PC_4","PC_5")
gc_new<-cbind(gc_ms[,c(1,2)],gc_new)#Bind with the dependent variables
head(gc_new)

# PC Plots

# HSI PC Plot
p1 <- hsi_new %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, shape = class_1, color = perc_adulter)) +
  geom_point() + 
  labs(x = "PC1 (79.8%)", y = "PC2 (10.9%)",title = "HSI PC Plot", 
       shape = "Oil type", color = "Percent Adulteration") +
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "none") +
  scale_color_gradient(low = "#000000", high = "red") +
  stat_ellipse(aes(group = class_1), 
               level = 0.95, 
               geom = "polygon", alpha = 0.2,
               color = 'black', linewidth = 0.6)


#Raman Plot
p2 <- raman_new %>% 
  ggplot(mapping = aes(x = PC_2, y = PC_3, shape = class_1, color = perc_adulter)) +
  geom_point() + 
  labs(x = "PC2 (17.1%)", y = "PC3 (10.7%)",title = "Raman PC Plot",
       shape = "Oil type", color = "Percent Adulteration") +
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    axis.text.x = element_text(color = 'black', size = 10),
    panel.grid = element_blank(),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)) +
  scale_color_gradient(low = "#000000", high = "red") +
  stat_ellipse(aes(group = class_1), 
               level = 0.95, 
               geom = "polygon", alpha = 0.2,
               color = 'black', linewidth = 0.6)


#FTIR Plot
p3 <- ftir_new %>% 
  ggplot(mapping = aes(x = PC_2, y = PC_3, shape = class_1, color = perc_adulter)) +
  geom_point() + 
  labs(x = "PC2 (10.9%)", y = "PC3 (7.0%)",title = "FTIR PC Plot", 
       shape = "Oil type", color = "Percent Adulteration") +
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    axis.text.x = element_text(color = 'black', size = 10),
    panel.grid = element_blank(),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.position = "none") +
  scale_color_gradient(low = "#000000", high = "red") +
  stat_ellipse(aes(group = class_1), 
               level = 0.95, 
               geom = "polygon", alpha = 0.2,
               color = 'black', linewidth = 0.6)


#Uv_Vis Plot
p4 <- uvvis_new%>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, shape = class_1, color = perc_adulter)) +
  geom_point() + 
  labs(x = "PC1 (74.5%)", y = "PC2 (11.7%)",title = "Uv_Vis PC Plot",
       shape = "Oil type", color = "Percent Adulteration") +
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)) +
  scale_color_gradient(low = "#000000", high = "red") +
  stat_ellipse(aes(group = class_1), 
               level = 0.95, 
               geom = "polygon", alpha = 0.2,
               color = 'black', linewidth = 0.6)

Patch the PC Plots together


suppressWarnings(suppressMessages(library(gridExtra)))
grid.arrange(p1,p2,p3,p4, nrow = 2)


#GC-MS Plot
p5 <- gc_new %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, shape = class_1, color = perc_adulter)) +
  geom_point() + 
  labs(x = "PC1 (39.3%)", y = "PC2 (18.5%)",title = "GC-MS PC Plot", 
       shape = "Oil type", color = "Percent Adulteration") +
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6)) +
  scale_color_gradient(low = "#000000", high = "red") +
  stat_ellipse(aes(group = class_1), 
               level = 0.95, 
               geom = "polygon", alpha = 0.2,
               color = 'black', linewidth = 0.6)
#Display plot
p5
# PC variable contributions
#HSI
h1<-fviz_contrib(hsi_pca, choice = "var", top = 5,
                 axes = 1, sort.val = 'desc', fill = "olivedrab")
h2<-fviz_contrib(hsi_pca, choice = "var", top = 5,
                 axes = 2, sort.val = 'desc', fill = "cadetblue")
grid.arrange(h1,h2, nrow = 1)


#Raman
r1<-fviz_contrib(raman_pca, choice = "var", top = 5,axes = 1,
                 sort.val = 'desc', fill = "#E7B800")
r2<-fviz_contrib(raman_pca, choice = "var", top = 5,
                 axes = 2, sort.val = 'desc', fill = "#00AFBB")
grid.arrange(r1,r2, nrow = 1)


#FTIR
f1<-fviz_contrib(ftir_pca, choice = "var", top = 5,axes = 1,
                 sort.val = 'desc', fill = "tan4")
f2<-fviz_contrib(ftir_pca, choice = "var", top = 5,axes = 2, 
                 sort.val = 'desc', fill = "cornflowerblue")
grid.arrange(f1,f2, nrow = 1)


#Uv-Vis
uv1<-fviz_contrib(uv_vis_pca, choice = "var", top = 5,axes = 1,
                  sort.val = 'desc', fill = "thistle")
uv2<-fviz_contrib(uv_vis_pca, choice = "var", top = 5,axes = 2,
                  sort.val = 'desc', fill = "powderblue")
grid.arrange(uv1,uv2, nrow = 1)


#GC-MS
gc1<-fviz_contrib(gc_ms_pca, choice = "var", top = 5,axes = 1, 
                  sort.val = 'desc', fill = "chocolate")
gc2<-fviz_contrib(gc_ms_pca, choice = "var", top = 5,axes = 2,
                  sort.val = 'desc', fill = "gray58")
grid.arrange(gc1,gc2, nrow = 1)

# K-Means Clustering


# Let us find the number of clusters based on silhoutte method
# optimal number of clusters for HSI
hsi_clust<-fviz_nbclust(hsi[,-c(1:2)], kmeans, method = "silhouette", k.max=10)
print(hsi_clust)


# optimal number of clusters for Raman
raman_clust<-fviz_nbclust(raman, kmeans, method = "silhouette", k.max=10)
print(raman_clust)


#optimal number of clusters for FTIR
ftir_clust<-fviz_nbclust(ftir[,-c(1,2)], kmeans, method = "silhouette", k.max=10)
print(ftir_clust)


#optimal number of clusters for UV-Vis
uvvis_clust<-fviz_nbclust(uv_vis, kmeans, method = "silhouette", k.max=10)
plot(uvvis_clust)


#optimal number of clusters for gc-ms
gc_clust<-fviz_nbclust(gc, kmeans, method = "silhouette", k.max=10)
print(gc_clust)

Then let us perform k-means clustering with the optimal number of clusters


#HSI k-means analysis and plots

hsi_kmeans <- kmeans(hsi[,-c(1,2)],2)
cluster<-  hsi_kmeans$cluster
hsi_k_data <-cbind(hsi_new,cluster)
hsi_k_data$cluster<-as.factor(hsi_k_data$cluster)

hsi_k_data %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, color = cluster, shape = class_1)) +
  geom_point() + 
  labs(x = "PC1", y = "PC2",title = "HSI k-means cluster Plot")+
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "right")+
  scale_color_manual(values = c("blue", "red"))


#Raman k-means analysis and plotting 

raman_kmeans <- kmeans(raman,3)
cluster<-  raman_kmeans$cluster
raman_k_data <-cbind(raman_new,cluster)
raman_k_data$cluster<-as.factor(raman_k_data$cluster)

raman_k_data %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, color = cluster, shape = class_1)) +
  geom_point() + 
  labs(x = "PC1", y = "PC2",title = "Raman k-means cluster Plot")+
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "right")+
  scale_color_manual(values = c("blue", "red","black"))


#FTIR k-means analysis and plotting

ftir_kmeans <- kmeans(ftir[,-c(1,2)],3)
cluster<-  ftir_kmeans$cluster
ftir_k_data <-cbind(ftir_new,cluster)
ftir_k_data$cluster<-as.factor(ftir_k_data$cluster)

ftir_k_data %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, color = cluster, shape = class_1)) +
  geom_point() + 
  labs(x = "PC1", y = "PC2",title = "FTIR k-means cluster Plot")+
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "right")+
  scale_color_manual(values = c("blue", "red","black"))


#UV-Vis k-means analysis and plotting

uvvis_kmeans <- kmeans(uv_vis,2)
cluster<-  uvvis_kmeans$cluster
uvvis_k_data <-cbind(uvvis_new,cluster)
uvvis_k_data$cluster<-as.factor(uvvis_k_data$cluster)

uvvis_k_data %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, color = cluster, shape = class_1)) +
  geom_point() + 
  labs(x = "PC1", y = "PC2",title = "UV-Vis k-means cluster Plot")+
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "right")+
  scale_color_manual(values = c("blue", "red"))


#GC-MS k-means analysis and plotting

gc_kmeans <- kmeans(gc,2)
cluster<-  gc_kmeans$cluster
gc_k_data <-cbind(gc_new,cluster)
gc_k_data$cluster<-as.factor(gc_k_data$cluster)

gc_k_data %>% 
  ggplot(mapping = aes(x = PC_1, y = PC_2, color = cluster, shape = class_1)) +
  geom_point() + 
  labs(x = "PC1", y = "PC2",title = "GC-MS k-means cluster Plot")+
  theme_bw() +
  theme(
    panel.border = element_rect(color = 'black', fill = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(color = 'black', size = 10), 
    aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    plot.title = element_text(size = 9, hjust = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    legend.position = "right")+
  scale_color_manual(values = c("blue", "red"))

------------------------------END-----------------------------------------------