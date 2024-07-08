# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

install_and_load(packages)
survey_df<-read.csv('D:\\CHRIST\\Boot camp\\DATA\\Survey.csv',header=TRUE)
sur_int=survey_df[,20:46] 


#B) Carry our cluster analysis and characterize the respondents based on their background variables. 
library(cluster) 
library(factoextra) 
show(sur_int) 
fviz_nbclust(sur_int,kmeans,method = "gap_stat") 
set.seed(123) 
km.res<-kmeans(sur_int,4,nstart = 25) 
fviz_cluster(km.res,data=sur_int,palette="jco", 
             ggtheme = theme_minimal()) 
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=4,palette = "jco") 
library(pheatmap) 
pheatmap(t(sur_int),cutree_cols = 4) 