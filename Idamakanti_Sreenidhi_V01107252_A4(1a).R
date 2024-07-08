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
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv('D:\\CHRIST\\Boot camp\\DATA\\Survey.csv',header=TRUE) 
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)


#A)Do principal component analysis and factor analysis and identify the dimensions in the data. 

is.na(survey_df) 
sum(is.na(survey_df)) 
sur_int=survey_df[,20:46] 
str(sur_int) 
dim(sur_int) 
library(GPArotation) 
pca <- principal(sur_int,5,n.obs =162, rotate ="promax") 
pca 

om.h<-omega(sur_int,n.obs=162,sl=FALSE) 
op<-par(mfrow=c(1,1)) 
om<-omega(sur_int,n.obs=162) 
library(FactoMineR) 
pca<-PCA(sur_int,scale.unit = TRUE) 
summary(pca) 
fviz_pca_biplot(pca, repel = TRUE)
str(sur_int) 
dim(sur_int) 
show(sur_int) 




