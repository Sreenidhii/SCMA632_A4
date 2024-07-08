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
sur_int=survey_df[,20:46] 

#Factor Analysis 

factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 