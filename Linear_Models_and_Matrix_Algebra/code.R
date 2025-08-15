setwd("/home/kwabena-robert/Desktop/Omics_Analysis/Data_Science_Life_Sciences/Linear_Models_and_Matrix_Algebra")

# Install Packages and Load them
pakg <- c("UsingR","rafalib","downloader", "dplyr", "gapminder")
for (pkg in pakg) {
  if (!require(pkg, quietly = T)){
    #install.packages(pkg, dependencies = T)
    require(pkg, character.only = T)
  }
}