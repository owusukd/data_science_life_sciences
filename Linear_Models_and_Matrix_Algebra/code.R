setwd("/home/kwabena-robert/Desktop/Omics_Analysis/Data_Science_Life_Sciences/Linear_Models_and_Matrix_Algebra")

# Install Packages and Load them
pakg <- c("UsingR","rafalib","downloader", "dplyr", "gapminder")
for (pkg in pakg) {
  if (!require(pkg, quietly = T)){
    #install.packages(pkg, dependencies = T)
    require(pkg, character.only = T)
  }
}

# Matrix
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
a
b
a%*%b

# Calculating the Mean using matrix
y <- father.son$sheight
print(mean(y))
N <- length(y)
Y <- matrix(y,N,1)
A <- matrix(1,N,1)
Y_bar <- t(A)%*%Y / N
Y_bar
# OR
Y_bar <- crossprod(A,Y)/N
Y_bar

# Calculating the Sample Variance using matrix
var(y)
r <- Y - as.numeric(Y_bar)*A
Y_var <- t(r)%*%r /(N-1)
Y_var

# Linear models
y <- father.son$sheight
x <- father.son$fheight
X <- cbind(1,x)
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
beta_hat
# OR
beta_hat <- solve(crossprod(X))%*%crossprod(X,y)
beta_hat
