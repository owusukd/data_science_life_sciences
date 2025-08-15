setwd("/home/kwabena-robert/Desktop/Omics_Analysis/Data_Science_Life_Sciences/Statistics_and_R")

# Install Packages and Load them
pakg <- c("UsingR","rafalib","downloader", "dplyr", "gapminder")
for (pkg in pakg) {
  if (!require(pkg, quietly = T)){
    #install.packages(pkg, dependencies = T)
    require(pkg, character.only = T)
  }
}

# Download and load data
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
if (!file.exists(filename)) download(url, destfile=filename)
dat <- read.csv(filename)

# Filter the Data
controls <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist()
mean(controls)
treatments <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist()
mean(treatments)
obsdiff <- mean(treatments) - mean(controls)

# Tutorial
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv(filename)

# Select a set of columns: the name and the sleep\_total columns.
sleepData <- select(msleep, name, sleep_total)
head(sleepData)

# To select all the columns *except* a specific column, use the "-" 
# (subtraction) operator (also known as negative indexing)
head(select(msleep, -name))

# To select a range of columns by name, use the ":" (colon) operator
head(select(msleep, name:order))

# To select all columns that start with the character string "sl", 
# use the function `starts_with()`
head(select(msleep, starts_with("sl")))

# Filter the rows for mammals that sleep a total of more than 16 hours
filter(msleep, sleep_total >= 16)

# Filter the rows for mammals that sleep a total of more than 16 hours *and* 
# have a body weight of greater than 1 kilogram.
filter(msleep, sleep_total >= 16, bodywt >= 1)

# Filter the rows for mammals in the Perissodactyla and Primates taxonomic order
filter(msleep, order %in% c("Perissodactyla", "Primates"))

# Pipe Operator: %>%
head(select(msleep, name, sleep_total))
# OR
msleep %>% 
  select(name, sleep_total) %>% 
  head

# Arrange Or Re-order Rows Using `arrange()`
msleep %>% arrange(order) %>% head

# Now we will select three columns from msleep, arrange the rows by the taxonomic 
# order and then arrange the rows by sleep\_total and arrange the rows in the 
# sleep\_total column in a descending order. For this, use the function `desc()`
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>% 
  filter(sleep_total >= 16)

# Random Variables
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
if (!file.exists(filename)) download(url, destfile=filename)
population <- unlist( read.csv(filename) )
mean(population)

null <- vector("numeric", n)
for (i in 1:10000){
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  null[i] <- mean(treatment) - mean(control)
}
mean(null >= obsdiff)

# Probability Distributions
data(gapminder)
head(gapminder)

# Create a vector 'x' of the life expectancy of each country 
# for the year 1952. Plot a histogram of these life expectancy 
# to see the spread of the different countries.
colnames(gapminder)
x <- unlist(gapminder[which(gapminder$year == "1952"),"lifeExp"], use.names = F)
hist(x)

# Example, for q = 40
prop(40)

# Now let's build a range of q's that we can apply the function to
qs <- seq(from=min(x), to=max(x), 20)

# Proportion of life expectancy <= q function
prop <- function(q){
  mean(x <= q)
}
props <- sapply(qs, prop)
# Or
props = sapply(qs, function(q) mean(x <= q))

plot(qs, props)
plot(ecdf(x))

# Normal Distribution 
# Using the same process as before (in Null Distribution Exercises), 
# set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 
# times. Save these averages. After that, set the seed at 1, then using a 
# for-loop take a random sample of 50 mice 1,000 times. Save these averages.
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
if (!file.exists(filename)) download(url, destfile=filename)
population <- unlist( read.csv(filename) )

set.seed(1)
n <- 1000
# Sample of 5
mean_weight <- vector("numeric", n)
for (i in 1:n) {
  mean_weight[i] <- sample(population, 5) %>% mean
}
hist(mean_weight)
# Sample of 50
set.seed(1)
mean_weight <- vector("numeric", n)
for (i in 1:n) {
  mean_weight[i] <- sample(population, 50) %>% mean
}
hist(mean_weight)

# Population, Samples, and Estimates
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
if (!file.exists(filename)) download(url, destfile=filename)
dat <- read.csv(filename) 

# We will remove the lines that contain missing values:
dat <- na.omit( dat )
# Use dplyr to create a vector x with the body weight of all males on the 
# control (chow) diet. What is this population’s average?
x <- filter(dat, Sex=="M" & Diet=="chow")[,"Bodyweight"] %>% unlist(use.names = F)
# Or
x <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist(use.names = F)
mean(x)
# Now use the rafalib package and use the popsd function to compute the 
# population standard deviation.
popsd(x)
# Set the seed at 1. Take a random sample X 
# of size 25 from x. What is the sample average?
set.seed(1)
X <- sample(x, 25)
mean(X)
# Use dplyr to create a vector y with the body weight of all males on the 
# high fat (hf) diet. What is this population’s average?
y <- filter(dat, Sex=="M" & Diet=="hf")[, "Bodyweight"] %>% unlist(use.names = F)
# Or
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist(use.names = F)
mean(y)
# Now use the rafalib package and use the popsd function to compute the 
# population standard deviation.
popsd(y)
# Set the seed at 1. Take a random sample Y
# of size 25 from y. What is the sample average?
set.seed(1)
Y <- sample(y, 25)
mean(Y)
# What is the difference in absolute value between y¯−x¯
# and $$\bar{X}-\bar{Y}$?
abs(mean(y) - mean(x))

# We can see that, as predicted by the CLT, the distribution of the random 
# variable is very well approximated by the normal distribution.
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

# T-test
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
if (!file.exists(filename)) download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

# First, let's split this into two birth weight datasets: one of birth weights 
# to non-smoking mothers and the other of birth weights to smoking mothers.
bwt.nonsmoke <- filter(babies, smoke == 0) %>% select(bwt) %>% unlist(use.names = F)
bwt.smoke <- filter(babies, smoke == 1) %>% select(bwt) %>% unlist(use.names = F)
# true population difference in means between smoking and non-smoking birth weights
mean(bwt.smoke) - mean(bwt.nonsmoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
# T Test
t.test(bwt.smoke, bwt.nonsmoke)

# Confidence Interval
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- "mice_pheno.csv"
if (!file.exists(filename)) download(url,destfile=filename)
dat <- read.csv(filename)

# Demonstrating Central Limit Theorem
# We show 250 random realizations of 95% confidence intervals. 
# The color denotes if the interval fell on the parameter or not.
# This is done for female whose diet is chow
chow_bwt <- filter(dat, Sex=="F") %>% select(Bodyweight) %>% unlist(use.names = F)
iter <- 250
N <- 30
Z <- qnorm(1 - 0.05/2)
plot(mean(chow_bwt)+c(-7,7), c(1,1), type = "n", xlab = "Weight",
     ylab = "Interval", ylim = c(1, iter))
abline(v=mean(chow_bwt))
for (i in 1:iter) {
  chow <- sample(chow_bwt,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Z*se, mean(chow)+Z*se)
  covered <- mean(chow_bwt) <= interval[2] & mean(chow_bwt) >= interval[1]
  color <- ifelse(covered, "green", "red")
  lines(interval, c(i,i), col = color)
}
mypar()

# Power Calculation
controlPopulation <- filter(dat,Sex == "F" & Diet == "chow") %>%  
  select(Bodyweight) %>% unlist(use.names = F)

hfPopulation <- filter(dat,Sex == "F" & Diet == "hf") %>%  
  select(Bodyweight) %>% unlist(use.names = F)

mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print((mu_hf - mu_control)/mu_control * 100) #percent increase

set.seed(1)
reject <- function(N, alpha=0.05){
  hf <- sample(hfPopulation,N) 
  control <- sample(controlPopulation,N)
  pval <- t.test(hf,control)$p.value
  pval < alpha
}
iter <- 10000
N <- 30
rejections <- replicate(iter,reject(N))
# Power for N = 30
mean(rejections)

# Power Calculation for different N's
mypar()
Ns <- seq(5, 50, 5)
power <- sapply(Ns,
                function(N){
                  rejections <- replicate(iter, reject(N))
                  mean(rejections)
                  }
                )
plot(Ns, power, type="b")

# Power Calculation for different Alpha's for N = 30
mypar()
N <- 30
alphas <- c(0.1, 0.05, 0.01, 0.001, 0.0001)
power <- sapply(alphas,
                function(alpha){
                  rejections <- replicate(iter, reject(N))
                  mean(rejections)
                })
plot(alphas, power, type="b")

# EDA
# QQ-Plot
# Check if Bodyweight follows a normal distribution
qqnorm(dat$Bodyweight)
qqline(dat$Bodyweight)
hist(dat$Bodyweight)

# Check if Bodyweight follows a normal distribution by Diet
qqnorm(chow_bwt)
qqline(chow_bwt)
hist(chow_bwt)

hf_bwt <- filter(dat, Sex=="F") %>% select(Bodyweight) %>% unlist(use.names = F)

# Boxplot
boxplot(split(dat$Bodyweight, dat$Diet), xlab = "Diet", ylab = "Bodyweight")
# OR
boxplot(dat$Bodyweight ~ dat$Diet, xlab = "Diet", ylab = "Bodyweight")

# Scatter Plot
data("father.son", package = "UsingR")
y <- father.son$sheight
x <- father.son$fheight

plot(x, y, xlab = "Father's Height", ylab = "Son's Height", 
     main = paste("Correlation =", round(cor(x,y),2)))

# Stratify Son's Height by Fatther's Height
boxplot(split(y, round(x)))
# Stratify based on Z-scores and plotting only the means of each strata
# and adding a linear line
xz <- (x-mean(x))/sd(x)
yz <- (y-mean(y))/sd(y)
means <- tapply(yz, round(xz*4)/4, mean)
fatherheights <- as.numeric(names(means))
plot(fatherheights, means, ylim = range(fatherheights))
abline(0, cor(xz,yz))

# Symmetry of Log Ratios
data(nym.2002, package="UsingR")
time <- nym.2002$time
# A plot of the ratio of times to the median time, with horizontal lines at 
# twice as fast as the median time, and twice as slow as the median time.
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
# A plot of the log2 ratio of times to the median time. The horizontal lines 
# indicate the same as above: twice as fast and twice as slow.
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

# Median, MAD, and Spearman Correlation
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)
chick = na.omit(chick)

# Mean vs Median, The Median is robust to the outlier.
weight.4 <- chick$weight.4
weight.4.outlier <- c(weight.4, 3000)
# Ratio of the means and sd
mean(weight.4.outlier)/mean(weight.4)
sd(weight.4.outlier)/sd(weight.4)
# Ratio of the medians
median(weight.4.outlier)/median(weight.4)
mad(weight.4.outlier)/mad(weight.4)

# Pearson Correlation vs Spearman Correlation
# The Spearman Correlation is robust to the outlier.

# Plot the weights of chicks from day 4 and day 21.
plot(chick$weight.4, chick$weight.21)
# Pearson correlation   
pr <- cor(chick$weight.4, chick$weight.21)
pr.outlier <- cor(c(chick$weight.4,3000), c(chick$weight.21,3000))
pr.outlier/pr
# Spearman Correlation
spr <- cor(chick$weight.4, chick$weight.21, method = "spearman")
spr.outlier <- cor(c(chick$weight.4,3000), c(chick$weight.21,3000), method = "spearman")
spr.outlier/spr

# T-test vs Wilcoxon test, The Wilcoxon test is robust to the outlier.
x <- filter(chick, Diet == 1) %>% select(weight.4) %>% unlist(use.names = F)
y <- filter(chick, Diet == 4) %>% select(weight.4) %>% unlist(use.names = F)
t.test(x,y)
wilcox.test(x,y)

x.outlier <- c(x, 200)
t.test(x.outlier, y)
wilcox.test(x.outlier, y)

# Downside of Wilcoxon Test
# Because the Wilcoxon works on ranks, once the two 
# groups show complete separation, that is all points from group ‘y’ are above 
# all points from group ‘x’, the statistic will not change, regardless of how 
# large the difference grows
# Likewise, the p-value has a minimum value, regardless of how far apart 
# the groups are.

library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic
wilcox.test(x, y+10)$statistic
wilcox.test(x, y+100)$statistic

# Same P-value regardless of how large the y values grows.
wilcox.test(c(1,2,3), c(4,5,6))
wilcox.test(c(1,2,3), c(400,500,600))

