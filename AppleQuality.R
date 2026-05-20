library(readr)
install.packages("caret")
install.packages("randomForest")
library(ggplot2)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
install.packages("psych")
library(psych) 
library(GGally)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("kableExtra")
library(kableExtra)
Apple <- read_csv("apple_quality.csv")
tinytex::install_tinytex()
View(Apple)
head(Apple$Quality)
#Variable Identification
summary(Apple)
str(Apple) #Checks the string of each variable
table(Apple$Quality) #We have 1996 counts of Bad quality and 2004 counts of good quality
Apple$Quality <- as.factor(Apple$Quality)
str(Apple)
num_data <- Apple[, sapply(Apple, is.numeric)]
Numeric_table <- data.frame(
  Mean = sapply(num_data, mean),
  Median = sapply(num_data, median),
  Min = sapply(num_data, min),
  Max = sapply(num_data, max),
  Variance = sapply(num_data, var),
  SD = sapply(num_data, sd),
  Range = sapply(num_data, function(x) max(x) - min(x)),
  IQR = sapply(num_data, IQR)
)

Numeric_table

var_info <- data.frame(
  Variable = names(Apple),
  DataType = sapply(Apple, class),
  Category = sapply(Apple, function(x) {
    if (is.numeric(x)) {
      "Continuous"
    } else if (is.factor(x) | is.character(x)) {
      "Categorical"
    }
  }),
  stringsAsFactors = FALSE)

var_info$Role <- c("Independent", "Independent", "Independent","Independent", "Independent", "Independent","Independent", "Independent" ,"Dependent")
# Add roles and descriptions manually
var_info$Description <- c(
  "Identifies each fruit of the data size",
  "Explains the size of the Apple",
  "Explains the weight of the Apple",
  "The degree of sweetness for the Apple",
  "Explains how crunchy the Apple is",
  "Explains the level of juiciness",
  "Explains the stage of ripeness",
  "The Acidity level of the Apple",
  "Overall Quality of the Apple"
)
var_info
#The table explains all details of each variable including the descriptions.
#This gives a basic understanding of each variable, including it's purpose.


#Univariate Analysis

#Summary Table of all Numeric Variables
num_data <- Apple[, sapply(Apple, is.numeric)]

Summary <- describe(num_data[ , c('Size', 'Weight', 'Sweetness'
                    , 'Crunchiness', 'Juiciness'
                    , 'Ripeness', 'Acidity', "A_id")], fast=TRUE)
Summary$variance <- Summary$sd^2
Summary[,c("mean", "median", "min", "max", "variance", "sd" )]
#The summary table explains all necessary numeric descriptions of each variable

#Summary table of the dependent variable "Quality"
qty <- Apple$Quality
qty_sum <- data.frame(
  Category = names(table(qty)),
  Count = as.vector(table(qty)),
  Percent = round(as.vector(prop.table(table(qty))) * 100, 1)
)
qty_sum
#qty_sum shows the percentage of bad and good quality apples given in the Dataset
qty_bar <- ggplot(data = Apple, aes(x = Quality)) + geom_bar(fill = "lightblue", color = "black") + labs(title = "Bar Chart of Apple Quality",
                                                                                                         x = "Quality",y = "Count")  
qty_bar # Visual representation of the distribution of Quality

#Visual Analysis of each variable
for(i in names(num_data)) {
  hist(num_data[[i]],
       main = paste("Histogram of", i),
       xlab = i,
       col = "lightblue",
       border = "black")
}
for(i in names(num_data)) {
  boxplot(num_data[[i]],
          main = paste("Barplot of", i),
          ylab = "Size",
          col = "lightblue")
}
#The code creates a Histogram and Bar plot for each variable showing the variables visually.

####Bi-Variate Analysis
ggpairs(num_data) #shows the relationship between all numeric variables in an easy to understand table
ggpairs(Apple)
#A heat map will help explain it better
corr <- round(cor(num_data), 3)
corr #This table shows the correlations of all variables up to 3 decimal points

corr2 <- round(cor(Apple),2)
ggcorrplot(corr)
ggcorrplot(corr, method="circle")
ggcorrplot(corr2) #correlation of all variables
ggcorrplot(corr2, method="circle")

#boxplot bivariate analysis
boxplot(Apple$Size ~ Apple$Quality, main = "Size by Quality")
boxplot(Apple$Weight ~ Apple$Quality, main = "Weight by Quality")
boxplot(Apple$Sweetness ~ Apple$Quality, main = "Sweetness by Quality")
boxplot(Apple$Crunchiness ~ Apple$Quality, main = "Crunchiness by Quality")
boxplot(Apple$Juiciness ~ Apple$Quality, main = "Juiciness by Quality")
boxplot(Apple$Ripeness ~ Apple$Quality, main = "Ripeness by Quality")
boxplot(Apple$Acidity ~ Apple$Quality, main = "Acidity by Quality")

####Missing Values Treatment
sum(is.na(Apple))
colSums(is.na(Apple))
#There is no missing values for any variables, so there will not be any treatment required

####Outlier Treatment
# Ask prof

####Variable Transformation
for(i in names(num_data)) {
  hist(num_data[[i]],
       main = paste("Histogram of", i),
       xlab = i,
       col = "lightblue",
       border = "black")
} # All variables have normal distrubitions so no variable transformation is required

####Variable Creation

#Also not required

#### Potential Method for analyzing the data set
#Since the dependent variable is categorical, I will be doing a log regression.
#This will help predict the probability of finding good or bad quality based on the variables
model <- glm(Quality ~ Size + Weight + Sweetness + Crunchiness + Juiciness + Ripeness + Acidity,
             data = Apple)
summary(model) #All variables but crunchiness are significant

model2 <- glm(Quality ~ Size + Weight + Sweetness + Juiciness + Ripeness + Acidity,
             data = Apple)
summary(model2)

