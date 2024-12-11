install.packages("haven")
# Git and GitHub
# 1) Provide the link to the GitHub repo that you used to practice git from Week 1. It should
# have:
#    Your name on the README file.
# • At least one commit with your name, with a description of what you did in that commit.

# Clone the repository (if not already done)
system("git clone https://github.com/LinneaLiny/Lin1-Jiang2-Fan3-a1.git")

# Set the working directory to the cloned repository
setwd("Lin1-Jiang2-Fan3-a1")

# Check the file exists and is not empty
if (file.exists("Assignment1.md") && file.info("Assignment1.md")$size > 0) {
  # Read the content of Assignment1.md
  lines <- readLines("Assignment1.md")
  writeLines(c(lines, ""), "Assignment1.md")
  assignment_content <- readLines("Assignment1.md")
  
  # Add your name at the end of the file
  assignment_content <- c(assignment_content, "group name3 = Zhaoyun Fan")

  # Write the updated content back to Assignment1.md
  writeLines(assignment_content, "Assignment1.md")

  # Stage the updated file
  system("git add Assignment1.md")

  # Commit the changes with a descriptive message
  system('git commit -m "Added name to Assignment1.md: group name3 = Zhaoyun Fan"')

  # Push the changes to the remote repository
  system("git push origin main")
} else {
  cat("Assignment1.md file does not exist or is empty.\n")
}


# Reading Data
#2) Read in the .dta version and store in an object called angell_stata.
library(haven)
angell_stata <- read_dta("D://master//SURV 727//angell.dta")

#3) Read in the .txt version and store it in an object called angell_txt
angell_txt <- read.table("D://master//SURV 727//angell.txt",header = FALSE)

#4) What are the differences between angell_stata and angell_txt? Are there differences in the classes of the individual columns?
str(angell_stata)
str(angell_txt)

sapply(angell_stata, class)
sapply(angell_txt, class)
##response:The primary differences between angell_stata and angell_txt lie in the column names and the presence of metadata. However, there are no differences in the classes of the columns.

# Column Names:
#   angell_stata features original variable names: "city", "morint", "ethhet", "geomob", and "region".
# angell_txt uses default column names: "V1", "V2", "V3", "V4", and "V5".
#
# Metadata:
#   angell_stata includes Stata-specific formatting metadata (e.g., %15s for character columns), which is not present in angell_txt.
#
# Column Classes:
#   There are no differences in column classes between the two datasets. Both datasets have character for the first and fifth columns and numeric for the second, third, and fourth columns.


#5) Make any updates necessary so that angell_txt is the same as angell_stata
colnames(angell_txt) <- c( "city"  , "morint","ethhet", "geomob" ,"region")
# Simulate Stata format for character columns
angell_txt$city <- sprintf("%-15s", angell_txt$city)
angell_txt$morint <- formatC(angell_txt$morint, format = "f",width=9, digits = 1)
angell_txt$ethhet <- formatC(angell_txt$ethhet, format = "f",width=9, digits = 1)
angell_txt$geomob <- formatC(angell_txt$geomob, format = "f", width=9,digits = 1)
angell_txt$region <- sprintf("%-9s", angell_txt$region)
angell_txt$morint <- as.numeric(as.character(angell_txt$morint))
angell_txt$ethhet <- as.numeric(as.character(angell_txt$ethhet))
angell_txt$geomob <- as.numeric(as.character(angell_txt$geomob))
str(angell_txt)
str(angell_stata)


# 6)Describe the Ethnic Heterogeneity variable. Use descriptive statistics such as mean, median, standard deviation, etc. How does it differ by region?
library(dplyr)
# Descriptive statistics for Ethnic Heterogeneity
ethnic_heterogeneity_stats <- angell_txt %>%
  summarise(
    Mean = mean(ethhet, na.rm = TRUE),
    Median = median(ethhet, na.rm = TRUE),
    Standard_Deviation = sd(ethhet, na.rm = TRUE),
    )
print(ethnic_heterogeneity_stats)

# Descriptive statistics by region
ethnic_heterogeneity_by_region <- angell_txt %>%
  group_by(region) %>%
  summarise(
    Mean = mean(ethhet, na.rm = TRUE),
    Median = median(ethhet, na.rm = TRUE),
    Standard_Deviation = sd(ethhet, na.rm = TRUE),
    )
print(ethnic_heterogeneity_by_region)
# The Ethnic Heterogeneity variable varies significantly by region.
# Region N has the highest mean and median, indicating the most ethnic diversity with considerable variation. 
# Region E also shows high diversity but with less variation than Region N. 
# Region W has the lowest mean and minimal variation, reflecting the least diversity. 
# Region S exhibits the lowest mean with a narrow range, indicating relatively consistent but lower levels of ethnic diversity. 
# These differences underscore the varying levels of ethnic diversity across regions.


##describing data
#7) Install the “MASS” package, load the package. Then, load the Boston dataset
install.packages("MASS")
library(MASS)
data("Boston")
head(Boston)
str(Boston)

#8)What is the type of the Boston object?
typeof(Boston)

#9)What is the class of the Boston object?
class(Boston)

#10)How many of the suburbs in the Boston data set bound the Charles river?
num_boundaries <- sum(Boston$chas == 1)
print(num_boundaries)


#11)Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each variable.
summary(Boston$crim)
max_crim_index <- which.max(Boston$crim)
max_crim_suburb <- Boston[max_crim_index,]
max_crim_suburb
crim_range <- range(Boston$crim)
crim_range
#The suburb with the highest crime rate is 381, with a value of 88.97620.
#The range of the crime rates spans from 0.00632 to 88.97620. This large range indicates significant variation in crime rates across different subsurbs.


summary(Boston$tax)
max_tax_index <- which.max(Boston$tax)
max_tax_Suburb <-  Boston[max_tax_index,]
max_tax_Suburb
tax_range <- range(Boston$tax)
tax_range
#The suburb with the highest tax rate is 489, with a value of 711.
#The tax rates is from 187 to 711, which shows that tax rates vary considerably among the suburbs.



summary(Boston$ptratio)
max_ptratio_index <- which.max(Boston$ptratio)
max_ptratio_suburb <- Boston[max_ptratio_index,]
max_ptratio_suburb
ptratio_range <- range(Boston$ptratio)
ptratio_range
#The suburb with the highest Pupil-teacher ratios is 355, with a value of 22.0.
#The Pupil-teacher ratios is from 12.6 to 22.0.This range reflects some variation, but not as extreme as the crime rates and tax rates.

#12)Describe the distribution of pupil-teacher ratio among the towns in this data set that have a per capita crime rate larger than 1. How does it differ from towns that have a per capita crime rate smaller than 1?
class(Boston$crim)
high_ptratio <- Boston[Boston$crim>1,]
summary(high_ptratio)

low_ptratio <- Boston[Boston$crim<=1,]
low_ptratio

par(mfrow = c(1,2))  
hist(high_ptratio$ptratio, main = "Pupil-Teacher Ratio in High Crime Rate Towns",
     xlab = "Pupil-Teacher Ratio", breaks = 20)

hist(low_ptratio$ptratio, main = "Pupil-Teacher Ratio in Low Crime Rate Towns",
     xlab = "Pupil-Teacher Ratio", breaks = 20)
#The variability in pupil-teacher ratios in high crime rate towns might be more extensive, with more noticeable differences in the ratios across different towns. 
#In comparison, towns with lower crime rates might exhibit less variability and a more consistent range of pupil-teacher ratios.

## function
#13) Write a function that calculates 95% confidence intervals for a point estimate. The function
# should be called my_CI. When called with my_CI(2, 0.2), the function should print out “The
# 95% CI upper bound of point estimate 2 with standard error 0.2 is 2.392. The lower bound is
# 1.608.”
# Note: The function should take a point estimate and its standard error as arguments. You
# may use the formula for 95% CI: point estimate +/- 1.96*standard error.
# Hint: Pasting text in R can be done with: paste() and paste0()
my_CI <- function(pe,se){
  upper <- pe+1.96*se
  lower <- pe-1.96*se
  cat(paste("The 95% CI upper bound of point estimate", pe, 
            "with standard error", se, "is", round(upper, 3), 
            ". The lower bound is", round(lower, 3), ".\n"))
}
print(my_CI(2,0.2))

#14) Create a new function called my_CI2 that does that same thing as the my_CI function but
# outputs a vector of length 2 with the lower and upper bound of the confidence interval instead
# of printing out the text. Use this to find the 95% confidence interval for a point estimate of 0
# and standard error 0.4.

my_CI2 <- function(pe,se){
  upper <- pe+1.96*se
  lower <- pe-1.96*se
  return(c(lower,upper))
}
print(my_CI2(0,0.4))

#15)Update the my_CI2 function to take any confidence level instead of only 95%. Call the
# new function my_CI3. You should add an argument to your function for confidence level.
# Hint: Use the qnorm function to find the appropriate z-value. For example, for a 95% confidence
# interval, using qnorm(0.975) gives approximately 1.96.

my_CI3 <- function(pe,se,cl){
  z_value <- qnorm(1-(1-cl)/2)
  upper <- pe+z_value*se
  lower <- pe-z_value*se
  upper <- round(upper,3)
  lower <- round(lower,3)
  return(c(lower,upper))
}
print(my_CI3(2,0.2,0.95))

#16) Without hardcoding any numbers in the code, find a 99% confidence interval for Ethnic Heterogeneity in the Angell dataset. Find the standard error by dividing the standard
# deviation by the square root of the sample size.
print(angell_stata$ethhet)
n <- length(angell_stata$ethhet)
sd_ethhet <- sd(angell_stata$ethhet)
se_ethhet = sd_ethhet/sqrt(n)
mean_ethhet= mean(angell_stata$ethhet)
my_CI3(mean_ethhet,se_ethhet,0.99)

# #17) Write a function that you can apply to the Angell dataset to get 95% confidence intervals.
# The function should take one argument: a vector. Use if-else statements to output NA and
# avoid error messages if the column in the data frame is not numeric or logical.
my_CI95 <- function(vector){
  if(is.numeric(vector)||is.logical(vector)){
  n <- length(vector)
  se <- sd(vector)/sqrt(n)
  mean <- mean(vector)
  upper <- mean+1.96*se
  lower <- mean-1.96*se
  upper <- round(upper,3)
  lower <- round(lower,3)
  return(c(lower,upper))}
  else{return(NA)}
}
print(my_CI95(angell_stata$ethhet))
  