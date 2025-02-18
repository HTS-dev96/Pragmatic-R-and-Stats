#A Pragmatic approach to R
# By Jack 

#Your R studio has 4 windows
# Console: Where you code is run and can see results, you can write code in here
# but it won't be saved
# This corner: is your R file where you write your code and it is saved
# It is also where you can view your data
# Environment: It shows all the objects (data) you have loaded into R
# Viewer: Where your graphs and tables you create 

# Adding a "#" at the start of a line means that it isn't active code
# This is how you leave notes to yourself or others

# To run code either at the end of your line of code or after highlighting 
# your code press control + enter. Or highlight your code and click "Run" at 
# the top of this window

# When you run functions you usually want to save that results
# To start, let's load some practice data

data("iris")
#Now click on <Promise> in the enviroment window
#Now you have an object called iris with data inside

#Let's look at our dataset
View(iris)
  # It has categorical and continuous variables

#Let's rename this dataset
data <- iris
  # The left pointing arrow means I want to save the results of the function
  # on the right as an object with the name on the left. 
  # This will overwrite objects with a similar name so be careful

#Now we have the same data under a new name but it shows you how to use the 
# most common part of code in R

# Selecting specific colomns or rows in R
  #For some stats functions you need to compare one column in your dataset to 
  # another or specific columns from two datasets

#Lets call the Sepal.Length column
#Method 1: dollar sign

data$Sepal.Length

#Method 2: column mumber

data[,1]
  #It doesn't come out as nice
  #In this formant [rows, columns] and if you leave it blank then it will include
  # all rows/columns

# Common issues with data type can be a column you think is saved as numeric
# is accidentally saved as a character (maybe there was a period there)
# You can check with the function str

str(data)

# We're lucky here that all variables are coded correctly 
# The factor is also correctly coded with the three levels and nice labels
# Later in this we'll code our own factor 

#R Packages: what makes R special helps you do additional functions

#Packages need to be installed first and then can be selected for use within
# each project you're doing

install.packages("gtsummary")

#Then activate it using
library(gtsummary)
  #If you quit R then you need to do this each time

# Now lets install all the packages you'll typically need

install.packages('tidyverse')
install.packages('survminer')
install.packages('DescTools')
install.packages('ggplot2')
install.packages('stats')
install.packages('survival')

library(DescTools)
library(survival)
library(survminer)
library(tidyverse)
library(ggplot2)
library(stats)

## Testing your table of covariates for a study 
  #Create a dataframe of your covariates and a variable splitting your two groups

tbl <- tbl_summary(
  data,
  include = c(Petal.Length, Petal.Width, Sepal.Length), #Variables you want in the table, can leave blank for all
  by = Species, # split table by group
  missing = "no" # don't list missing data separately
) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update the column header
  bold_labels()

#Save your table as word doc so you can edit it as needed
tbl |> 
  as_gt() |> 
  gt::gtsave(filename = "table.docx") #Title it whatever you'd like, it will go to your working directory

## Testing for normality
  #Test one variable at a time

shapiro.test(data$Sepal.Length)
  #You want the p value to be greater than 0.05

##Hypothesis testing

#t-testing

t.test(data$Sepal.Length, data$Sepal.Width, alternative = 'two.sided', paired = F)
  #t.test(variable_1, variable_2)
  # alternative can be two.sided, less, or greater
  #If your data is paired then just change it from "F" to "T"

#mann whitney u test

wilcox.test(data$Sepal.Length, data$Sepal.Width,
        alternative = 'two.sided') #Can also be less, greater

#Anova and related tests
  #Need to build the model
  #Takes the form: output ~ variable_1 + variable_2 ...

anova_fit <- aov(Sepal.Length ~ #this is your outcome
                   Species + #this are your inputs, add more with "+"
                   Petal.Width, data = data) #your dataframe goes here

summary(anova_fit)

#Chi squared test
  #For this test you need to build the contingency table but R makes it easy

table(data$Species) #this builds a table for you, you can add more categorical variables by using a comma

#To run the chi squared test you "nest" the table function inside like below
#We don't have a second categorical variable so this won't produce results 
#Just make sure all of your cells have a number greater than 10
table(data$Species, data$Category)
#If all greater than 10 use Chi squared otherwise use Fischer's
chisq.test(table(data$Species, data$Category))
fisher.test(table(data$Species, data$Category))

#Regression

glm_fit <- glm(Sepal.Length ~ #your outcome
                 Petal.Width + #Your input variables
                 Species, #You can even include catagorical variables!
               data = data, #the name of your dataframe
               family = gaussian) #This is the type of regression

summary(glm_fit)

## Changing the type of regression means just changing the family
# Binomial - 'binomial'
# Possion - 'poisson'

##Survival modeling

# We are going to need new data for this part
data(cancer, package="survival")
#Then click on lung

# Kaplan-meier curve
# Fit the model
km_fit <- survfit(Surv(time, #this is time until event or last known f/u
                       status) #This is if they had the event or not
                  ~ ph.ecog, #this is where you add your inputs, ussually 1-2  
                  data = lung) #The dataframe to find all this info

# Graph it 
km_fit %>%
  ggsurvplot(
    data = lung, #Same dataframe as above
    fun = "pct",
    xlab = 'Time (Days)', #Change to whatever your time variable is
    linetype = "strata", # Change line type by groups
    conf.int = TRUE, #this gets rid of shading around the lines
    risk.table = TRUE, #Change to false to get rid of table on the bottom
    pval = T,
    fontsize = 3, # used in risk table
    surv.median.line = "hv", # median horizontal and vertical ref lines
    ggtheme = theme_classic(),
    title = "Kaplan-Meier Survival Curve", #Write whatever you want!
    legend.title = "" #Can write in something if you want 
  )

#Cox PH model
  #Very similar set-up 
cox_fit <- coxph(Surv(time, status) ~ age + 
                        ph.ecog +
                        sex, 
                      data = lung,
                      x = T)
summary(cox_fit)

#Testing assumptions
ggcoxdiagnostics(cox_fit, type = "dfbeta", linear.predictions = FALSE)
cox_test_ph <- cox.zph(cox_fit)
ggcoxzph(cox_test_ph)
