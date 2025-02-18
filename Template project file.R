# R Project file

# Load packages
library(gtsummary)
library(DescTools)
library(survival)
library(survminer)
library(tidyverse)
library(ggplot2)
library(stats)

# Set working directory 
setwd("C:/file_path")
set.seed(123)

# Load data
data <- read.csv('file_name.csv')
