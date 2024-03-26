# --------------------------------------------
# Script Name: data_manipul.R
# Purpose: This scribes how to to illustrate some functions
#          or packages for data frame processing.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-03-25
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 1) Import and save data

library(tidyverse)

# Creating a sample data frame

df <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Name = c("Alice", "Bob", "Charlie", "Dave", "Eve"),
  Age = c(25, 32, 18, 47, 22),
  Salary = c(50000, 65000, 30000, 75000, 40000),
  stringsAsFactors = FALSE
)
print(df)

# Save the dataframe as a xlsx file
# Verify if the 'openxlsx' package is installed
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")# Install the 'openxlsx' package if it's not already installed
}
library("openxlsx") 

write.xlsx(df, file = "data/employees.xlsx", # save in a file
           colNames=TRUE, # Include column names in the Excel file
           rowNames=FALSE, # Do not include row names (X. column)
           sheetName="Sheet1",# Name the worksheet "Sheet1"
           append = FALSE)  

# Import and save data as a csv file

excel_data<- read.xlsx("data/employees.xlsx")  
print(excel_data) 
write.csv(excel_data, "data/exployees.csv")

# 2) Inspect data structure
# View the structure of the data frame
csv_data <- read.csv("data/exployees.csv")  #Load data
print(csv_data) 
print(is.data.frame(csv_data)) 
head(csv_data)
str(csv_data)

# 3）Check whether a column or row has missing data


