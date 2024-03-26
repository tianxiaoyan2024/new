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
write.csv(excel_data, "data/employees.csv")

# 2) Inspect data structure

# View the structure of the data frame

csv_data <- read.csv("data/exployees.csv")  #Load data
print(csv_data) 
print(is.data.frame(csv_data)) 
head(csv_data)
str(csv_data)

# 3）Check whether a column or row has missing data

# Check for missing values in a specific column (e.g., column "Age")

is.na(df$Age)

any_missing <- any(is.na(df$Age))
print(paste("Does 'Age' have missing data?", any_missing))

# Check for missing values in the entire data frame
any_missing_in_data <- any(is.na(df))
cat("Does the data frame have missing data?", any_missing_in_data)

# 4) Extract values from a column or select/add a column

# Extract values from a column (e.g., column "Name")

library("tidyverse")  # load the tidyverse packages, incl. dplyr
column_values <- df$Name
print(df$Name)

# Select specific columns
selected_data <- select(df, Age, Salary)
print(selected_data)

# Add a new column based on existing columns
df1 <- mutate(df, Total = Age + Salary)

# 5) Transform a wider table to a long format

library(tidyverse)
long_df <- df %>%
  gather(key = "Category", value = "value", -ID, -Name) 
str(long_df)

#Note: In newer versions of the 'tidyr' package, the 'gather()' and 'spread()' functions 
#      have been replaced by 'pivot longer()'and 'pivot wider()'.There is an example to use
#      'pivot longer()' for reshaping data

wide_df <- read.csv("data/employees.csv")
long_df <- wide_df %>%
  pivot_longer(cols = -c(ID, Name),  # Select all columns except ID and Name
               names_to = "Category",  # The new column name to store the original column names
               values_to = "Value")  # he new column name to store the corresponding values
str(long_df)


# 6) Visualize the data 
p <- ggplot(df, aes(x = Age, y = Salary, color = factor(ID))) +
  geom_point() +  # Add scatter points to the plot
  labs(title = "Age vs. Salary",  # Add a title to the plot
       x = "Age",                 # Label the x-axis
       y = "Salary") +         # Label the y-axis
  theme_minimal()           # Use a minimal theme for the plot
  
# Print the plot to display it
print(p)
