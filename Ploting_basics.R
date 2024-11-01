# Loading a dataset
data(mtcars)
data(mtcars)
summary(mtcars)
(mtcarsW$mpg,mtcars$wt, main="MPG vs Weight", xlab="ojon", ylab="Miles/Gallon")
# Simple analysis
summary(mtcars$mpg)
# Basic plot
plot(mtcars$mpg,type ="l", mtcars$wt, main="MPG vs Weight", xlab="Weight", ylab="Miles Per Gallon")


library(dplyr)
library(ggplot2)

# Assuming 'data' has already been loaded with read.csv()
# Update the file path as per your environment
# data <- read.csv("path/to/ID00150_observed.csv")

# Inspect the structure (done in R console with str(data))

# Summary of data to get an overview (done in R console with summary(data))

# Data cleaning and transformation
# We'll remove rows with NA values in 'prcp' or 'tmax', and create a new column 'double_prcp'
cleaned_data <- data %>%
  filter(!is.na(prcp) & !is.na(tmax)) %>%
  mutate(double_prcp = prcp * 2)

# Simple analysis and visualization
# Plotting 'tmax' against the new 'double_prcp' column
ggplot(cleaned_data, aes(x=double_prcp, y=tmax)) + 
  geom_point() +
  labs(title="Maximum Temperature vs. Double Precipitation",
       x="Double Precipitation (2 * prcp)",
       y="Maximum Temperature (tmax)") +
  theme_minimal()


# Example: Basic Data Frame Operations
data <- data.frame(
  Name = c("John", "Doe", "Jane", "Doe"),
  Age = c(23, 34, 45, 56),
  Salary = c(50000, 60000, 70000, 80000)
)
View(data)

# Accessing data frame columns
data$Age

# Subsetting rows based on a condition
subset_data <- subset(data, Age > 30)

# Basic plot
library(ggplot2)
ggplot(subset_data, aes(x=Name, y=Salary)) + geom_bar(stat="identity")+
  theme_light()
# Module 2, Lesson 2: Data Manipulation with dplyr
# Comprehensive Lecture Note with Example Codes

# Before we start, make sure you have dplyr and readr installed and loaded
# install.packages("dplyr")
# install.packages("readr")
library(dplyr)
library(readr)

# Introduction to dplyr
# dplyr is a part of the tidyverse suite designed for data manipulation in R.
# It provides a set of verbs like filter(), select(), mutate(), summarise(), and arrange()
# for performing basic data manipulation tasks in a more readable and intuitive way.

# Loading the Dataset
# Replace "path/to/ID00150_observed.csv" with the actual path to the dataset
data_path <- "path/to/ID00150_observed.csv"
data <- read_csv(data_path)

# Ensure the dataset is loaded correctly by examining the first few rows
head(data)

# 1. Filtering Data
# Let's filter the dataset for observations in the year 1980 with non-zero precipitation.
filtered_data <- data %>%
  filter(year == 1980, prcp > 0)
# Display the first few rows of the filtered data
head(filtered_data)

# 2. Selecting Columns
# Now, we'll select only the date columns and temperature columns.
selected_columns <- data %>%
  select(year, mon, day, tmax, tmin)
# View the structure of the selected columns
str(selected_columns)

# 3. Creating and Transforming Variables
# We'll add a new column that calculates the average temperature for each day.
transformed_data <- data %>%
  mutate(tavg = (tmax + tmin) / 2)
# Check the first few rows to see the new column
head(transformed_data)

# 4. Summarising and Grouping Data
# Calculate the average maximum temperature for each month of 1980.
monthly_avg_tmax <- data %>%
  filter(year == 1980) %>%
  group_by(mon) %>%
  summarise(avg_tmax = mean(tmax, na.rm = TRUE))
# Display the average maximum temperature by month
print(monthly_avg_tmax)

# 5. Arranging Data
# Arrange the dataset by descending maximum temperature.
arranged_data <- data %>%
  arrange(desc(tmax))
# View the first few rows of the arranged data
head(arranged_data)

# 6. Joining Data Frames (Example Setup)
# Assuming you have another dataframe named monthly_rainfall
# For the sake of this example, let's create a mock dataframe
monthly_rainfall <- tibble(
  year = c(1980, 1980),
  mon = c(1, 2),
  avg_prcp = c(10, 20)
)
monthly_rainfall
# Perform a left join with our main data to add average precipitation
joined_data <- data %>%
  left_join(monthly_rainfall, by = c("year", "mon"))
# View the first few rows of the joined data
head(joined_data)

# Conclusion:
# In this lesson, we've covered essential dplyr functions for data manipulation,
# including filtering, selecting, mutating, summarising, arranging, and joining data.
# These functions are foundational for any data analysis tasks in R, providing a
# powerful yet intuitive framework for manipulating and analyzing data.
