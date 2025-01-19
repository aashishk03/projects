#readxl is used for reading Excel files in R.
library(readxl)
#dplyr is used for data manipulation tasks like filtering, mutating, and renaming columns.
library(dplyr)

# File Path
file_path <- 'D:/Alberta/STAT/Project/Data/pwt1001.xlsx'

# Read Excel File
#Legend has the full name of the various variables in the dataset
Legend <- read_excel(file_path, sheet = "Legend")
df <- read_excel(file_path, sheet = "Data")
print(head(df))

# Filter Pakistan
pak_rows <- df %>% filter(countrycode == 'PAK')
print(pak_rows)

# Handle Missing Values by replace empty values with mean values of the respective column
# Compute column means for numeric columns only
numeric_cols <- sapply(pak_rows, is.numeric)
column_means <- colMeans(pak_rows[, numeric_cols], na.rm = TRUE)

# Replace NA values in numeric columns with their respective means
df_filled <- pak_rows %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), column_means[cur_column()], .)))

# Replace NA values in non-numeric columns with the column mean conceptually for numeric applicability
clean_data <- df_filled %>% 
  mutate(across(where(~ !is.numeric(.)), ~ ifelse(is.na(.), as.character(mean(as.numeric(.), na.rm = TRUE)), .)))

print(clean_data)

# Check Data for Missing Values
print(colSums(is.na(clean_data)))

# Select Data
selected_columns <- clean_data %>% 
  select(year, country, countrycode, pop, emp, avh, hc, rgdpna, rgdpe)
print(selected_columns)

# Renaming Variables as per Legend Definition
renamed_selected_columns <- selected_columns %>% 
  rename(
    `Population (in millions)` = pop,
    `Number of persons engaged (in millions)` = emp,
    `Average annual hours worked by persons engaged` = avh,
    `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9` = hc,
    `Real GDP at constant 2017 national prices (in mil. 2017US$)` = rgdpna,
    `Expenditure-side real GDP at chained PPPs (in mil. 2017US$)` = rgdpe,
  )
print(renamed_selected_columns)

library(ggplot2)
library(openxlsx)  # For writing to Excel

file_path2 <- 'D:/Alberta/STAT/Project/Data/P_Data_Extract_From_World_Development_Indicators.xlsx'

# Read the Excel File
df2 <- read_excel(file_path2)
print(head(df2))

# Filter df2 to include only rows where "Country Name" is "Pakistan"
df2 <- df2 %>% filter(`Country Name` == "Pakistan")
print(head(df2))

# Transpose df2
df2_transposed <- as.data.frame(t(df2))

# Adjust column names after transposing 
colnames(df2_transposed) <- df2_transposed[3, ]  # Use the third row as column names
df2_transposed <- df2_transposed[-c(1:4), ]      # Remove the first four rows

# Drop the "Year" column if it exists
df2_transposed$Year <- NULL

df2_transposed$year <- 1960:2023

print(head(df2_transposed))


# Combine with renamed_selected_columns 
df3 <- full_join(renamed_selected_columns, df2_transposed, by = "year")
print(df3)

# Write the combined data to an Excel file
write.xlsx(df3, 'output2.xlsx')

# Print column names
print(colnames(df3))


library(ggplot2)
library(scales)  # For formatting

# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Real GDP at constant 2017 national prices (in mil. 2017US$)`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Expenditure-side real GDP at chained PPPs (in mil. 2017US$)",
    title = "Real GDP of Pakistan over the years 1950 to 2019"
  ) +
  xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points





# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Population (in millions)`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Population (in millions)",
    title = "Population of Pakistan over the years 1950 to 2019"
  ) +
  xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points


# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Number of persons engaged (in millions)`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Number of persons engaged (in millions)",
    title = "Number of persons engaged in Pakistan over the years 1950 to 2019"
  ) +
  xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points



# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Average annual hours worked by persons engaged`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Average annual hours worked by persons engaged",
    title = "Average annual hours worked by persons engaged in Pakistan over the years 1950 to 2019"
  ) +
  xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points



# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "HCI, based on years of schooling and returns to education",
    title = "Human capital index of Pakistan over the years 1950 to 2019"
  ) +
  xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points


# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Real GDP at constant 2017 national prices (in mil. 2017US$)`)) +
    geom_line(color = "blue", linewidth = 1) +  # Line style and color
    theme_minimal() +
    theme(
        panel.grid.major = element_line(linetype = "dashed", color = "grey"),
        plot.title = element_text(hjust = 0.5)  # Center the title
    ) +
    labs(
        x = "Year",
        y = "Real GDP at constant 2017 national prices (in mil. 2017US$)",
        title = "Real GDP at constant 2017 national prices of Pakistan over the years 1950 to 2019"
    ) +
    xlim(1950, 2019) +  # Set the x-axis limits from 1950 to 2019
    scale_y_continuous(labels = scales::label_number(accuracy = 0.1))  # Format y-axis in decimal points


# Ensure the column is numeric and replace NA with the mean
df3$`Unemployment, total (% of total labor force) (modeled ILO estimate)` <- as.numeric(df3$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)

# Replace NA values with the mean of the respective column
mean_unemployment <- mean(df3$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, na.rm = TRUE)
df3$`Unemployment, total (% of total labor force) (modeled ILO estimate)`[is.na(df3$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)] <- mean_unemployment

# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Unemployment, total (% of total labor force) (modeled ILO estimate)`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Unemployment",
    title = "Unemployment in Pakistan over the years 1991 to 2022"
  ) +
  xlim(1991, 2022) +  # Set the x-axis limits from 1991 to 2022
  ylim(0, 7)  # Set the y-axis limits from 0 to 7



# Ensure the column is numeric and replace NA with the mean
df3$`Unemployment, male (% of male labor force) (national estimate)` <- as.numeric(df3$`Unemployment, male (% of male labor force) (national estimate)`)

# Replace NA values with the mean of the respective column
mean_unemployment_male <- mean(df3$`Unemployment, male (% of male labor force) (national estimate)`, na.rm = TRUE)
df3$`Unemployment, male (% of male labor force) (national estimate)`[is.na(df3$`Unemployment, male (% of male labor force) (national estimate)`)] <- mean_unemployment_male

# Data visualization using ggplot2
ggplot(df3, aes(x = year, y = `Unemployment, male (% of male labor force) (national estimate)`)) +
  geom_line(color = "blue", linewidth = 1) +  # Line style and color
  theme_minimal() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "grey"),
    plot.title = element_text(hjust = 0.5)  # Center the title
  ) +
  labs(
    x = "Year",
    y = "Unemployment Male",
    title = "Unemployment Male in Pakistan over the years 1991 to 2022"
  ) +
  xlim(1990, 2019) +  # Set the x-axis limits from 1990 to 2019
  ylim(0, 7)  # Set the y-axis limits from 0 to 7



# Filter df3 to include only rows where the 'year' column is between 1991 and 2019
df4 <- df3 %>% filter(year >= 1991 & year <= 2019)

# View the first few rows of df4 to confirm
print(head(df4))





# Calculate percentage change for the unemployment columns
df4$`Unemployment, total (% of total labor force) (modeled ILO estimate) Change` <- 
  c(NA, diff(df4$`Unemployment, total (% of total labor force) (modeled ILO estimate)`)/head(df4$`Unemployment, total (% of total labor force) (modeled ILO estimate)`, -1) * 100)

df4$`Unemployment, male (% of male labor force) (national estimate) Change` <- 
  c(NA, diff(df4$`Unemployment, male (% of male labor force) (national estimate)`)/head(df4$`Unemployment, male (% of male labor force) (national estimate)`, -1) * 100)

# Replace NA values with 0
df4$`Unemployment, total (% of total labor force) (modeled ILO estimate) Change` <- 
  replace(df4$`Unemployment, total (% of total labor force) (modeled ILO estimate) Change`, is.na(df4$`Unemployment, total (% of total labor force) (modeled ILO estimate) Change`), 0)

df4$`Unemployment, male (% of male labor force) (national estimate) Change` <- 
  replace(df4$`Unemployment, male (% of male labor force) (national estimate) Change`, is.na(df4$`Unemployment, male (% of male labor force) (national estimate) Change`), 0)

# Create df5 with year, country and the unemployment percentage change columns
df5 <- df4 %>% select(year, country, 
                      `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`,
                      `Unemployment, male (% of male labor force) (national estimate) Change`)

# View the first few rows of df5 to confirm
print(head(df5))




# Calculate percentage change for the selected renamed columns
df4$`Population (in millions) Change` <- 
  c(NA, diff(df4$`Population (in millions)`)/head(df4$`Population (in millions)`, -1) * 100)

df4$`Number of persons engaged (in millions) Change` <- 
  c(NA, diff(df4$`Number of persons engaged (in millions)`)/head(df4$`Number of persons engaged (in millions)`, -1) * 100)

df4$`Average annual hours worked by persons engaged Change` <- 
  c(NA, diff(df4$`Average annual hours worked by persons engaged`)/head(df4$`Average annual hours worked by persons engaged`, -1) * 100)

df4$`Human capital index, based on years of schooling and returns to education Change` <- 
  c(NA, diff(df4$`Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`)/head(df4$`Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`, -1) * 100)

df4$`Real GDP at constant 2017 national prices (in mil. 2017US$) Change` <- 
  c(NA, diff(df4$`Real GDP at constant 2017 national prices (in mil. 2017US$)`)/head(df4$`Real GDP at constant 2017 national prices (in mil. 2017US$)`, -1) * 100)

df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$) Change` <- 
  c(NA, diff(df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$)`)/head(df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$)`, -1) * 100)

# Replace NA values with 0
df4$`Population (in millions) Change` <- 
  replace(df4$`Population (in millions) Change`, is.na(df4$`Population (in millions) Change`), 0)

df4$`Number of persons engaged (in millions) Change` <- 
  replace(df4$`Number of persons engaged (in millions) Change`, is.na(df4$`Number of persons engaged (in millions) Change`), 0)

df4$`Average annual hours worked by persons engaged Change` <- 
  replace(df4$`Average annual hours worked by persons engaged Change`, is.na(df4$`Average annual hours worked by persons engaged Change`), 0)

df4$`Human capital index, based on years of schooling and returns to education Change` <- 
  replace(df4$`Human capital index, based on years of schooling and returns to education Change`, is.na(df4$`Human capital index, based on years of schooling and returns to education Change`), 0)

df4$`Real GDP at constant 2017 national prices (in mil. 2017US$) Change` <- 
  replace(df4$`Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, is.na(df4$`Real GDP at constant 2017 national prices (in mil. 2017US$) Change`), 0)

df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$) Change` <- 
  replace(df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$) Change`, is.na(df4$`Expenditure-side real GDP at chained PPPs (in mil. 2017US$) Change`), 0)

# Create df6 with the year, country and the percentage change columns
df6 <- df4 %>% select(year, country, 
                      `Population (in millions) Change`,
                      `Number of persons engaged (in millions) Change`,
                      `Average annual hours worked by persons engaged Change`,
                      `Human capital index, based on years of schooling and returns to education Change`,
                      `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`,
                      `Expenditure-side real GDP at chained PPPs (in mil. 2017US$) Change`)

# View the first few rows of df6 to confirm
print(head(df6))


# Combine df5 and df6 by merging on 'year' and 'country' columns to ensure there's only one year and country column
df7 <- left_join(df5, df6, by = c("year", "country"))




# Create scatter plot
ggplot(df7, aes(x = `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`, 
                y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`)) +  
  geom_point(color = "blue") + 
  theme_minimal() +
  labs(
    x = "Unemployment Rate (%)",
    y = "Real GDP Growth Rate (%)",
    title = "Scatter Plot of Real GDP Growth Rate vs Unemployment Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



# Create scatter plot with line of best fit and legend
ggplot(df7, aes(x = `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`, 
                y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`)) +  
  geom_point(aes(color = "Data Points"), size = 2) +  # Add data points
  geom_smooth(method = "lm", aes(color = "Best Fit Line"), se = FALSE, size = 1) +  # Add best-fit line
  theme_minimal() +
  labs(
    x = "Unemployment Rate (%)",
    y = "Real GDP Growth Rate (%)",
    title = "Scatter Plot of Real GDP Growth Rate vs Unemployment Rate",
    color = "Legend"  # Legend title
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"  # Position the legend at the bottom
  ) +
  scale_color_manual(
    values = c("Data Points" = "blue", "Best Fit Line" = "red")  # Define colors for legend items
  )





# Create scatter plot
ggplot(df7, aes(x = `Unemployment, male (% of male labor force) (national estimate) Change`, 
                y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`)) +
  geom_point(color = "blue") + 
  theme_minimal() +
  labs(
    x = "Unemployment Male (%) Change",
    y = "Real GDP Growth Rate (%) Change",
    title = "Scatter Plot of Real GDP Growth Rate vs Unemployment Male Rate"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



# Create scatter plot with line of best fit and legend
ggplot(df7, aes(x = `Unemployment, male (% of male labor force) (national estimate) Change`, 
                y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`)) +
  geom_point(aes(color = "Data Points"), size = 2) +  # Add data points
  geom_smooth(method = "lm", aes(color = "Best Fit Line"), se = FALSE, size = 1) +  # Add best-fit line
  theme_minimal() +
  labs(
    x = "Unemployment Male (%) Change",
    y = "Real GDP Growth Rate (%) Change",
    title = "Scatter Plot of Real GDP Growth Rate vs Unemployment Male Rate",
    color = "Legend"  # Legend title
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"  # Position the legend at the bottom
  ) +
  scale_color_manual(
    values = c("Data Points" = "blue", "Best Fit Line" = "red")  # Define colors for legend items
  )





# Combine relevant columns from df4 and df7 to create df8
df8 <- df4 %>%
  select(year, country, 
         `Population (in millions)`, 
         `Number of persons engaged (in millions)`, 
         `Average annual hours worked by persons engaged`, 
         `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`,
         `Expenditure-side real GDP at chained PPPs (in mil. 2017US$)`) %>%  
  left_join(df7 %>%
              select(year, 
                     `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`,
                     `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`,
                     `Unemployment, male (% of male labor force) (national estimate) Change`), 
            by = "year")

# View the new dataframe
head(df8)








model <- lm(`Real GDP at constant 2017 national prices (in mil. 2017US$) Change` ~ 
              `Unemployment, total (% of total labor force) (modeled ILO estimate) Change` + 
              `Unemployment, male (% of male labor force) (national estimate) Change` + 
              `Population (in millions)` + 
              `Number of persons engaged (in millions)` + 
              `Average annual hours worked by persons engaged` + 
              `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`, 
            data = df8)

# View the model summary
summary(model)



# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Population and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Population (in millions)`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Population (in millions)`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Population (in millions)",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Population)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )




# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Number of persons engaged (in millions) and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Number of persons engaged (in millions)`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Number of persons engaged (in millions)`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Number of persons engaged (in millions)",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Persons Engaged)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )


# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Average annual hours worked by persons engaged and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Average annual hours worked by persons engaged`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Average annual hours worked by persons engaged`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Average Annual Hours Worked by Persons Engaged",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Average Annual Hours Worked)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )



# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Human capital index and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Human capital index, based on years of schooling and returns to education; see Human capital in PWT9`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Human Capital Index (based on years of schooling and returns to education)",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Human Capital Index)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )



# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Expenditure-side real GDP and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Expenditure-side real GDP at chained PPPs (in mil. 2017US$)`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Expenditure-side real GDP at chained PPPs (in mil. 2017US$)`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Expenditure-side Real GDP (in mil. 2017US$)",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Expenditure-side Real GDP)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )


# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Unemployment (total) Change and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Unemployment, total (% of total labor force) (modeled ILO estimate) Change`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Unemployment Rate (total, %) Change",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Unemployment Rate, total)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )


# Create a data frame with predictions
df8$predicted_gdp <- predict(model)

# Plot the actual vs predicted GDP with Unemployment (male) Change and a legend
ggplot() +
  geom_point(data = df8, 
             aes(x = `Unemployment, male (% of male labor force) (national estimate) Change`, 
                 y = `Real GDP at constant 2017 national prices (in mil. 2017US$) Change`, 
                 color = "Actual Data"), 
             alpha = 0.5) +  # Actual data points
  geom_line(data = df8, 
            aes(x = `Unemployment, male (% of male labor force) (national estimate) Change`, 
                y = predicted_gdp, 
                color = "Predicted Line"), 
            size = 1) +  # Predicted line
  theme_minimal() +
  scale_color_manual(
    name = "Legend", 
    values = c("Actual Data" = "blue", "Predicted Line" = "red")
  ) +
  labs(
    x = "Unemployment Rate (male, %) Change",
    y = "Real GDP Growth Rate (%)",
    title = "Predicted vs Actual Real GDP Growth Rate (Unemployment Rate, male)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"  # Adjust legend position as needed
  )


