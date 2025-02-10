# Load necessary libraries
library(ggplot2)
library(caTools)
library(dplyr)
library(ggforce)
library(gridExtra)
library(reshape2)
library(tidyr)

# Import dataset
data <- read.csv("Food_Delivery_Times.csv")

# Understanding the dataset
head(data, 4)
tail(data, 4)

# Shape of the dataset
dim(data)

# Column names
colnames(data)

# To find the sum of duplicated rows:
sum(duplicated(data))

# Check whether there are missing values
# Convert blank ("") values to NA
data[data == ""] <- NA

# Count missing values per column
missing_counts <- colSums(is.na(data))
print(missing_counts)

########################################################################################
# Spliting dataset

# Split the dataset into training and testing sets
set.seed(3003) # Set a random seed for reproducibility

# Get a random sample of 7000 rows for the training set
train_indices <- sample(1:nrow(data), 700)

# Create training and test sets using the sampled indices
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Check dimensions of the training set
dim(train_data)
dim(test_data)

########################################################################################
# Missing Value handling

# Count missing values in training test per column
missing_counts_train <- colSums(is.na(train_data))
print(missing_counts_train)

# Count missing values in test test per column
missing_counts_test <- colSums(is.na(test_data))
print(missing_counts_test)

# Calculate the mean of the 'Courier_Experience_yrs' column from the training data
Courier_mean <- mean(train_data$Courier_Experience_yrs, na.rm = TRUE)

# Impute missing values in 'Courier_Experience_yrs' in both train and test data with the mean
train_data$Courier_Experience_yrs <- ifelse(is.na(train_data$Courier_Experience_yrs), Courier_mean, train_data$Courier_Experience_yrs)
test_data$Courier_Experience_yrs <- ifelse(is.na(test_data$Courier_Experience_yrs), Courier_mean, test_data$Courier_Experience_yrs)

# Find the mode for categorical columns from the training data
Weather_mode <- names(sort(table(train_data$Weather), decreasing = TRUE))[1]
Traffic_mode <- names(sort(table(train_data$Traffic_Level), decreasing = TRUE))[1]
Time_mode <- names(sort(table(train_data$Time_of_Day), decreasing = TRUE))[1]

# Impute missing values for categorical columns in both train and test data with the mode
train_data$Weather <- ifelse(is.na(train_data$Weather), Weather_mode, train_data$Weather)
test_data$Weather <- ifelse(is.na(test_data$Weather), Weather_mode, test_data$Weather)

train_data$Traffic_Level <- ifelse(is.na(train_data$Traffic_Level), Traffic_mode, train_data$Traffic_Level)
test_data$Traffic_Level <- ifelse(is.na(test_data$Traffic_Level), Traffic_mode, test_data$Traffic_Level)

train_data$Time_of_Day <- ifelse(is.na(train_data$Time_of_Day), Time_mode, train_data$Time_of_Day)
test_data$Time_of_Day <- ifelse(is.na(test_data$Time_of_Day), Time_mode, test_data$Time_of_Day)

# Count missing values in training test per column
missing_counts_train <- colSums(is.na(train_data))
print(missing_counts_train)

# Count missing values in test test per column
missing_counts_test <- colSums(is.na(test_data))
print(missing_counts_test)

########################################################################################
###### Descriptive Analysis ######

## Univariate Analysis

# Summary for Numerical Variables
numeric_vars <- c("Distance_km", "Preparation_Time_min", "Courier_Experience_yrs", "Delivery_Time_min")

# Calculate summary statistics for numerical variables
summary_stats <- summary(train_data[numeric_vars])
print(summary_stats)

# Visualize distribution of numerical variables using histograms (unchanged)
ggplot(train_data, aes(x = Distance_km)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Distance_km", x = "Distance (km)", y = "Frequency")

ggplot(train_data, aes(x = Preparation_Time_min)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Preparation Time (minutes)", x = "Preparation Time (min)", y = "Frequency")

ggplot(train_data, aes(x = Courier_Experience_yrs)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Courier Experience (years)", x = "Experience (yrs)", y = "Frequency")

ggplot(train_data, aes(x = Delivery_Time_min)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "#4682B4", color = "black") + # Updated blue shade
  geom_density(color = "darkblue", size = 1.2) + # Add density curve
  labs(title = "Distribution of Delivery Time (minutes)", 
       x = "Delivery Time (min)", 
       y = "Density") +
  theme_minimal()
# Frequency distribution for categorical variables
Weather_freq <- table(train_data$Weather)
Traffic_freq <- table(train_data$Traffic_Level)
Time_freq <- table(train_data$Time_of_Day)
Vehicle_freq <- table(train_data$Vehicle_Type)

# Print frequency tables
cat("Weather Frequency:\n")
print(Weather_freq)

cat("\nTraffic Level Frequency:\n")
print(Traffic_freq)

cat("\nTime of Day Frequency:\n")
print(Time_freq)

cat("\nVehicle Type Frequency:\n")
print(Vehicle_freq)

# Weather Pie Chart
weather_percentages <- train_data %>%
  count(Weather) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(weather_percentages, aes(x = "", y = Percentage, fill = Weather)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Weather", fill = "Weather") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Traffic Level Pie Chart
traffic_percentages <- train_data %>%
  count(Traffic_Level) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(traffic_percentages, aes(x = "", y = Percentage, fill = Traffic_Level)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Traffic Level", fill = "Traffic Level") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Time of Day Pie Chart
time_percentages <- train_data %>%
  count(Time_of_Day) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(time_percentages, aes(x = "", y = Percentage, fill = Time_of_Day)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Time of Day", fill = "Time of Day") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Vehicle Type Pie Chart
vehicle_percentages <- train_data %>%
  count(Vehicle_Type) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(vehicle_percentages, aes(x = "", y = Percentage, fill = Vehicle_Type)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  scale_fill_brewer(palette = "Blues") + 
  labs(title = "Distribution of Vehicle Type", fill = "Vehicle Type") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


### Boxplots for multiple numerical variables 

# Select relevant columns
boxplot_data <- train_data[, c("Distance_km", "Delivery_Time_min", "Courier_Experience_yrs", "Preparation_Time_min")]

# Reshape the data into long format
boxplot_long <- boxplot_data %>%pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Create boxplots
ggplot(boxplot_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(
    title = "Boxplots for Multiple Variables",
    x = "Variable",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_brewer(palette = "Set2")


## Bivariate Analysis

# Bivariate analysis for numerical variables vs Delivery Time

# Scatter plot for Delivery Time vs Distance
ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Delivery Time vs Distance", x = "Distance (km)", y = "Delivery Time (min)")

# Scatter plot for Delivery Time vs Preparation Time
ggplot(train_data, aes(x = Preparation_Time_min, y = Delivery_Time_min)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Delivery Time vs Preparation Time", x = "Preparation Time (min)", y = "Delivery Time (min)")

# Scatter plot for Delivery Time vs Courier Experience
ggplot(train_data, aes(x = Courier_Experience_yrs, y = Delivery_Time_min)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Delivery Time vs Courier Experience", x = "Courier Experience (years)", y = "Delivery Time (min)")

# Box plot for Delivery Time vs Preparation Time with different colors
ggplot(train_data, aes(x = Preparation_Time_min, y = Delivery_Time_min, fill = as.factor(Preparation_Time_min))) +
  geom_boxplot(color = "black") +
  scale_fill_viridis_d() +  # Use a predefined color scale for continuous data
  labs(title = "Delivery Time vs Preparation Time", x = "Preparation Time (min)", y = "Delivery Time (min)") +
  theme_minimal()

# Box plot for Delivery Time vs Courier Experience with different colors
ggplot(train_data, aes(x = Courier_Experience_yrs, y = Delivery_Time_min, fill = as.factor(Courier_Experience_yrs))) +
  geom_boxplot(color = "black") +
  scale_fill_viridis_d() +  # Use a predefined color scale for continuous data
  labs(title = "Delivery Time vs Courier Experience", x = "Courier Experience (years)", y = "Delivery Time (min)") +
  theme_minimal()

# Bivariate analysis for categorical variables vs Delivery Time

# Box plot for Delivery Time vs Weather with different colors for categories

train_data$Weather <- factor(train_data$Weather, levels = c("Clear", "Windy", "Rainy", "Foggy", "Snowy"), ordered = TRUE)

ggplot(train_data, aes(x = Weather, y = Delivery_Time_min, fill = Weather)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c(
    "Clear" = "skyblue",   
    "Windy" = "lightcyan", 
    "Foggy" = "lightgray", 
    "Rainy" = "dodgerblue", 
    "Snowy" = "white"       
  ))  + # Customize colors
  labs(title = "Delivery Time vs Weather", x = "Weather", y = "Delivery Time (min)") +
  theme_minimal()

# Box plot for Delivery Time vs Traffic Level with different colors for categories

# Convert Traffic_Level to an ordered factor (Low < Medium < High)
train_data$Traffic_Level <- factor(train_data$Traffic_Level, levels = c("Low", "Medium", "High"), ordered = TRUE)

ggplot(train_data, aes(x = Traffic_Level, y = Delivery_Time_min, fill = Traffic_Level)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("High" = "blue", "Medium" = "lightblue", "Low" = "grey")) + # Customize colors
  labs(title = "Delivery Time vs Traffic Level", x = "Traffic Level", y = "Delivery Time (min)") +
  theme_minimal()

# Box plot for Delivery Time vs Time of Day with different colors for categories
ggplot(train_data, aes(x = Time_of_Day, y = Delivery_Time_min, fill = Time_of_Day)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightcyan", "Evening" = "white", "Night" = "gray")) + # Customize colors
  labs(title = "Delivery Time vs Time of Day", x = "Time of Day", y = "Delivery Time (min)") +
  theme_minimal()

# Box plot for Delivery Time vs Vehicle Type with different colors for categories
ggplot(train_data, aes(x = Vehicle_Type, y = Delivery_Time_min, fill = Vehicle_Type)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) + # Customize colors
  labs(title = "Delivery Time vs Vehicle Type", x = "Vehicle Type", y = "Delivery Time (min)") +
  theme_minimal()


# Bar graph for Vehicle Type vs Weather
ggplot(train_data, aes(x = Weather, fill = Vehicle_Type)) +
  geom_bar(position = "dodge") +  # Dodge position for side-by-side bars
  scale_fill_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) +  # Customize colors
  labs(title = "Vehicle Type vs Weather", x = "Weather", y = "Count of Vehicle Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Bar graph for Vehicle Type vs Traffic Level
ggplot(train_data, aes(x = Traffic_Level, fill = Vehicle_Type)) +
  geom_bar(position = "dodge") +  # Dodge position for side-by-side bars
  scale_fill_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) +  # Customize colors
  labs(title = "Vehicle Type vs Traffic Level", x = "Traffic Level", y = "Count of Vehicle Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


## Trivariate Analysis

# Create a new column that combines Vehicle_Type and Traffic_Level
train_data$Vehicle_Traffic <- paste(train_data$Vehicle_Type, train_data$Traffic_Level, sep = "_")

# Convert Traffic_Level to an ordered factor (Low < Medium < High)
train_data$Traffic_Level <- factor(train_data$Traffic_Level, levels = c("Low", "Medium", "High"), ordered = TRUE)

# Box plot for Delivery Time vs Traffic Level with Vehicle Type as a color category
ggplot(train_data, aes(x = Traffic_Level, y = Delivery_Time_min, fill = Vehicle_Type)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) + # Customize colors for Vehicle Type
  labs(title = "Delivery Time vs Traffic Level and Vehicle Type", x = "Traffic Level", y = "Delivery Time (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Box plot for Delivery Time vs Weather with Vehicle_Type 
ggplot(train_data, aes(x = Weather, y = Delivery_Time_min, fill = Vehicle_Type)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) + # Customize colors
  labs(title = "Delivery Time vs Weather by Vehicle Type", x = "Weather", y = "Delivery Time (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# Scatterplot for Delivery Time and Distance vs Vehicle Type
ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min, color = Vehicle_Type)) +
  geom_point(size = 3) +  # Add points to the scatterplot
  scale_color_manual(values = c("Bike" = "purple", "Scooter" = "yellowgreen", "Car" = "pink")) +  # Customize colors
  labs(title = "Delivery Time vs Distance by Vehicle Type", x = "Distance (km)", y = "Delivery Time (min)") +
  theme_minimal() +
  theme(legend.position = "top")  # Position legend at the top

# Ensure Time_of_Day is ordered correctly
train_data$Time_of_Day <- factor(train_data$Time_of_Day, levels = c("Morning", "Afternoon", "Evening", "Night"))

# Scatterplot: Delivery Time vs Distance, grouped by Time of Day
ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min, color = Time_of_Day)) +
  geom_point(alpha = 0.9, size = 3) + # Add scatter points with transparency and size
  scale_color_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightgreen", "Evening" = "lightcoral", "Night" = "gray")) + # Custom colors
  labs(
    title = "Delivery Time vs Distance by Time of Day",
    x = "Distance (km)",
    y = "Delivery Time (min)",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    axis.text = element_text(color = "grey20"),
    legend.position = "right"
  )

# Scatterplot: Delivery Time vs Distance, grouped by Weather
ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min, color = Weather)) +
  geom_point(alpha = 0.7, size = 3) + # Add scatter points with transparency and size
  scale_color_manual(values = c("Clear" = "yellow", "Windy" = "blue", "Foggy" = "gray", "Rainy" = "green", "Snowy" = "red")) + # Custom colors
  labs(
    title = "Delivery Time vs Distance by Weather",
    x = "Distance (km)",
    y = "Delivery Time (min)",
    color = "Weather"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    axis.text = element_text(color = "grey20"),
    legend.position = "right"
  )

# Scatterplot: Delivery Time vs Distance, grouped by Traffic Level
ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min, color = Traffic_Level)) +
  geom_point(alpha = 0.7, size = 3) + # Add scatter points with transparency and size
  scale_color_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red")) + # Custom colors
  labs(
    title = "Delivery Time vs Distance by Traffic Level",
    x = "Distance (km)",
    y = "Delivery Time (min)",
    color = "Traffic Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
    axis.text = element_text(color = "grey20"),
    legend.position = "right"
  )



# Scatter plot for Delivery Time vs Preparation Time, with color based on Traffic Level
ggplot(train_data, aes(x = Preparation_Time_min, y = Delivery_Time_min, color = Traffic_Level)) +
  geom_point(size = 2, alpha = 0.7) +  # Add points with size and transparency
  scale_color_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red")) + # Customize colors for Traffic Level
  labs(title = "Delivery Time vs Preparation Time", 
       x = "Preparation Time (min)", 
       y = "Delivery Time (min)", 
       color = "Traffic Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatterplot: Delivery Time vs Preparation Time, grouped by Weather
ggplot(train_data, aes(x = Preparation_Time_min, y = Delivery_Time_min, color = Weather)) +
  geom_point(alpha = 0.7, size = 3) + # Add scatter points with transparency and size
  scale_color_manual(values = c("Clear" = "yellow", "Windy" = "blue", "Foggy" = "gray", "Rainy" = "green", "Snowy" = "red")) + # Custom colors
  labs(
    title = "Delivery Time vs Preparation Time by Weather",
    x = "Preparation Time (min)",
    y = "Delivery Time (min)",
    color = "Weather"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold the title
    axis.text = element_text(color = "grey20"),
    legend.position = "right"
  )

ggplot(train_data, aes(x = Distance_km, y = Delivery_Time_min, 
                       color = Traffic_Level, size = Courier_Experience_yrs)) +
  geom_point(alpha = 0.3) +  # Add scatter points with transparency and size
  scale_color_manual(values = c("Low" = "green", "Medium" = "orange", "High" = "red")) +  # Custom colors
  labs(
    x = "Distance (km)",
    y = "Delivery Time (min)",
    color = "Traffic Level",
    size = "Courier Experience (yrs)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title (if title is added)
    axis.text = element_text(color = "grey20"),
    legend.position = "right"
  )


# Bin Courier_Experience_yrs into categories

# Adjust the breaks and labels as needed for your data
train_data$Experience_bin <- cut(train_data$Courier_Experience_yrs, 
                                 breaks = c(0, 1, 3, 5, Inf), 
                                 labels = c("0-1", "1-3", "3-5", "5+"),
                                 right = FALSE)



# Boxplot Delivery Time vs Traffic Level by Courier Experience
ggplot(train_data, aes(x = Traffic_Level, y = Delivery_Time_min, fill = Experience_bin)) +
  geom_boxplot(position = position_dodge(width = 0.8), color = "black") +
  labs(title = "Delivery Time vs Traffic Level by Courier Experience",
       x = "Traffic Level",
       y = "Delivery Time (min)",
       fill = "Courier Experience (yrs)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####################################################################################


# Subset the dataset for specific numerical variables
selected_vars <- data[, c("Distance_km", "Courier_Experience_yrs", "Preparation_Time_min","Delivery_Time_min")]

# Compute the correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")

cor_melt <- melt(cor_matrix)

# Create a heatmap
library(ggplot2)
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "grey", high = "dodgerblue", mid = "lightcyan",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "Variables", y = "Variables")


#################################################################################
# principal component analysis and partial least square regression
library(mdatools)

xtrain<-train_data[,c(2,7,8)]
ytrain<-train_data[,9,drop = FALSE]

xtest<-test_data[,c(2,7,8)]
ytest<-test_data[,9,drop = FALSE]

pca_model <- pca(xtrain, scale = TRUE)
summary(pca_model)
plot(pca_model)
plotScores(pca_model, show.labels = FALSE)
plotLoadings(pca_model, show.labels = TRUE)


Model<-pls(xtrain, ytrain, scale = TRUE,cv=1,
           info = "Delivery time prediction model")

summary(Model)

plot(Model)
plotXScores(Model,show.labels = FALSE)
plotXYLoadings(Model,show.labels =TRUE,legend=FALSE)

