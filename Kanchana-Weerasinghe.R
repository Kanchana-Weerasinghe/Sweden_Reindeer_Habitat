library(corrplot)
library(ggplot2)
library(reshape2)
library(fmsb)
library(dplyr)
library(fmsb)

library(randomForest)
library(rpart.plot)

set.seed(123)

current_dir <- getwd()
file_path <- file.path(current_dir, "Reindeer.csv")

# Read the dataset
data <- read.csv(file_path)
data$Pallet_Combination <- ifelse(data$Pellet_Found_2009 == 1 & data$Pellet_Found_2010 == 1, "Both",
                                     ifelse(data$Pellet_Found_2009 == 1, "2009",
                                              ifelse(data$Pellet_Found_2010 == 1, "2010", "None")))

#######################  Draw radar chart ###########################


draw_radar_chart <- function(radar_data) {
  
  # Define function for min-max scaling
  min_max_scaling <- function(x) {
    scaled_value <- (x - min(x)) / (max(x) - min(x))
    scaled_value * 99 + 1  # Adjust scale to range from 1 to 100
  }
  
  # Apply min-max scaling to each variable
  min_max_scalig <- data.frame(
    Ele = min_max_scaling(radar_data$Elevation),
    Slp = min_max_scaling(radar_data$Slope),
    RugI = min_max_scaling(radar_data$Ruggedness_Index),
    FAS = min_max_scaling(radar_data$Forest_Age_Structure),
    DPL = min_max_scaling(radar_data$Dis_Power_Lines),
    DAR = min_max_scaling(radar_data$Dis_All_Roads),
    DBR = min_max_scaling(radar_data$Dis_All_Big_Roads),
    DMI = min_max_scaling(radar_data$Dis_To_Mine)
  )
  
  # Extract min-max values
  min_max_values <- data.frame(
    Ele = c(min(min_max_scalig$Ele), max(min_max_scalig$Ele)),
    Slp = c(min(min_max_scalig$Slp), max(min_max_scalig$Slp)),
    RugI = c(min(min_max_scalig$RugI), max(min_max_scalig$RugI)),
    FAS = c(min(min_max_scalig$FAS), max(min_max_scalig$FAS)),
    DPL = c(min(min_max_scalig$DPL), max(min_max_scalig$DPL)),
    DAR = c(min(min_max_scalig$DAR), max(min_max_scalig$DAR)),
    DBR = c(min(min_max_scalig$DBR), max(min_max_scalig$DBR)),
    DMI = c(min(min_max_scalig$DMI), max(min_max_scalig$DMI))
  )
  
  
  
  # Create dataframe for max and min values
  max_min_df <- data.frame(
    Ele = min_max_values$Ele,
    Slp = min_max_values$Slp,
    RugI = min_max_values$RugI,
    FAS = min_max_values$FAS,
    DPL = min_max_values$DPL,
    DAR = min_max_values$DAR,
    DBR = min_max_values$DBR,
    DMI = min_max_values$DMI
  )
  
  rownames(max_min_df) <- c("Max", "Min")
  
  # Bind variable ranges to the data
  df <- rbind(max_min_df, min_max_scalig)
  
  # Plot radar chart
  radarchart(df)
}

# Example usage:
# draw_radar_chart(grid_data)







### Data exploratory analysis ############################################################
print("First few rows of the dataset:")
print(head(data))

# Variable information
print("Variable information:")
print(str(data))


print("Summary statistics:")
print(summary(data))
print("")



################ General summary ########################################################

# Column names
all_column_names <- names(data)

types <- sapply(data, class)

# Count
counts <- sapply(data, length)

# Number of unique values
unique_counts <- sapply(data, function(x) length(unique(x)))

# Missing values
missing_values <- sapply(data, function(x) sum(is.na(x)))

# Create a data frame to combine all information
general_summary_table <- data.frame(
  column=all_column_names,
  Data_Type = types,
  Row_Count = counts,
  Unique_counts = unique_counts,
  Missing_values = missing_values
)

# Print the summary table
print("General Summary Statistics:")
print(general_summary_table)




#################### Numerical features understanding #################### 
numerical_features <- sapply(data, is.numeric)
numerical_data <- data[, numerical_features]
# Column names
column_names <- names(numerical_data)

# Minimum
min_values <- sapply(numerical_data, min, na.rm = TRUE)

# Max
max_values <- sapply(numerical_data, max, na.rm = TRUE)

# Mean
mean_values <- colMeans(numerical_data, na.rm = TRUE)

# STD
std_values <- apply(numerical_data, 2, sd, na.rm = TRUE)

# Coefficient of Variation
cv_values <- apply(numerical_data, 2, function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE))

num_types <- sapply(numerical_data, class)

# Count
num_counts <- sapply(numerical_data, length)

# Number of unique values
num_unique_counts <- sapply(numerical_data, function(x) length(unique(x)))

# Missing values
num_missing_values <- sapply(numerical_data, function(x) sum(is.na(x)))


# Create a data frame to combine all information
summary_table <- data.frame(
  Column=column_names,
  Minimum = sprintf("%.2f", min_values),
  Maximum = sprintf("%.2f", max_values),
  Mean = sprintf("%.2f", mean_values),
  STD = sprintf("%.2f", std_values),
  CV = sprintf("%.2f", cv_values),
  Data_Types=num_types,
  Uniq_Counts=num_unique_counts,
  Missing_Conut=num_missing_values
)

# Print the summary table
print("Summary Statistics:")
print(summary_table)

# Normality and spread
# Distribution (Histogram)
# Example of setting smaller margins
par(mar = c(2, 2, 2, 2))  # Setting smaller margins
par(mfrow = c(3, 3))
for (col in colnames(numerical_data)) {
  hist(numerical_data[[col]], main = paste("Histogram of", col), xlab = col)
}
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # Reset to default margin settings




#################Visit Analysis ############################################
# Create a grid of 20 by 20
grid_data <- expand.grid(
  X = 1:20,
  Y = 1:20
)

View(grid_data)

# Create a new column in grid_data to represent row numbers
grid_data$row_number <- seq_len(nrow(grid_data))

# Merge with the data to indicate presence of pallets
grid_data <- merge(grid_data, data.frame(row_number = seq_len(nrow(data)), data), by = "row_number", all.x = TRUE)
View(grid_data)


# Fill NA values with 0 for pallets not found
grid_data[is.na(grid_data)] <- 0

# Create a new column to represent combination of pallets found in 2009 and 2010
grid_data$Pallet_Combination <- ifelse(grid_data$Pellet_Found_2009 == 1 & grid_data$Pellet_Found_2010 == 1, "Both",
                                       ifelse(grid_data$Pellet_Found_2009 == 1, "2009",
                                              ifelse(grid_data$Pellet_Found_2010 == 1, "2010", "None")))

grid_data_first_ver <-grid_data

# Plot
ggplot(grid_data, aes(x = X, y = Y, fill = Pallet_Combination, label = ID)) +
  geom_tile(color = "black", size = 0.2) +
  geom_text(color = "black", size = 3) +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF", "#DE3163", "white"), 
                    labels = c("2009", "2010", "Both", "None")) +
  labs(title = "Presence of Pallets in 2009 and 2010",
       x = "X",
       y = "Y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())




############### Cluster the plots bases on the reindeer visiting behaviors ###########

data$Visit_Status <- ifelse(data$Pallet_Combination %in% c("2009", "2010", "Both"), 1, 0)

cluster_data <- data
cluster_data <- cluster_data[cluster_data$Visit_Status %in% c(1), ]


k <- 3  # Number of clusters
kmeans_result_power <- kmeans(cluster_data$Dis_Power_Lines, centers = k)
cluster_data$Grid_Distance_Cluster_No <- as.factor(kmeans_result_power$cluster)  # Convert to factor

# Plot the clusters
ggplot(cluster_data, aes(x = seq_along(Dis_Power_Lines), y = Dis_Power_Lines, color = Grid_Distance_Cluster_No)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = rainbow(k)) +
  labs(title = "K-means Clustering on Dis_Power_Lines",
       x = "Index",
       y = "Distance to Power Lines",
       color = "Cluster") +
  theme_minimal()

cluster_data_specific <- cluster_data[cluster_data$Grid_Distance_Cluster_No %in% c(2), ]
View(cluster_data_specific)
draw_radar_chart(cluster_data_specific)


kmeans_result_forest <- kmeans(cluster_data$Forest_Age_Structure, centers = k)
cluster_data$Forest_Cluster_No <-  as.factor(kmeans_result_forest$cluster)

ggplot(cluster_data, aes(x = seq_along(Forest_Age_Structure), y = Forest_Age_Structure, color = Forest_Cluster_No)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = rainbow(k)) +
  labs(title = "K-means Clustering on Forest Age Structure",
       x = "Index",
       y = "Forest Age Structure",
       color = "Cluster") +
  theme_minimal()


cluster_counts <- as.data.frame(table(cluster_data$Forest_Cluster_No))
names(cluster_counts) <- c("Cluster", "Count")


ggplot(cluster_counts, aes(x = Cluster, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Count of Each Forest Cluster",
       x = "Cluster Number",
       y = "Count") +
  theme_minimal()


cluster_freq <- data.frame(table(cluster_data$Forest_Cluster_No))
colnames(cluster_freq) <- c("Cluster", "Frequency")

# Plot the bar chart
ggplot(cluster_freq, aes(x = Cluster, y = Frequency, fill = Cluster)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow(k)) +
  labs(title = "Frequency of Cluster Labels",
       x = "Cluster",
       y = "Frequency",
       fill = "Cluster") +
  theme_minimal()


cluster_data_specific <- cluster_data[cluster_data$Forest_Cluster_No %in% c(3), ]
row_count <- nrow(cluster_data_specific)

View(cluster_data_specific)
draw_radar_chart(cluster_data_specific)



kmeans_result_rods <- kmeans(cluster_data$Dis_All_Roads, centers = k)
cluster_data$Road_Distance_Cluster_No <- kmeans_result_rods$cluster




kmeans_result_slope <- kmeans(cluster_data$Slope, centers = k)
cluster_data$Slope_Cluster_No <- kmeans_result_slope$cluster


kmeans_result_ruggedness <- kmeans(cluster_data$Ruggedness_Index, centers = k)
cluster_data$Ruggedness_Cluster_No <- kmeans_result_ruggedness$cluster

kmeans_result_forest <- kmeans(cluster_data$Forest_Age_Structure, centers = k)
cluster_data$Forest_Cluster_No <- kmeans_result_forest$cluster

cluster_data <- cluster_data %>%
  mutate(Clustering_Score = paste(Grid_Distance_Cluster_No, Road_Distance_Cluster_No, Slope_Cluster_No,Ruggedness_Cluster_No,Forest_Cluster_No, sep = ""))


k <- 3
final_kmeans_result <- kmeans(cluster_data$Clustering_Score, centers = k)
cluster_data$Final_Cluster_No <- final_kmeans_result$cluster
# View the cluster assignments for each data point
print(final_kmeans_result$cluster)

cluster_data$Cluster <- final_kmeans_result$cluster

ggplot(cluster_data, aes(x = seq_along(Clustering_Score), y = Clustering_Score, color = factor(Cluster))) +
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = rainbow(k)) +
  labs(title = "K-means Clustering on Clustering_Score",
       x = "Index",
       y = "Clustering Score",
       color = "Cluster") +
  theme_minimal()


cluster_data_specific <- cluster_data[cluster_data$Final_Cluster_No %in% c(3), ]
View(cluster_data_specific)
draw_radar_chart(cluster_data_specific)





############### Check correlation ##################################################


filtered_grid_data_corr <- subset(grid_data, select = c(Elevation,Slope,Ruggedness_Index ,
                                                        Forest_Age_Structure,Dis_Power_Lines,Dis_All_Roads,Dis_All_Big_Roads,
                                                        Dis_To_Mine ,Dis_Small_Roads    ))

# Compute the correlation matrix
correlation_matrix <- cor(filtered_grid_data_corr, use = "complete.obs")

# Visualize the correlation matrix as a heatmap
corrplot(correlation_matrix, method = "color", type = "upper",
         order = "hclust", addrect = 2)

correlation_matrix





################ Power grid lines avoidance  prediction  #########

library(caret)
library(Metrics)
library(randomForest)
library(pROC)


data$Visit_Status <- ifelse(data$Pallet_Combination %in% c("2009", "2010", "Both"), 1, 0)
data$Visit <- ifelse(data$Pallet_Combination %in% c("2009", "2010", "Both"), "yes", "no")
power_line_avd_prediction <-data


# Create the bar chart
ggplot(power_line_avd_prediction, aes(x = Visit)) +
  geom_bar(aes(fill = Visit)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Count of Yes and No in Visit Column",
       x = "Visit",
       y = "Count") +
  theme_minimal()

# Save the data frame to a CSV file
# write.csv(power_line_avd_prediction, "power_line_avd_prediction.csv", row.names = FALSE)

# Define the training control
train_control <- trainControl(method = "repeatedcv",   # Use k-fold cross-validation
                              number = 10,            # Number of folds
                              repeats = 100,            # Number of repeats
                              search = "random",
                              savePredictions = TRUE,
                              classProbs = TRUE)      # Save probabilities

# Define the grid of hyperparameters to tune
tuneGrid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# Train the random forest model (Model 1: using only Dis_Power_Lines)
rf_model_power_line <- train(Visit ~ Dis_Power_Lines, 
                             data = power_line_avd_prediction, 
                             method = "rf", 
                             tuneGrid = tuneGrid,
                             trControl = train_control, 
                             ntree = 600)

# Print the best tuning parameter
print(rf_model_power_line$bestTune)

# Extract predictions for the best mtry value for Model 1
best_mtry_power_line <- rf_model_power_line$bestTune$mtry
sub_rf_power_line <- subset(rf_model_power_line$pred, mtry == best_mtry_power_line)

# Compute the confusion matrix
conf_matrix <- caret::confusionMatrix(sub_rf_power_line$pred, sub_rf_power_line$obs)

# Print the confusion matrix
print(conf_matrix)


roc_power_line <- roc(sub_rf_power_line$obs, sub_rf_power_line$yes, plot = TRUE, col = "blue", lwd = 2, main = "ROC Curves for Models")


train_control <- trainControl(method = "repeatedcv",   # Use k-fold cross-validation
                              number = 10,            # Number of folds
                              repeats = 100,            # Number of repeats
                              search = "random",
                              savePredictions = TRUE,
                              classProbs = TRUE)      # Save probabilities

# Define the grid of hyperparameters to tune
tuneGrid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# Train the random forest model (Model 2: using all predictors)
rf_model_all <- train(Visit ~ Dis_Power_Lines + Elevation + Slope + Ruggedness_Index + Forest_Age_Structure + Dis_All_Roads + Dis_All_Big_Roads + Dis_To_Mine + Dis_Small_Roads + Broad_Leaved_Forest + Coniferous_Forest + Clear_Cut_Forest + Young_Forest + Mire + Lake, 
                      data = power_line_avd_prediction, 
                      method = "rf", 
                      tuneGrid = tuneGrid,
                      trControl = train_control, 
                      ntree = 600)

# Print the best tuning parameter
print(rf_model_all$bestTune)

# Extract predictions for the best mtry value for Model 2
best_mtry_all <- rf_model_all$bestTune$mtry
sub_rf_all <- subset(rf_model_all$pred, mtry == best_mtry_all)

# Compute the confusion matrix
conf_matrix_all <- caret::confusionMatrix(sub_rf_all$pred, sub_rf_all$obs)

# Print the confusion matrix
print(conf_matrix_all)


roc_power_all <- roc(sub_rf_all$obs, sub_rf_all$yes, plot = TRUE, col = "red", lwd = 2, main = "ROC Curves for Models")

plot(roc_power_line, col = "blue", lwd = 2, main = "ROC Curves for Models")
plot(roc_all, col = "red", lwd = 2, add = TRUE)



################ Next Visit Prediction ##########
library(caret)
library(Metrics)
library(randomForest)
library(pROC)


data$Visit_Status <- ifelse(data$Pallet_Combination %in% c("2009", "2010", "Both"), 1, 0)
data$Visit <- ifelse(data$Pallet_Combination %in% c("2009", "2010", "Both"), "yes", "no")
next_visit_prediction <-data

# Define the control using the RFE function
control <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 10, repeats = 5)

# Define the predictor variables and the response variable
predictors <- next_visit_prediction[, c("Dis_Log_Power_Lines", "Elevation", "Slope", "Dis_Log_All_Roads", 
                                        "Dis_All_Log_Big_Roads", "Dis_To_Log_Mine", "Dis_Log_Small_Roads", 
                                        "Broad_Leaved_Forest", "Coniferous_Forest", "Clear_Cut_Forest", 
                                        "Young_Forest", "Mire", "Lake", "Flat_Areas", "NE_Slop", "SE_Slop", 
                                        "SW_Slop", "NW_Slop", "Forest_Age_Structure")]
response <- as.factor(next_visit_prediction$Visit)

# Check for missing values in predictors
if (any(is.na(predictors))) {
  stop("Predictors contain missing values")
}

# Check for missing values in response
if (any(is.na(response))) {
  stop("Response contains missing values")
}

# Ensure predictors are numeric
predictors <- data.frame(lapply(predictors, as.numeric))

# Ensure response is a factor
response <- as.factor(response)

# Check data consistency: ensure the number of rows in predictors and response are the same
if (nrow(predictors) != length(response)) {
  stop("Predictors and response do not have the same number of rows")
}

# Run the RFE algorithm
results <- rfe(predictors, response, sizes = c(1:15), rfeControl = control)

# Print the results
print(results)

# List the chosen features
selected_features <- predictors(results)
print(selected_features)


train_control <- trainControl(method = "repeatedcv",
                              number = 10,            # Number of folds
                              repeats = 100,          # Number of repeats
                              savePredictions = "final",
                              classProbs = TRUE)      # Save class probabilities

# Train the Logistic Regression model using all predictors
logistic_model <- train(Visit ~ Dis_Log_Power_Lines + Dis_All_Log_Big_Roads + SE_Slop + Elevation + Dis_To_Log_Mine + Young_Forest ,
                        data = next_visit_prediction,
                        method = "glm",
                        family = "binomial",
                        trControl = train_control)

# Print the best tuning parameter
print(logistic_model$bestTune)

# Extract predictions for the logistic regression model
sub_logistic_all <- logistic_model$pred

# Compute the confusion matrix
conf_matrix_all <- caret::confusionMatrix(sub_logistic_all$pred, sub_logistic_all$obs)



# Print the confusion matrix
print(conf_matrix_all)

##Calculate the Confident Intervers


################# Land Plysical Characteristics- Slope analysis a############################################

# Filter grid_data based on Pallet_Combination values
filtered_grid_data <- grid_data_first_ver[grid_data_first_ver$Pallet_Combination %in% c("2009", "2010", "Both"), ]

# Find the minimum value
min_value <- min(filtered_grid_data$Elevation)

# Find the maximum value
max_value <- max(filtered_grid_data$Elevation)

# Print the results
print(paste("Minimum value:", min_value))
print(paste("Maximum value:", max_value))

# Calculate counts for each category and Pallet_Combination
category_counts <- aggregate(. ~ Pallet_Combination, data = filtered_grid_data, FUN = sum)

# Reshape the data to long format for plotting
category_counts_long <- tidyr::pivot_longer(category_counts, cols = c('Flat_Areas', 'NE_Slop', 'SE_Slop', 'SW_Slop', 'NW_Slop'), names_to = "Category", values_to = "Count")

# Plot
ggplot(category_counts_long, aes(x = Category, y = Count, fill = Pallet_Combination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Location Slop characteristics - Pallet Foun '2009', '2010', 'Both'",
       x = "Category",
       y = "Count",
       fill = "Pallet_Combination") +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF", "#DE3163")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






################# Water source   analysis ############################################
filtered_grid_data <- grid_data_first_ver[grid_data_first_ver$Pallet_Combination %in% c("2009", "2010", "Both"), ]

# Calculate counts for each category and Pallet_Combination
category_counts <- aggregate(. ~ Pallet_Combination, data = filtered_grid_data, FUN = sum)

# Reshape the data to long format for plotting
category_counts_long <- tidyr::pivot_longer(category_counts, cols = c('Mire', 'Lake'), names_to = "Category", values_to = "Count")

# Plot
ggplot(category_counts_long, aes(x = Category, y = Count, fill = Pallet_Combination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Water source - Pallet Found '2009', '2010', 'Both'",
       x = "Category",
       y = "Count",
       fill = "Pallet_Combination") +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF", "#DE3163")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






################# Forest type   analysis ############################################
filtered_grid_data <- grid_data_first_ver[grid_data_first_ver$Pallet_Combination %in% c("2009", "2010", "Both"), ]

# Calculate counts for each category and Pallet_Combination
category_counts <- aggregate(. ~ Pallet_Combination, data = filtered_grid_data, FUN = sum)

# Reshape the data to long format for plotting
category_counts_long <- tidyr::pivot_longer(category_counts, cols = c('Broad_Leaved_Forest', 'Coniferous_Forest','Clear_Cut_Forest','Young_Forest'), names_to = "Category", values_to = "Count")

# Plot
ggplot(category_counts_long, aes(x = Category, y = Count, fill = Pallet_Combination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Forest Types - Pallet Found '2009', '2010', 'Both'",
       x = "Category",
       y = "Count",
       fill = "Pallet_Combination") +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF", "#DE3163")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




################# Distance to man mad places analysis ############################################

# Calculate minimum and maximum values
# Define a function to perform min-max scaling and adjust the scale to 1-100
min_max_scaling <- function(x) {
  scaled_value <- (x - min(x)) / (max(x) - min(x))
  scaled_value * 99 + 1  # Adjust scale to range from 1 to 100
}



# Apply min-max scaling to each variable
min_max_scalig <- data.frame(
  Ele = min_max_scaling(grid_data$Elevation),
  Slp = min_max_scaling(grid_data$Slope),
  RugI = min_max_scaling(grid_data$Ruggedness_Index),
  FAS = min_max_scaling(grid_data$Forest_Age_Structure),
  DPL = min_max_scaling(grid_data$Dis_Power_Lines),
  DAR = min_max_scaling(grid_data$Dis_All_Roads),
  DBR = min_max_scaling(grid_data$Dis_All_Big_Roads),
  DMI= min_max_scaling(grid_data$Dis_To_Mine),
  Pallet_Combination =grid_data$Pallet_Combination 
)


min_max_values <- data.frame(
  Ele = c(min(min_max_scalig$Ele), max(min_max_scalig$Ele)),
  Slp = c(min(min_max_scalig$Slp), max(min_max_scalig$Slp)),
  RugI = c(min(min_max_scalig$RugI), max(min_max_scalig$RugI)),
  FAS = c(min(min_max_scalig$FAS), max(min_max_scalig$FAS)),
  DPL = c(min(min_max_scalig$DPL), max(min_max_scalig$DPL)),
  DAR = c(min(min_max_scalig$DAR), max(min_max_scalig$DAR)),
  DBR = c(min(min_max_scalig$DBR), max(min_max_scalig$DBR)),
  DMI = c(min(min_max_scalig$DMI), max(min_max_scalig$DMI))
)


# Print the min_max_values dataframe
max_min_df <- data.frame(
  Ele = min_max_values$Ele,
  Slp = min_max_values$Slp,
  RugI = min_max_values$RugI,  # Use scaled_ruggedness_index directly
  FAS = min_max_values$FAS,
  DPL = min_max_values$DPL,
  DAR = min_max_values$DAR,
  DBR = min_max_values$DBR,
  DMI = min_max_values$DMI
  
)

# Print the max_min dataframe
filtered_grid_data_min_max_scale <- min_max_scalig[min_max_scalig$Pallet_Combination %in% c("Both"), ]

filtered_grid_data_min_max_scale <- subset(filtered_grid_data_min_max_scale, select = -Pallet_Combination)


rownames(max_min_df) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min_df, filtered_grid_data_min_max_scale)

# Plot the radar chart
radarchart(df)


# Create histo plots for each numerical variable
distance_variance_his_df <- subset(min_max_scalig, select = -Pallet_Combination)


par(mar = c(2, 2, 2, 2))  # Setting smaller margins
par(mfrow = c(3, 3))
for (col in colnames(distance_variance_his_df)) {
  hist(distance_variance_his_df[[col]], main = paste("Histogram of", col), xlab = col)
}
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)




###### Grid View ##################################################

min_max_scalig_distance_df <- data.frame(
  ID = grid_data$ID,
  Ele = min_max_scaling(grid_data$Elevation),
  Slp = min_max_scaling(grid_data$Slope),
  RugI = min_max_scaling(grid_data$Ruggedness_Index),
  FAS = min_max_scaling(grid_data$Forest_Age_Structure),
  DPL = min_max_scaling(grid_data$Dis_Power_Lines),
  DAR = min_max_scaling(grid_data$Dis_All_Roads),
  DBR = min_max_scaling(grid_data$Dis_All_Big_Roads),
  DMI= min_max_scaling(grid_data$Dis_To_Mine),
  DSR = min_max_scaling(grid_data$Dis_Small_Roads),
  Pallet_Combination =grid_data$Pallet_Combination 
)

grid_data_distance <- expand.grid(
  X = 1:20,
  Y = 1:20
)

# Create a new column in grid_data to represent row numbers
grid_data_distance$row_number <- seq_len(nrow(grid_data_distance))

# Merge with the data to indicate presence of pallets
grid_data_distance <- merge(grid_data_distance, data.frame(row_number = seq_len(nrow(min_max_scalig_distance_df))
                                                           , min_max_scalig_distance_df), by = "row_number", all.x = TRUE)


sorted_grid_data_distance <- grid_data_distance[order(grid_data_distance$DPL), ]


# Filter out rows where ID equals 0
sorted_grid_data_distance <- sorted_grid_data_distance[sorted_grid_data_distance$ID != 0, ]



# Calculate the number of rows corresponding to 30% of the total rows
rows_30_percent <- round(0.3 * nrow(sorted_grid_data_distance))

# Take the first 30% of the rows
first_30_percent <- sorted_grid_data_distance[1:rows_30_percent, ]

View(first_30_percent)

category_counts_30 <- table(first_30_percent$Pallet_Combination)

# Calculate percentage for each category
category_percentages_30 <- prop.table(category_counts_30) * 100

# Combine counts and percentages into a data frame
visit_summary_stats_30 <- data.frame(Category = names(category_counts_30),
                                  Count = as.numeric(category_counts_30),
                                  Percentage = as.numeric(category_percentages_30))

print(visit_summary_stats_30)

# Plot
ggplot(first_30_percent, aes(x = X, y = Y, fill = Pallet_Combination, label = ID)) +
  geom_tile(color = "black", size = 0.2) +
  geom_text(color = "black", size = 3) +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF","white","#DE3163"), 
                    labels = c("2009", "2010","None", "Both")) +
  labs(title = "Presence of Pallets in 2009 and 2010 - closest 30 % to the Power Lines",
       x = "X",
       y = "Y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

next_30_percent_rows <- sorted_grid_data_distance[(rows_30_percent + 1):(rows_30_percent * 2), ]

# Plot
ggplot(next_30_percent_rows, aes(x = X, y = Y, fill = Pallet_Combination, label = ID)) +
  geom_tile(color = "black", size = 0.2) +
  geom_text(color = "black", size = 3) +
  scale_fill_manual(values = c("#40E0D0", "#CCCCFF","white","#DE3163"), 
                    labels = c("2009", "2010","None", "Both")) +
  labs(title = "Presence of Pallets in 2009 and 2010 - Second closest 30 % to the Power Lines",
       x = "X",
       y = "Y") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

View (next_30_percent_rows)


category_counts_next_30 <- table(next_30_percent_rows$Pallet_Combination)

# Calculate percentage for each category
category_percentages_next_30 <- prop.table(category_counts_next_30) * 100

# Combine counts and percentages into a data frame
visit_summary_stats_next_30 <- data.frame(Category = names(category_counts_next_30),
                                     Count = as.numeric(category_counts_next_30),
                                     Percentage = as.numeric(category_percentages_next_30))

print(visit_summary_stats_next_30)


unique_ids <- unique(sorted_grid_data_distance$ID)

# Filter grid_data for IDs present in sorted_grid_data_distance
filtered_grid_data <- grid_data_first_ver[grid_data_first_ver$ID %in% unique_ids, ]

# View the filtered grid_data
View(filtered_grid_data)

category_counts_30 <- aggregate(. ~ Pallet_Combination, data = filtered_grid_data, FUN = sum)

# Reshape the data to long format for plotting
category_counts_long_30 <- tidyr::pivot_longer(category_counts_30, cols = c('Broad_Leaved_Forest', 'Coniferous_Forest','Clear_Cut_Forest','Young_Forest'), names_to = "Category", values_to = "Count")

# Plot
palette <- c("2009" = "#40E0D0", "2010" = "#CCCCFF", "Both" = "#DE3163" ,"None"="#D4D4D4")


# Plot
ggplot(category_counts_long_30, aes(x = Category, y = Count, fill = Pallet_Combination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Forest Types - Pallet Found '2009', '2010', 'Both'",
       x = "Category",
       y = "Count",
       fill = "Pallet_Combination") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 40))

category_counts <- aggregate(. ~ Pallet_Combination, data = filtered_grid_data, FUN = sum)

# Reshape the data to long format for plotting
category_counts_long <- tidyr::pivot_longer(category_counts, cols = c('Flat_Areas', 'NE_Slop', 'SE_Slop', 'SW_Slop', 'NW_Slop'), names_to = "Category", values_to = "Count")

# Plot
palette <- c("2009" = "#40E0D0", "2010" = "#CCCCFF", "Both" = "#DE3163" ,"None"="#D4D4D4")

ggplot(category_counts_long, aes(x = Category, y = Count, fill = Pallet_Combination)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Location Slop characteristics - Pallet Foun '2009', '2010', 'Both'",
       x = "Category",
       y = "Count",
       fill = "Pallet_Combination") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(ylim = c(0, 40))



