# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the data
data <- read.csv("data/20240517_OSDS_Parcels_Attributes(Sheet1).csv")

# Filter data
data <- data %>% filter(AOI == "Anahola")

# Define the categorical columns with more readable names
categorical_columns <- c("OSDS.Present", "Newly.Developed..2005...2021.", "X500.ft.Sewer.Buffer", 
                         "X500.ft.Coastline.Buffer", "WWTP.1.Mi.Buffer", "CEJST.LIDAC")
readable_names <- c("OSDS Present", "Newly Developed (2005 - 2021)", "500-ft Sewer Buffer", 
                    "500-ft Coastline Buffer", "WWTP 1-Mi Buffer", "CEJST LIDAC")

# Function to calculate counts and percentages
calculate_summary <- function(df, column) {
  total_count <- nrow(df)
  yes_count <- sum(df[[column]] == "Yes", na.rm = TRUE)
  no_count <- sum(df[[column]] == "No", na.rm = TRUE)
  yes_percentage <- (yes_count / total_count) * 100
  no_percentage <- (no_count / total_count) * 100
  
  data.frame(
    Column = column,
    Yes_Count = yes_count,
    No_Count = no_count,
    Yes_Percentage = yes_percentage,
    No_Percentage = no_percentage
  )
}

# Apply the function to each categorical column and bind the results
summary_stats <- do.call(rbind, lapply(categorical_columns, function(col) calculate_summary(data, col)))

# Print the summary statistics
print(summary_stats)

# Function to plot categorical summary with actual values on the plot
plot_categorical_summary <- function(df, column, readable_name) {
  summary_stats <- df %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / sum(Count)) * 100)
  
  ggplot(summary_stats, aes(x = !!sym(column), y = Count, fill = !!sym(column))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = paste("Distribution of", readable_name), x = readable_name, y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Plot each categorical column with readable names
for (i in seq_along(categorical_columns)) {
  col <- categorical_columns[i]
  readable_name <- readable_names[i]
  print(plot_categorical_summary(data, col, readable_name))
}


# Function to calculate counts and percentages for a column with multiple options
calculate_multiclass_summary <- function(df, column) {
  total_count <- nrow(df)
  value_counts <- df %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / total_count) * 100)
  
  return(value_counts)
}

# Apply the function to the 'Sea Grant Priority' column
sea_grant_summary <- calculate_multiclass_summary(data, "Sea.Grant.Priority")

# Print the summary statistics
print(sea_grant_summary)

# Function to calculate counts and percentages for a column with multiple options
calculate_multiclass_summary <- function(df, column) {
  total_count <- nrow(df)
  value_counts <- df %>%
    group_by(!!sym(column)) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = (Count / total_count) * 100)
  
  return(value_counts)
}

# Apply the function to the 'Island' and 'AOI' columns
island_summary <- calculate_multiclass_summary(data, "Island")
aoi_summary <- calculate_multiclass_summary(data, "AOI")

# Print the summary statistics
print("Island Summary:")
print(island_summary)
print("AOI Summary:")
print(aoi_summary)

# Function to plot multiclass summary with actual values on the plot
plot_multiclass_summary <- function(summary_df, column_name, readable_name) {
  ggplot(summary_df, aes(x = reorder(!!sym(column_name), -Count), y = Count, fill = !!sym(column_name))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Count), vjust = -0.5) +
    labs(title = paste("Distribution of", readable_name), x = readable_name, y = "Count") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(ylim = c(0, max(summary_df$Count) * 1.1)) # Adjust y-axis limits to prevent cutting off tall columns
}

# Plot the 'Island' column
print(plot_multiclass_summary(island_summary, "Island", "Island"))

# Plot the 'AOI' column
print(plot_multiclass_summary(aoi_summary, "AOI", "Area of Interest (AOI)"))

# Function to calculate summary statistics for numerical columns
calculate_numerical_summary <- function(df, column) {
  summary_stats <- df %>%
    summarise(
      Mean = mean(!!sym(column), na.rm = TRUE),
      Median = median(!!sym(column), na.rm = TRUE),
      SD = sd(!!sym(column), na.rm = TRUE),
      Min = min(!!sym(column), na.rm = TRUE),
      Max = max(!!sym(column), na.rm = TRUE),
      Q1 = quantile(!!sym(column), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(column), 0.75, na.rm = TRUE)
    )
  
  summary_stats <- data.frame(Column = column, summary_stats)
  return(summary_stats)
}

# Apply the function to the 'Shape_Length' and 'Shape_Area' columns
shape_length_summary <- calculate_numerical_summary(data, "Shape_Length")
shape_area_summary <- calculate_numerical_summary(data, "Shape_Area")

print(summary_stats)

# Install and load the clipr package if you haven't already
install.packages("clipr")
library(clipr)

# Write summary_stats to a temporary file in tab-separated values (TSV) format
write.table(summary_stats, file = "summary_stats.tsv", sep = "\t", row.names = FALSE)

# Copy the contents of the temporary file to the clipboard
write_clip(summary_stats)

