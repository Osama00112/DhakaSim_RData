# Load necessary libraries
library(ggplot2)
library(extrafont)

# Define the file paths
high_density_without_objects <- "high_without.csv"
with_objects_moderate_density <- "moderate_with.csv"
with_objects_high_density <- "high_with.csv"
moderate_density_without_objects <- "moderate_without.csv"

# Read data
data_high_density_without_objects <- read.csv(high_density_without_objects)
data_with_objects_moderate_density <- read.csv(with_objects_moderate_density)
data_with_objects_high_density <- read.csv(with_objects_high_density)
data_moderate_density_without_objects <- read.csv(moderate_density_without_objects)

# Convert time from seconds to minutes
convert_to_minutes <- function(data) {
  data / 60
}

data_high_density_without_objects$sp_bike <- convert_to_minutes(data_high_density_without_objects$sp_bike)
data_with_objects_moderate_density$sp_bike <- convert_to_minutes(data_with_objects_moderate_density$sp_bike)
data_with_objects_high_density$sp_bike <- convert_to_minutes(data_with_objects_high_density$sp_bike)
data_moderate_density_without_objects$sp_bike <- convert_to_minutes(data_moderate_density_without_objects$sp_bike)

# Extract `sp_bike` data
sp_bike_high_density_without_objects <- data_high_density_without_objects$sp_bike
sp_bike_with_objects_moderate_density <- data_with_objects_moderate_density$sp_bike
sp_bike_with_objects_high_density <- data_with_objects_high_density$sp_bike
sp_bike_moderate_density_without_objects <- data_moderate_density_without_objects$sp_bike

# Manually input real-time data (converted to minutes)
real_time_avg_moderate <- 21.5  # Replace with actual value
real_time_var_moderate <- (100 / 60)^2  # Replace with actual value
real_time_avg_high <- 27       # Replace with actual value
real_time_var_high <- (200 / 60)^2    # Replace with actual value

# Calculate means and standard deviations
calculate_stats <- function(data) {
  avg <- mean(data)
  sd <- sd(data)
  return(c(avg, sd))
}

stats_moderate_without_objects <- calculate_stats(sp_bike_moderate_density_without_objects)
stats_with_objects_moderate <- calculate_stats(sp_bike_with_objects_moderate_density)
stats_high_without_objects <- calculate_stats(sp_bike_high_density_without_objects)
stats_with_objects_high <- calculate_stats(sp_bike_with_objects_high_density)

# Prepare data for plotting
plot_data <- data.frame(
  Density = rep(c("Moderate", "High"), each = 3),
  Type = rep(c("Without Objects", "With Objects", "Real Time"), 2),
  Avg = c(
    
    stats_high_without_objects[1],
    stats_with_objects_high[1],
    real_time_avg_high,
    stats_moderate_without_objects[1],
    stats_with_objects_moderate[1],
    real_time_avg_moderate
  ),
  SD = c(
    
    stats_high_without_objects[2],
    stats_with_objects_high[2],
    sqrt(real_time_var_high),
    stats_moderate_without_objects[2],
    stats_with_objects_moderate[2],
    sqrt(real_time_var_moderate)
  )
)

# Update factor levels for order in plot
plot_data$Type <- factor(plot_data$Type, levels = c("Without Objects", "With Objects", "Real Time"))

# Plot
# Plot with adjusted text position
ggplot(plot_data, aes(x = Density, y = Avg, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = Avg - SD, ymax = Avg + SD), 
    position = position_dodge(width = 0.8), 
    width = 0.2
  ) +
  geom_text(
    aes(label = round(Avg, 1), y = Avg / 2), # Adjust text position to the middle of the bar
    position = position_dodge(width = 0.8), 
    size = 4, 
    family = "Times New Roman"
  ) +
  labs(
    title = "",
    x = "Vehicular density",
    y = "Travel time (min)",
    fill = ""
  ) +
  scale_x_discrete(labels = c("Moderate", "High")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, family = "Times New Roman"),
    axis.text.y = element_text(size = 14, family = "Times New Roman"),
    axis.title.x = element_text(size = 16, family = "Times New Roman", margin = margin(t = 15)),
    axis.title.y = element_text(size = 16, family = "Times New Roman", margin = margin(r = 15)),
    plot.title = element_text(size = 18, family = "Times New Roman", face = "bold"),
    legend.position = c(0.4, 1.07),
    legend.direction = "horizontal",
    legend.justification = c(0.5, 1),
    legend.text = element_text(size = 12, family = "Times New Roman"),
    legend.title = element_text(size = 14, family = "Times New Roman"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_fill_brewer(palette = "Set1")
