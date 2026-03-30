library(ggplot2)
library(dplyr)

# Load data
results <- read.csv("face_detection_results.csv")
summary_data <- read.csv("face_detection_summary.csv")

# EDA for results.csv
print("Summary of face_detection_results.csv")
print(summary(results))

# Distribution of number of faces
ggplot(results, aes(x = num_faces)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Number of Faces Detected", x = "Number of Faces", y = "Frequency") +
  theme_minimal()
ggsave("num_faces_histogram.png")

# Distribution of average confidence (only for detected faces)
detected_results <- results %>% filter(detected == TRUE)
ggplot(detected_results, aes(x = avg_conf)) +
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Average Confidence Scores", x = "Average Confidence", y = "Frequency") +
  theme_minimal()
ggsave("avg_conf_histogram.png")

# Boxplot of confidence by category
ggplot(detected_results, aes(x = reorder(category, avg_conf, median), y = avg_conf)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  coord_flip() +
  labs(title = "Average Confidence by Category", x = "Category", y = "Average Confidence") +
  theme_minimal()
ggsave("conf_by_category_boxplot.png")

# Boxplot of num_faces by category
ggplot(results, aes(x = reorder(category, num_faces, median), y = num_faces)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  coord_flip() +
  labs(title = "Number of Faces by Category", x = "Category", y = "Number of Faces") +
  theme_minimal()
ggsave("num_faces_by_category_boxplot.png")

# Correlation between num_faces and avg_conf
cor_test <- cor.test(detected_results$num_faces, detected_results$avg_conf)
print(paste("Correlation between num_faces and avg_conf:", cor_test$estimate))
print(cor_test)

ggplot(detected_results, aes(x = num_faces, y = avg_conf)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation: Number of Faces vs Average Confidence", x = "Number of Faces", y = "Average Confidence") +
  theme_minimal()
ggsave("correlation_plot.png")

# EDA for summary_data.csv
print("Summary of face_detection_summary.csv")
print(summary(summary_data))

# Bar plot of detection rates
ggplot(summary_data, aes(x = reorder(category, detection_rate), y = detection_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Detection Rate by Category", x = "Category", y = "Detection Rate") +
  theme_minimal()
ggsave("detection_rate_bar.png")

# Scatter plot of avg_num_faces vs avg_confidence
ggplot(summary_data, aes(x = avg_num_faces, y = avg_confidence, label = category)) +
  geom_point() +
  geom_text(vjust = -1) +
  labs(title = "Average Number of Faces vs Average Confidence by Category", x = "Average Number of Faces", y = "Average Confidence") +
  theme_minimal()
ggsave("summary_scatter.png")

print("EDA complete. Plots saved as PNG files.")