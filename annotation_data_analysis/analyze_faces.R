library(magick)
library(image.libfacedetection)

setwd("C:/Radhya/Masters Application/Columbia University/Spring 2026/Data Science and Public Policy/Data Assignment 2")

# Selected categories
#categories <- c("0--Parade", "3--Riot", "4--Dancing", "5--Car_Accident", "12--Group", "14--Traffic", "20--Family_Group", "24--Soldier_Firing", "28--Sports_Fan", "34--Baseball", "42--Car_Racing", "47--Matador_Bullfighter", "50--Celebration_Or_Party", "53--Raid", "54--Rescue", "56--Voter", "58--Hockey", "61--Street_Battle")
categories <- c(
  # Original 18
  "0--Parade", "3--Riot", "4--Dancing", "5--Car_Accident",
  "12--Group", "14--Traffic", "20--Family_Group", "24--Soldier_Firing",
  "28--Sports_Fan", "34--Baseball", "42--Car_Racing",
  "47--Matador_Bullfighter", "50--Celebration_Or_Party",
  "53--Raid", "54--Rescue", "56--Voter", "58--Hockey", "61--Street_Battle",
  
  # Add these for MORE HEAVY OCCLUSION:
  "2--Demonstration",        # protest crowds, people blocking each other
  "9--Press_Conference",     # microphones/hands in front of faces
  "22--Picnic",              # informal, people overlapping
  "39--Ice_Fishing",         # hats, scarves, gear covering faces
  
  # Add these for MORE LARGE / CLOSE-UP FACES:
  "1--Handshaking",          # close-up, clear frontal faces
  "10--People_Marching",     # mix of distances
  "16--Award_Ceremony",      # podium shots, close faces
  "21--Festival",            # varied but includes close-ups
  
  # Add these for MORE ATYPICAL POSE:
  "30--Gymnastics",          # extreme body/head orientations
  "38--Tennis",              # serving, jumping poses
  "40--Yoga"                 # inverted and angled poses
)

# Data frame to store results
results <- data.frame(category = character(), image = character(), detected = logical(), num_faces = integer(), avg_conf = numeric(), stringsAsFactors = FALSE)

# Function to process an image
process_image <- function(cat, img_file) {
  img_path <- paste0("WIDER_val/WIDER_val/images/", cat, "/", img_file)
  img <- image_read(img_path)
  faces <- image_detect_faces(img)
  if (nrow(faces$detections) > 0) {
    detected <- TRUE
    num_faces <- nrow(faces$detections)
    avg_conf <- mean(faces$detections$confidence)
  } else {
    detected <- FALSE
    num_faces <- 0
    avg_conf <- NA
  }
  return(list(detected = detected, num_faces = num_faces, avg_conf = avg_conf))
}

# Setting the seed
set.seed(44)

# Loop over categories
for (cat in categories) {
  img_files <- list.files(paste0("WIDER_val/WIDER_val/images/", cat))
  # Sample 10 images, or all if less
  n_sample <- min(40, length(img_files))
  sampled <- sample(img_files, n_sample)
  for (img in sampled) {
    res <- process_image(cat, img)
    results <- rbind(results, data.frame(category = cat, image = img, detected = res$detected, num_faces = res$num_faces, avg_conf = res$avg_conf))
  }
}

# Save results
write.csv(results, "face_detection_results.csv", row.names = FALSE)

# Summary by category
summary_stats <- aggregate(cbind(detected, num_faces, avg_conf) ~ category, data = results, FUN = function(x) {
  if (is.numeric(x)) {
    mean(x, na.rm = TRUE)
  } else if (is.logical(x)) {
    mean(x)  # proportion detected
  } else {
    NA
  }
})

# Rename columns
names(summary_stats) <- c("category", "detection_rate", "avg_num_faces", "avg_confidence")

# Define difficulty levels based on paper's occlusion, scale, pose (inferred from category)
summary_stats$occlusion_difficulty <- ifelse(summary_stats$category %in% c("3--Riot", "61--Street_Battle", "53--Raid", "14--Traffic"), "Hard",
                                             ifelse(summary_stats$category %in% c("0--Parade", "28--Sports_Fan", "50--Celebration_Or_Party", "4--Dancing", "34--Baseball", "58--Hockey"), "Medium", "Easy"))

summary_stats$scale_difficulty <- ifelse(summary_stats$category %in% c("0--Parade", "14--Traffic", "42--Car_Racing", "61--Street_Battle"), "Hard",
                                         ifelse(summary_stats$category %in% c("28--Sports_Fan", "34--Baseball", "58--Hockey", "3--Riot", "53--Raid"), "Medium", "Easy"))

summary_stats$pose_difficulty <- ifelse(summary_stats$category %in% c("4--Dancing", "34--Baseball", "58--Hockey", "28--Sports_Fan", "47--Matador_Bullfighter"), "Hard",
                                        ifelse(summary_stats$category %in% c("3--Riot", "61--Street_Battle", "42--Car_Racing", "0--Parade"), "Medium", "Easy"))

write.csv(summary_stats, "face_detection_summary.csv", row.names = FALSE)

print(summary_stats)