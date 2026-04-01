library(dplyr)
library(ggplot2)

# ── 1. PARSE THE WIDER FACE ANNOTATION TXT FILE ──────────────────────────────
# The format is:
#   filename
#   number_of_faces
#   x1 y1 w h blur expression illumination invalid occlusion pose (per face)

parse_wider_annotations <- function(txt_path) {
  lines <- readLines(txt_path)
  records <- list()
  i <- 1
  while (i <= length(lines)) {
    img_path <- trimws(lines[i]); i <- i + 1
    if (i > length(lines)) break
    n_faces <- as.integer(trimws(lines[i])); i <- i + 1
    
    if (n_faces == 0) {
      # One dummy line even when 0 faces
      i <- i + 1
      records[[length(records) + 1]] <- data.frame(
        image_path = img_path, n_gt_faces = 0,
        avg_occlusion = NA, pct_heavy_occlusion = NA,
        avg_pose = NA, pct_atypical_pose = NA,
        avg_face_height = NA, scale_category = NA,
        stringsAsFactors = FALSE)
      next
    }
    
    face_data <- matrix(NA, nrow = n_faces, ncol = 10)
    for (f in 1:n_faces) {
      vals <- as.numeric(strsplit(trimws(lines[i]), "\\s+")[[1]]); i <- i + 1
      face_data[f, ] <- vals
    }
    # Columns: x y w h blur expression illumination invalid occlusion pose
    # occlusion: 0=none, 1=partial, 2=heavy
    # pose:      0=typical, 1=atypical
    
    heights      <- face_data[, 4]  # bounding box height
    occlusions   <- face_data[, 9]
    poses        <- face_data[, 10]
    
    avg_h <- mean(heights, na.rm = TRUE)
    scale_cat <- ifelse(avg_h < 30, "Small", ifelse(avg_h < 300, "Medium", "Large"))
    
    records[[length(records) + 1]] <- data.frame(
      image_path           = img_path,
      n_gt_faces           = n_faces,
      avg_occlusion        = mean(occlusions, na.rm = TRUE),
      pct_heavy_occlusion  = mean(occlusions == 2, na.rm = TRUE),
      avg_pose             = mean(poses, na.rm = TRUE),
      pct_atypical_pose    = mean(poses == 1, na.rm = TRUE),
      avg_face_height      = avg_h,
      scale_category       = scale_cat,
      stringsAsFactors     = FALSE)
  }
  bind_rows(records)
}

annotations <- parse_wider_annotations("C:/Radhya/Masters Application/Columbia University/Spring 2026/Data Science and Public Policy/Data Assignment 2/wider_face_annotations/wider_face_split/wider_face_val_bbx_gt.txt")
# NOTE: use wider_face_train_bbx_gt.txt or wider_face_vaal_bbx_gt.txt depending
# on which split your results CSV was built from. analyze_faces.R used WIDER_test,
# but the test .txt may not have bbox labels — if so, use the val split instead.

# ── 2. EXTRACT JUST THE FILENAME AND CATEGORY TO MATCH WITH RESULTS CSV ──────
annotations <- annotations %>%
  mutate(
    image    = basename(image_path),
    category = sub("/.*", "", image_path)   # e.g. "0--Parade"
  )

# ── 3. LOAD EXISTING DETECTION RESULTS AND JOIN ───────────────────────────────
results <- read.csv("face_detection_results.csv", stringsAsFactors = FALSE)

joined <- results %>%
  left_join(annotations, by = c("category", "image"))

# ── 4. CREATE BINNED ATTRIBUTE VARIABLES FOR ANALYSIS ────────────────────────
joined <- joined %>%
  mutate(
    occlusion_level = case_when(
      avg_occlusion < 0.5  ~ "Low",
      avg_occlusion < 1.5  ~ "Partial",
      TRUE                 ~ "Heavy"
    ),
    pose_level = case_when(
      pct_atypical_pose < 0.25 ~ "Mostly Typical",
      pct_atypical_pose < 0.75 ~ "Mixed",
      TRUE                     ~ "Mostly Atypical"
    )
  )

write.csv(joined, "face_detection_annotated.csv", row.names = FALSE)

# ── FACTOR LEVELS ──────────────────────────────────────────────────────────────
# Set these once at the top and all plots will use them

joined$occlusion_level <- factor(joined$occlusion_level, 
                                 levels = c("Low", "Partial", "Heavy"))

joined$scale_category <- factor(joined$scale_category, 
                                levels = c("Small", "Medium", "Large"))

joined$pose_level <- factor(joined$pose_level, 
                            levels = c("Mostly Typical", "Mixed", "Mostly Atypical"))

# ── 5. ANALYSIS: DETECTION RATE BY TRUE ATTRIBUTE ────────────────────────────

## 5a. Occlusion
occ_summary <- joined %>%
  filter(!is.na(occlusion_level)) %>%
  group_by(occlusion_level) %>%
  summarise(
    detection_rate = mean(detected, na.rm = TRUE),
    avg_confidence = mean(avg_conf, na.rm = TRUE),
    n = n()
  )

ggplot(occ_summary, aes(x = occlusion_level, y = detection_rate, fill = occlusion_level)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "Partial" = "#f39c12", "Heavy" = "#e74c3c")) +
  labs(title = "Detection Rate by Occlusion Level (Ground Truth)",
       x = "Occlusion Level", y = "Detection Rate") +
  theme_minimal() + theme(legend.position = "none")
ggsave("detection_by_occlusion.png")

## 5b. Scale
scale_summary <- joined %>%
  filter(!is.na(scale_category)) %>%
  group_by(scale_category) %>%
  summarise(
    detection_rate = mean(detected, na.rm = TRUE),
    avg_confidence = mean(avg_conf, na.rm = TRUE),
    n = n()
  )

ggplot(scale_summary, aes(x = scale_category, y = detection_rate, fill = scale_category)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Small" = "#e74c3c", "Medium" = "#f39c12", "Large" = "#2ecc71")) +
  labs(title = "Detection Rate by Face Scale (Ground Truth)",
       x = "Face Scale", y = "Detection Rate") +
  theme_minimal() + theme(legend.position = "none")
ggsave("detection_by_scale.png")

## 5c. Pose
pose_summary <- joined %>%
  filter(!is.na(pose_level)) %>%
  group_by(pose_level) %>%
  summarise(
    detection_rate = mean(detected, na.rm = TRUE),
    avg_confidence = mean(avg_conf, na.rm = TRUE),
    n = n()
  )

ggplot(pose_summary, aes(x = pose_level, y = detection_rate, fill = pose_level)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3.5) +
  labs(title = "Detection Rate by Pose (Ground Truth)",
       x = "Pose Level", y = "Detection Rate") +
  theme_minimal() + theme(legend.position = "none")
ggsave("detection_by_pose.png")

# ── 6. COMPARE HARDCODED vs GROUND TRUTH LABELS ──────────────────────────────
summary_data <- read.csv("face_detection_summary.csv", stringsAsFactors = FALSE)

gt_by_category <- joined %>%
  group_by(category) %>%
  summarise(
    gt_occlusion_level = case_when(
      mean(avg_occlusion, na.rm = TRUE) < 0.5  ~ "Easy",
      mean(avg_occlusion, na.rm = TRUE) < 1.5  ~ "Medium",
      TRUE                                      ~ "Hard"
    ),
    gt_scale = case_when(
      mean(avg_face_height, na.rm = TRUE) < 30  ~ "Hard",
      mean(avg_face_height, na.rm = TRUE) < 300 ~ "Medium",
      TRUE                                      ~ "Easy"
    )
  )


# -----------------------------------------------------------------
# Confidence by occlusion
occ_conf <- joined %>%
  filter(!is.na(occlusion_level), detected == TRUE) %>%
  group_by(occlusion_level) %>%
  summarise(avg_confidence = mean(avg_conf, na.rm = TRUE), n = n())

ggplot(occ_conf, aes(x = occlusion_level, y = avg_confidence, fill = occlusion_level)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Low" = "#2ecc71", "Partial" = "#f39c12", "Heavy" = "#e74c3c")) +
  labs(title = "Average Confidence Score by Occlusion Level (Detected Images Only)",
       x = "Occlusion Level", y = "Average Confidence Score") +
  theme_minimal() + theme(legend.position = "none")
ggsave("confidence_by_occlusion.png")

# Confidence by scale
scale_conf <- joined %>%
  filter(!is.na(scale_category), detected == TRUE) %>%
  group_by(scale_category) %>%
  summarise(avg_confidence = mean(avg_conf, na.rm = TRUE), n = n())

ggplot(scale_conf, aes(x = scale_category, y = avg_confidence, fill = scale_category)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Small" = "#e74c3c", "Medium" = "#f39c12", "Large" = "#2ecc71")) +
  labs(title = "Average Confidence Score by Face Scale (Detected Images Only)",
       x = "Face Scale", y = "Average Confidence Score") +
  theme_minimal() + theme(legend.position = "none")
ggsave("confidence_by_scale.png")

# -----------------------------------------------------------------
heatmap_data <- joined %>%
  filter(!is.na(scale_category), !is.na(occlusion_level)) %>%
  group_by(scale_category, occlusion_level) %>%
  summarise(detection_rate = mean(detected, na.rm = TRUE), n = n(), .groups = "drop")

ggplot(heatmap_data, aes(x = occlusion_level, y = scale_category, fill = detection_rate)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = paste0(round(detection_rate * 100), "%\nn=", n)), size = 3.5) +
  scale_fill_gradient(low = "#e74c3c", high = "#2ecc71", name = "Detection Rate") +
  labs(title = "Detection Rate by Scale × Occlusion (Ground Truth)",
       x = "Occlusion Level", y = "Face Scale") +
  theme_minimal()
ggsave("heatmap_scale_occlusion.png")

# -----------------------------------------------------------------
summary_data <- read.csv("face_detection_summary.csv", stringsAsFactors = FALSE)

gt_by_category <- joined %>%
  group_by(category) %>%
  summarise(
    gt_occlusion = case_when(
      mean(avg_occlusion, na.rm = TRUE) < 0.5 ~ "Easy",
      mean(avg_occlusion, na.rm = TRUE) < 1.5 ~ "Medium",
      TRUE ~ "Hard"),
    gt_scale = case_when(
      mean(avg_face_height, na.rm = TRUE) < 30  ~ "Hard",
      mean(avg_face_height, na.rm = TRUE) < 300 ~ "Medium",
      TRUE ~ "Easy")
  )

comparison <- summary_data %>%
  left_join(gt_by_category, by = "category") %>%
  mutate(
    occlusion_match = occlusion_difficulty == gt_occlusion,
    scale_match = scale_difficulty == gt_scale
  ) %>%
  select(category, occlusion_difficulty, gt_occlusion, occlusion_match,
         scale_difficulty, gt_scale, scale_match)

print(comparison)
write.csv(comparison, "label_comparison.csv", row.names = FALSE)
# ----------------------------------------------------------------
cat_occ <- joined %>%
  filter(!is.na(occlusion_level)) %>%
  group_by(category, occlusion_level) %>%
  summarise(detection_rate = mean(detected, na.rm = TRUE), 
            n = n(), .groups = "drop") %>%
  mutate(category = gsub("^[0-9]+--", "", category))  # clean category names

ggplot(cat_occ, aes(x = occlusion_level, y = reorder(category, detection_rate), 
                    fill = detection_rate)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(detection_rate * 100), "%")), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#e74c3c", high = "#2ecc71", 
                      name = "Detection\nRate") +
  labs(title = "Detection Rate by Event Category and Occlusion Level",
       x = "Occlusion Level", y = "Event Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
ggsave("heatmap_category_occlusion.png", width = 8, height = 10)
# ----------------------------------------------------------------
cat_scale <- joined %>%
  filter(!is.na(scale_category)) %>%
  group_by(category, scale_category) %>%
  summarise(detection_rate = mean(detected, na.rm = TRUE), 
            n = n(), .groups = "drop") %>%
  mutate(category = gsub("^[0-9]+--", "", category))

ggplot(cat_scale, aes(x = scale_category, y = reorder(category, detection_rate), 
                      fill = detection_rate)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(detection_rate * 100), "%")), 
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#e74c3c", high = "#2ecc71", 
                      name = "Detection\nRate") +
  labs(title = "Detection Rate by Event Category and Face Scale",
       x = "Face Scale", y = "Event Category") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
ggsave("heatmap_category_scale.png", width = 8, height = 10)
# ----------------------------------------------------------------
category_summary <- joined %>%
  group_by(category) %>%
  summarise(
    detection_rate   = mean(detected, na.rm = TRUE),
    avg_occlusion    = mean(avg_occlusion, na.rm = TRUE),
    avg_faces        = mean(n_gt_faces, na.rm = TRUE),
    avg_face_height  = mean(avg_face_height, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    category_clean = gsub("^[0-9]+--", "", category),
    scale_group = case_when(
      avg_face_height < 30  ~ "Small",
      avg_face_height < 300 ~ "Medium",
      TRUE                  ~ "Large"
    )
  )

ggplot(category_summary, aes(x = avg_occlusion, y = detection_rate, 
                             size = avg_faces, color = scale_group,
                             label = category_clean)) +
  geom_point(alpha = 0.7) +
  geom_text(vjust = -1, size = 2.8, check_overlap = TRUE) +
  scale_size_continuous(name = "Avg Faces\nper Image", range = c(3, 12)) +
  scale_color_manual(values = c("Small" = "#e74c3c", 
                                "Medium" = "#f39c12", 
                                "Large" = "#2ecc71"),
                     name = "Face Scale") +
  labs(title = "Detection Rate by Category: Occlusion, Scale, and Crowd Size",
       x = "Average Occlusion Level (0=None, 2=Heavy)", 
       y = "Detection Rate") +
  theme_minimal()
ggsave("bubble_category_summary.png", width = 12, height = 8)
# ----------------------------------------------------------------
worst_categories <- category_summary %>%
  arrange(detection_rate) %>%
  head(8) %>%
  mutate(category_clean = reorder(category_clean, detection_rate))

ggplot(worst_categories, aes(x = category_clean, y = detection_rate, 
                             fill = scale_group)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  geom_text(aes(label = paste0(round(detection_rate * 100), "%")), 
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("Small" = "#e74c3c", 
                               "Medium" = "#f39c12", 
                               "Large" = "#2ecc71"),
                    name = "Dominant\nFace Scale") +
  scale_y_continuous(limits = c(0, 1.1)) +
  labs(title = "Lowest Detection Rate Categories",
       x = "Event Category", y = "Detection Rate") +
  theme_minimal()
ggsave("worst_categories.png", width = 9, height = 6)