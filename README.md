# WIDER FACE Dataset Analysis: Face Detection Performance Across Settings

## Overview
This project analyzes the performance of a facial recognition algorithm across various real-world settings using the WIDER FACE dataset. The goal is to examine when face detection algorithms are more likely to succeed or fail, addressing concerns about bias in facial recognition technology that could lead to worse decision-making in regulated applications.

## Data Description
The WIDER FACE dataset contains face images in naturalistic contexts, divided into training, validation, and testing sets. Images are organized into 61 categories representing diverse "real-world" scenarios, such as parades, riots, sports events, traffic scenes, family gatherings, and more. Each category folder contains images with varying numbers of faces, poses, occlusions, and environmental conditions.

- **Source**: WIDER FACE dataset (Yang et al., 2016)
- **Structure**:
  - `WIDER_train/images/`: Training images across categories
  - `WIDER_val/images/`: Validation images
  - `WIDER_test/images/`: Testing images
- **Key Features**: Designed for face detection in unconstrained environments, including challenges like blur, occlusion, and varying lighting.

## Methodology/Approach
Face detection was performed using Professor Shiqi Yu's CNN-based model, implemented via the `image.libfacedetection` R package. This model provides confidence scores (0-100) for detected faces.

### Steps:
1. **Setup**: Installed required R packages (`magick`, `image.libfacedetection`).
2. **Sampling**: Selected 18 diverse categories from the test set. For each category, randomly sampled 20 images (or all available if fewer).
3. **Detection**: Applied `image_detect_faces()` to each image, recording:
   - Detection success (presence of faces)
   - Number of faces detected
   - Average confidence score per image
4. **Aggregation**: Computed per-category metrics:
   - Detection rate (proportion of images with detections)
   - Average number of faces
   - Average confidence across detections
5. **Analysis**: Examined variations by setting to identify patterns in performance.

### Tools Used:
- R with `magick` for image handling, `image.libfacedetection` for face detection, and `ggplot2`/`dplyr` for EDA.
- Scripts: `analyze_faces.R` (detection), `eda_faces.R` (exploratory analysis)

## Results
Analysis of 360 images across 18 categories yielded the following key metrics from both detection analysis and exploratory data analysis (EDA):

### Detection Rate
- **Overall (Category-Level)**: 100% across all categories (faces detected in every sampled image at category aggregation).
- **Image-Level**: 93% (335/360 images with detections; 25 images with no faces detected). Non-detections occurred across various categories, indicating potential failures in challenging settings.

### Average Number of Faces Detected
- **Range**: 0 to 46 faces per image (mean: 4.52, median: 3).
- **By Category (Highest)**: Group (13.4), Family_Group (6.2), Sports_Fan (6.6)
- **By Category (Lowest)**: Car_Accident (2.1), Soldier_Firing (2.5), Car_Racing (2.7)
- **Distribution**: Skewed right; most images have 1-10 faces, with outliers in group settings.

### Average Confidence Scores
- **Range**: 25 to 100 (mean: 76.68, median: 80) for detected images; NAs for non-detections.
- **By Category (Highest)**: Family_Group (88.3), Group (84.8), Voter (77.6)
- **By Category (Lowest)**: Car_Accident (60.5), Traffic (71.3), Raid (73.0)
- **Distribution**: Roughly normal, centered around 70-90, with variability by category.

### EDA Insights
- **Correlations**: Weak positive correlation (r = 0.12, p = 0.026) between number of faces and average confidence—more faces don't strongly predict higher confidence.
- **Visualizations** (generated via `eda_faces.R`):
  - Histograms: Distributions of face counts and confidence scores.
  - Boxplots: Variability in confidence and face counts by category (e.g., higher spread in dynamic settings like Car_Accident).
  - Scatter Plots: Relationships between metrics, highlighting clusters (e.g., high-faces/high-confidence categories).
- **Outliers**: Images with 0 faces or extreme confidence scores suggest areas for manual review.

Detailed results are available in:
- `face_detection_results.csv`: Per-image data
- `face_detection_summary.csv`: Category-level summaries
- PNG plots from EDA (e.g., `num_faces_histogram.png`, `conf_by_category_boxplot.png`)

## Analysis
The face detection algorithm demonstrates high robustness, successfully identifying faces in all tested images across diverse settings. However, performance quality varies:

- **Strengths**: Excels in group settings (e.g., family gatherings, sports) with high confidence and multiple face detections, suggesting reliability in social or controlled environments.
- **Weaknesses**: Lower confidence in dynamic, chaotic contexts (e.g., car accidents, traffic, street battles), potentially due to motion blur, occlusion, or small face sizes. This could indicate reduced accuracy in high-stakes scenarios.
- **Bias Implications**: While no complete detection failures were seen, varying confidence levels highlight potential biases. Facial recognition systems might perform inconsistently across contexts, leading to biased outcomes (e.g., missed detections in emergency situations). This aligns with regulatory concerns about algorithmic fairness.

The dataset's curation for face detection may contribute to the 100% success rate; real-world applications might see more failures.

## Next Steps
1. **Expand Sampling**: Test all 61 categories and larger samples to capture more variation.
2. **Threshold Analysis**: Apply confidence thresholds (e.g., >70) to simulate "failures" and assess accuracy.
3. **Error Analysis**: Manually review low-confidence detections for false positives/negatives.
4. **Demographic Bias**: If annotations are available, analyze performance by demographics (age, gender, ethnicity).
5. **Integration**: Combine with downstream facial recognition tasks (e.g., identification) to study end-to-end bias.
6. **Alternative Models**: Compare with other detectors (e.g., MTCNN, Dlib) for robustness.
7. **Visualization**: Add plots of confidence distributions by category.

For questions or contributions, refer to the analysis script and data files.