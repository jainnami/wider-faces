## Data Science and Public Policy
## Facial recognition in various settings
##--##--##--##--##--##--##--##--##--##--##--##

## The packages below provide tools for analyzing image data in R
## libfacedetection was developed by Professor Shiqi Yu (https://github.com/ShiqiYu)
## It's a CNN that was trained to detect faces in images.
## For more information, see: https://github.com/ShiqiYu/libfacedetection

## This code provides an example for how to use his packages to detect images
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##


library(magick)
library(image.libfacedetection)

setwd("<path to your working directory here>")

# Example: accessing images in training set under the "parade" category:
train_images_parade = list.files("WIDER_train/images/0--Parade/")

# The function image_read() reads the image into memory (you can run a loop to load all images):
example_image = image_read(paste0("WIDER_train/images/0--Parade/",train_images_parade[sample(1:length(train_images_parade),1)]))

# The function image_detect_faces() uses Yu's model to detect faces in an image
# Note that if the model returns no data, it means that it did not detect a face:
example_image_faces = image_detect_faces(example_image)

# The "confidence" variable is a confidence score for the detection (ranges from 0-100)
# You can use it as your main measure for how well the model detected a face
example_image_faces$detections$confidence

# You can also visualize with this code:
plot(example_image_faces, example_image, border = "green", lwd = 1, col = "white")

