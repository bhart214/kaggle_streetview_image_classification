# Load Data

library(readr)
library(bmp)
library(pixmap)
library(dplyr)


# location of train and test images
train_files = list.files(path="C:/Users/bhart/Documents/R_Projects/streetview_data/trainResized/.", pattern=".Bmp",all.files=T, full.names=T, no.. = T) 
test_files = list.files(path="C:/Users/bhart/Documents/R_Projects/streetview_data/testResized/.", pattern=".Bmp",all.files=T, full.names=T, no.. = T)

# we will create a matrix with a row for each image.
# since images are 20 by 20 pixels, our new matrix will have 400 columns of pixel data.
train_images = list(rep(0,length(train_files)))
train_mat = matrix(ncol = 401, nrow = length(train_files))
for(i in 1:nrow(train_mat)) {
  ID = unlist(strsplit(train_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  train_images[[i]] = read.bmp(train_files[i])
  img = train_images[[i]]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = (r + g + b)/3
    temp = as.vector(temp)
    temp = c(ID2, temp)
    train_mat[i,] = temp
  }
  else {
    temp = as.vector(img[ , ])
    temp = c(ID2, temp)
    train_mat[i,] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(train_mat)
sum(is.na(train_mat))


# and for the test images also
test_images = list(rep(0,length(test_files)))
test_mat = matrix(ncol = 401, nrow = length(test_files))
for(i in 1:nrow(test_mat)) {
  ID = unlist(strsplit(test_files[i], split = ".", fixed=T))[2]
  ID2 = as.numeric(unlist(strsplit(ID, split = "/"))[2])
  test_images[[i]] = read.bmp(test_files[i])
  img = test_images[[i]]
  if (length(dim(img)) == 3) {
    r = img[ , , 1]
    g = img[ , , 2]
    b = img[ , , 3]
    temp = (r + g + b)/3
    temp = as.vector(temp)
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
  else {
    temp = as.vector(img[ , ])
    temp = c(ID2, temp)
    test_mat[i,] = temp
  }
}
# check dimensions of matrix and to make sure everything loaded properly (no NAs)
dim(test_mat)
sum(is.na(test_mat))

# convert matrices do data frames and delete other matrices, vectors, and lists from memory
trainDF = as.data.frame(train_mat)
trainDF = trainDF %>% dplyr::rename(ID = V1)
testDF = as.data.frame(test_mat)
testDF = testDF %>% dplyr::rename(ID = V1)
rm(r, g, b, temp, i, img, ID, ID2, train_mat, test_mat, train_images, test_images)

# Read in the training labels and merge with the trainDF
trainLabels = read_csv("C:/Users/bhart/Documents/R_Projects/streetview_image_classification_kaggle/trainLabels.csv", col_types = cols(ID = col_character()))
trainLabels$ID = as.numeric(trainLabels$ID)
trainDF = merge(x = trainDF, y = trainLabels, by = "ID") # merge by common ID columns
trainDF = trainDF[, c(1,402,2:401)] # rearrange columns in dataframe so that ID is first, Class is second, ...
trainDF$Class = as.factor(trainDF$Class)

# Duplicates - change indices to 3:402 if you just loaded the data
dup1 = which(duplicated(trainDF[,3:402]))
dup2 = which(duplicated(trainDF[,3:402], fromLast = TRUE))
dup_rows = sort(c(dup1, dup2))
trainDF[dup_rows,1:5]
# row 3055 is classified as a "1",  but it has the same pixel values as row 4106, which is classified as an "E"
# it's definitely a 1, so we will remove row 4106
trainDF = trainDF[-c(2297, 3226, 4106, 4598, 4820, 5208, 6270), ]

# display image of specified row of trainDF
row_num = 4165
m = matrix(unlist(trainDF[row_num,-c(1,2)]),nrow = 20,byrow = F)
image(m,col=grey.colors(255), xlab = trainDF$Class[row_num])

# everything checks out, so I will remove the ID column
trainDF = trainDF[,-1]
rm(trainLabels)

# write trainDF and testDF to R Data Sets
saveRDS(testDF, "data/testDF.RDS")
saveRDS(trainDF, "data/trainDF.RDS")


