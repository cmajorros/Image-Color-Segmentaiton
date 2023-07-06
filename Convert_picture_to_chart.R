
library(imager)

image<- load.image("Your path of the picture")

# Convert the image to a cimg object
cimg <- as.cimg(image)
df <- as.data.frame(image)
nrow(df)

# Get the dimensions of the image
height <- dim(cimg)[1]
width <- dim(cimg)[2]

# Create empty matrices for R, G, and B values
red <- matrix(0, nrow = height, ncol = width)
green <- matrix(0, nrow = height, ncol = width)
blue <- matrix(0, nrow = height, ncol = width)


# Loop through each pixel and extract the R, G, and B values
for (i in 1:height) {
  for (j in 1:width) {
    pixel <- cimg[i, j, ]
    red[i, j] <- pixel[1]
    green[i, j] <- pixel[2]
    blue[i, j] <- pixel[3]
  }
}

# Output the resulting matrices for R, G, and B values
red
green
blue


df_red <- as.data.frame(red)
df_green <- as.data.frame(green)
df_blue <- as.data.frame(blue)



# Get the dimensions of df_red
rows <- nrow(df_red)
cols <- ncol(df_red)
total_row <- nrow(df)

# Create an empty dataframe with 60000 rows and 3 columns
df_new_red <- data.frame(x = numeric(total_row), y = numeric(total_row), z = numeric(total_row))

# Fill in the values of x, y, and z
k <- 1
for (i in 1:rows) {
  for (j in 1:cols) {
    df_new_red[k, "x"] <- j
    df_new_red[k, "y"] <- i
    df_new_red[k, "z"] <- df_red[i, j]
    k <- k + 1
  }
}



df_new_green <- data.frame(x = numeric(total_row), y = numeric(total_row), z = numeric(total_row))

# Output the resulting dataframe df_new
k <- 1
for (i in 1:rows) {
  for (j in 1:cols) {
    df_new_green[k, "x"] <- j
    df_new_green[k, "y"] <- i
    df_new_green[k, "z"] <- df_green[i, j]
    k <- k + 1
  }
}


df_new_blue <- data.frame(x = numeric(total_row), y = numeric(total_row), z = numeric(total_row))

# Output the resulting dataframe df_new
k <- 1
for (i in 1:rows) {
  for (j in 1:cols) {
    df_new_blue[k, "x"] <- j
    df_new_blue[k, "y"] <- i
    df_new_blue[k, "z"] <- df_blue[i, j]
    k <- k + 1
  }
}

print(df_new_red)


library(dplyr)
## merge rgb value
df_merge <- left_join(df, df_new_red , b = c("x" = "x", "y" = "y"), keep = FALSE)
df_merge <- df_merge %>%
  dplyr::rename( Red = z )

df_merge <- left_join(df_merge , df_new_green , b = c("x" = "x", "y" = "y"), keep = FALSE)
df_merge <- df_merge %>%
  dplyr::rename( Green = z )

df_merge <- left_join(df_merge , df_new_blue , b = c("x" = "x", "y" = "y"), keep = FALSE)
df_merge <- df_merge %>%
  dplyr::rename( Blue = z )

df_merge$Red <- as.numeric(df_merge$Red) 
df_merge$Green <- as.numeric(df_merge$Green)
df_merge$Blue <- as.numeric(df_merge$Blue) 

#Cluster the color
selected_cols <- c("Red", "Green", "Blue")
selected_data <- df_merge[selected_cols]

# Perform k-means clustering
k <- 15  # Number of clusters
nan_values <- is.na(selected_data)
# Remove rows with missing values
selected_data <- selected_data[complete.cases(selected_data), ]

# Impute missing values with mean
selected_data[is.na(selected_data)] <- mean(selected_data, na.rm = TRUE)

cluster <- kmeans(selected_data, centers = k, nstart = 1)

#See the center of cluster and size
cluster$centers
cluster$size


# I need only RBG info not the cluster to export to work somewher
write.table(df_merge, "YourPathToSave/Draw.txt", sep="\t", row.names = F)


