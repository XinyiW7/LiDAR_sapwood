install.packages('magrittr')
install.packages('lidR')
install.packages('data.table')
library(magrittr)
library(lidR)
library(data.table)
devtools::install_github("tiagodc/TreeLS")
library(TreeLS)
library(rgl)
install.packages('rglwidget')
library(rglwidget)
options(rgl.printRglwidget = TRUE)

stl = rgl::readSTL("/home/siruiy1/Documents/xinyiWang/Plot1/1.stl", plot = FALSE)

# Creation of a LAS object out of external data
data <- data.frame(X = stl[,1],
                   Y = stl[,2],
                   Z = stl[,3])

# Create a default header and quantize *by reference*
# the coordinates to fit with offset and scale factors
tls <- LAS(data)
plot(tls)
X <- tls$X
Y <- tls$Y
Z <- tls$Z
tls$X <- Z
tls$Y <- X
tls$Z <- Y
plot(tls)

# normalize the point cloud
tls = tlsNormalize(tls, keep_ground = F)
x = plot(tls)
rgl::axes3d(col='white')

# tls@data[which(tls@data$Z>3),] <- NA
#tls2 = tlsCrop(tls, 2.6,3 , 1, FALSE, FALSE)
#tls2 <- tls
#x2=plot(tls2)
#rgl::axes3d(col='white')

# Identify stem points
tls = tlsNormalize(tls)
tls = stemPoints(tls, stm.eigen.knn(voxel_spacing = .02))


# Calculate metrics for each height
dbh_algo = shapeFit(shape='circle', algorithm = 'ransac', n=20, inliers=.95, z_dev=10)
inv_05 = tlsInventory(tls, d_method = dbh_algo, dh = 0.5)
inv_05
add_tlsInventory(x, inv_05)
inv_1 = tlsInventory(tls, d_method = dbh_algo, dh = 1)
inv_1
add_tlsInventory(x, inv_1)
inv_15 = tlsInventory(tls, d_method = dbh_algo, dh = 1.5)
inv_15
add_tlsInventory(x, inv_15)
inv_2 = tlsInventory(tls, d_method = dbh_algo, dh = 2)
inv_2
add_tlsInventory(x, inv_2)
inv_25 = tlsInventory(tls,  d_method = dbh_algo, dh = 2.5)
inv_25
add_tlsInventory(x, inv_25)
inv_3 = tlsInventory(tls, d_method = dbh_algo, dh = 3)
inv_3
add_tlsInventory(x, inv_3)
inv_1_3 = tlsInventory(tls, hp = .95, d_method = dbh_algo, dh = 1.3)
inv_1_3
add_tlsInventory(x, inv_1_3)

TreeLS::writeTLS(tls, file = "/home/siruiy1/Documents/xinyiWang/fas/plot1/1.laz")

