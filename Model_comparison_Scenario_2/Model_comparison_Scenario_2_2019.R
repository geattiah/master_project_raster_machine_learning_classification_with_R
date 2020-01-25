#Packages required to run the process
install.packages("mlbench")
install.packages("caret")
install.packages("rasterVis")

#Load the installed packages
library(mlbench)
library(caret)
library(rasterVis)

#Total Training data for classification
trainingData <- shapefile("C:/Users/Gift/Desktop/Data/Training/2019/2019_Classification.shp")
responseColumn <- "class"

#Load Raster and Rename Data for the Osterby study area of 2019
r_data_2019_OS <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_1.tif")
names(r_data_2019_OS) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2019_EL <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_2.tif")
names(r_data_2019_EL) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2019_WA <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_3.tif")
names(r_data_2019_WA) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2019_SC <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_4.tif")
names(r_data_2019_SC) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2019_KR <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_5.tif")
names(r_data_2019_KR) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

#Combine all training data
df_training_2019 <- rbind(df_training_2019_OS, df_training_2019_EL, df_training_2019_WA,
                          df_training_2019_SC, df_training_2019_KR)
saveRDS(df_training_2019, file = "df_training_2019.rds")
df_training_2019 = readRDS("df_training_2019.rds")

#Develop the Models for all ALgorithms
control <- trainControl(method="repeatedcv", number=10, repeats=5)
# Random Forest
set.seed(7)
modelFit_rf_2019_yr <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2019, trControl=control)

#Support Vector Machine
set.seed(7)
modelFit_svm_2019_yr <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2019, trControl=control)

#Neural Network
set.seed(7)
modelFit_nn_2019_yr <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2019, trControl=control)

#Accuracy results
results_2019 <- resamples(list(RF_2019=modelFit_rf_2019_yr, SVM_2019=modelFit_svm_2019_yr, NN_2019=modelFit_nn_2019_yr))
summary(results_2019)

#Function to Classify, plot and save results of all models for Osterby
Classify <- function(input_raster, output_class, file_name, model_type){
  beginCluster()
  output_class <- clusterR(input_raster, raster::predict, args = list(model = model_type))
  endCluster()
  writeRaster(output_class,filename =file_name,datatype = "INT1U", format = "GTiff")
  cpal <- c('white','navy','green','orange')
  output_class <- ratify(output_class)
  rat_rf <- levels(output_class)[[1]]
  rat_rf$Landcover <-c('Site', 'Site Water', 'Vegetation' ,'Fields')
  levels(output_class) <- rat_rf
  levelplot(output_class,col.regions=cpal)
}

#Classify and plot all models for Osterby
Classify(classify_rf_2019_OS_yearly, r_data_2019_OS_yearly, modelFit_rf_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_OS_yearly_cl_rf.tif")
Classify(classify_svm_2019_OS_yearly, r_data_2019_OS_yearly, modelFit_svm_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_OS_yearly_cl_svm.tif")
Classify(classify_nn_2019_OS_yearly, r_data_2019_OS_yearly, modelFit_nn_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_OS_yearly_cl_nn.tif")
Classify(classify_rf_2019_EL_yearly, r_data_2019_EL_yearly, modelFit_rf_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_EL_yearly_cl_rf.tif")
Classify(classify_svm_2019_EL_yearly, r_data_2019_EL_yearly, modelFit_svm_2019_yr,  "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_EL_yearly_cl_svm.tif")
Classify(classify_nn_2019_EL_yearly, r_data_2019_EL_yearly, modelFit_nn_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_EL_yearly_cl_nn.tif")
Classify(classify_rf_2019_WA_yearly, r_data_2019_WA_yearly, modelFit_rf_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_WA_yearly_cl_rf.tif")
Classify(classify_svm_2019_WA_yearly, r_data_2019_WA_yearly, modelFit_svm_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_WA_yearly_cl_svm.tif")
Classify(classify_nn_2019_WA_yearly, r_data_2019_WA_yearly, modelFit_nn_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_WA_yearly_cl_nn.tif")
Classify(classify_rf_2019_SC_yearly, r_data_2019_SC_yearly, modelFit_rf_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_SC_yearly_cl_rf.tif")
Classify(classify_svm_2019_SC_yearly, r_data_2019_SC_yearly, modelFit_svm_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_SC_yearly_cl_svm.tif")
Classify(classify_nn_2019_SC_yearly, r_data_2019_SC_yearly, modelFit_nn_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_SC_yearly_cl_nn.tif")
Classify(classify_rf_2019_KR_yearly, r_data_2019_KR_yearly, modelFit_rf_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_KR_yearly_cl_rf.tif")
Classify(classify_svm_2019_KR_yearly, r_data_2019_KR_yearly, modelFit_svm_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_KR_yearly_cl_svm.tif")
Classify(classify_nn_2019_KR_yearly, r_data_2019_KR_yearly, modelFit_nn_2019_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2019_Tiff/2019_KR_yearly_cl_nn.tif")
