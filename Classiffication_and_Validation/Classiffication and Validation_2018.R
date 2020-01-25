#Load libraries
library(mlbench)
library(caret)
library(rasterVis)

#Raster Data for the Study areas in 2018
r_data_2018_OS <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_1.tif")
r_data_2018_EL <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_2.tif")
r_data_2018_WA <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_3.tif")
r_data_2018_SC <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_4.tif")
r_data_2018_KR <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_5.tif")

#Read SVM Model
modelFit_svm_all <- readRDS(file = "modelFit_svm.rds")

Classify <- function(input_raster, output_class, file_name){
  names(input_raster) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")
  beginCluster()
  output_class <- clusterR(input_raster, raster::predict, args = list(model = modelFit_svm_all))
  endCluster()
  writeRaster(output_class,filename =file_name,datatype = "INT1U", format = "GTiff")
  cpal <- c('white','navy','green','orange')
  output_class <- ratify(output_class)
  rat_rf <- levels(output_class)[[1]]
  rat_rf$Landcover <-c('Site', 'Site Water', 'Vegetation' ,'Fields')
  levels(output_class) <- rat_rf
  levelplot(output_class,col.regions=cpal)
}

#Classifying images from 2018
Classify(r_data_2018_OS, classify_2018_OS, "C:/Users/Gift/Dropbox/Final_Output/classify_2018_OS.tiff")
Classify(r_data_2018_EL, classify_2018_EL, "C:/Users/Gift/Dropbox/Final_Output/classify_2018_EL.tiff")
Classify(r_data_2018_WA, classify_2018_WA, "C:/Users/Gift/Dropbox/Final_Output/classify_2018_WA.tiff")
Classify(r_data_2018_SC, classify_2018_SC, "C:/Users/Gift/Dropbox/Final_Output/classify_2018_SC.tiff")
Classify(r_data_2018_KR, classify_2018_KR, "C:/Users/Gift/Dropbox/Final_Output/classify_2018_KR.tiff")

#Confusion Matrix SVM : External Validation
Predict <- function(val,pred_val,c_val){
  pred_val <- predict(modelFit_svm_all,val, type = "raw")
  c_val <- as.factor(val$class)
  confusionMatrix(pred_val, c_val)
}

#Reading validation data
df_Validataion_2018_1 = readRDS("df_Validation_2018_1.rds")
df_Validataion_2018_2 = readRDS("df_Validation_2018_2.rds")
df_Validataion_2018_3 = readRDS("df_Validation_2018_3.rds")
df_Validataion_2018_4 = readRDS("df_Validation_2018_4.rds")
df_Validataion_2018_5 = readRDS("df_Validation_2018_5.rds")

# Call Confusion Matrix function
Predict(df_Validataion_2018_1,p_Val_2018_svm_1, c_Val_2018_svm_1)
Predict(df_Validataion_2018_2,p_Val_2018_svm_2, c_Val_2018_svm_2)
Predict(df_Validataion_2018_3,p_Val_2018_svm_3, c_Val_2018_svm_3)
Predict(df_Validataion_2018_4,p_Val_2018_svm_4, c_Val_2018_svm_4)
Predict(df_Validataion_2018_5,p_Val_2018_svm_5, c_Val_2018_svm_4)
