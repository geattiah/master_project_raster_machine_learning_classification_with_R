#Load libraries
library(mlbench)
library(caret)
library(rasterVis)

#Raster Data for the Study areas in 2015
r_data_2015_OS <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2015_Tiff/2015_1.tif")
r_data_2015_EL <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2015_Tiff/2015_2.tif")
r_data_2015_WA <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2015_Tiff/2015_3.tif")
r_data_2015_SC <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2015_Tiff/2015_4.tif")
r_data_2015_KR <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2015_Tiff/2015_5.tif")

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

#Classifying images from 2015
Classify(r_data_2015_OS, classify_2015_OS, "C:/Users/Gift/Dropbox/Final_Output/classify_2015_OS.tiff")
Classify(r_data_2015_EL, classify_2015_EL, "C:/Users/Gift/Dropbox/Final_Output/classify_2015_EL.tiff")
Classify(r_data_2015_WA, classify_2015_WA, "C:/Users/Gift/Dropbox/Final_Output/classify_2015_WA.tiff")
Classify(r_data_2015_SC, classify_2015_SC, "C:/Users/Gift/Dropbox/Final_Output/classify_2015_SC.tiff")
Classify(r_data_2015_KR, classify_2015_KR, "C:/Users/Gift/Dropbox/Final_Output/classify_2015_KR.tiff")

#Confusion Matrix SVM : External Validation
Predict <- function(val,pred_val,c_val){
  pred_val <- predict(modelFit_svm_all,val, type = "raw")
  c_val <- as.factor(val$class)
  confusionMatrix(pred_val, c_val)
}

#Reading validation data
df_Validataion_2015_1 = readRDS("df_Validation_2015_1.rds")
df_Validataion_2015_2 = readRDS("df_Validation_2015_2.rds")
df_Validataion_2015_3 = readRDS("df_Validation_2015_3.rds")
df_Validataion_2015_4 = readRDS("df_Validation_2015_4.rds")
df_Validataion_2015_5 = readRDS("df_Validation_2015_5.rds")

# Call Confusion Matrix function
Predict(df_Validataion_2015_1,p_Val_2015_svm_1, c_Val_2015_svm_1)
Predict(df_Validataion_2015_2,p_Val_2015_svm_2, c_Val_2015_svm_2)
Predict(df_Validataion_2015_3,p_Val_2015_svm_3, c_Val_2015_svm_3)
Predict(df_Validataion_2015_4,p_Val_2015_svm_4, c_Val_2015_svm_4)
Predict(df_Validataion_2015_5,p_Val_2015_svm_5, c_Val_2015_svm_4)
