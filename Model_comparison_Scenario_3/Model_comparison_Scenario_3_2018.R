install.packages("mlbench")
install.packages("caret")
install.packages("rasterVis")

#Load the installed packages
library(mlbench)
library(caret)
library(rasterVis)

#Total Training data for classification
trainingData <- shapefile("C:/Users/Gift/Desktop/Data/Training/2018/2018_Classification.shp")
responseColumn <- "class"

#Load Raster and Rename Data for the Osterby study area of 2018
r_data_2018_OS <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_1.tif")
names(r_data_2018_OS) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2018_EL <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_2.tif")
names(r_data_2018_EL) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2018_WA <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_WA.tif")
names(r_data_2018_WA) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2018_SC <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_4.tif")
names(r_data_2018_SC) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

r_data_2018_KR <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_KR.tif")
names(r_data_2018_KR) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")

#Read All the training data 
df_training = readRDS("df_training.rds")

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# Random Forest
set.seed(7)
modelFit_rf <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training, trControl=control)
modelFit_rf

#Support Vector Machine
set.seed(7)
modelFit_svm <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training, trControl=control)
modelFit_svm

#Neural Network
set.seed(7)
modelFit_nn_ <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training, trControl=control)
modelFit_nn_

#Internal accuracy results
results <- resamples(list(RF=modelFit_rf, SVM=modelFit_svm, NN=modelFit_nn_))
summary(results)

#Save Model
saveRDS(modelFit_rf, file = "modelFit_rf.rds")
saveRDS(modelFit_svm, file = "modelFit_svm.rds")
saveRDS(modelFit_nn_, file = "modelFit_nn.rds")

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
Classify(classify_rf_2018_OS_all, r_data_2018_OS_all, modelFit_rf_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_OS_all_cl_rf.tif")
Classify(classify_svm_2018_OS_all, r_data_2018_OS_all, modelFit_svm_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_OS_all_cl_svm.tif")
Classify(classify_nn_2018_OS_all, r_data_2018_OS_all, modelFit_nn_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_OS_all_cl_nn.tif")
Classify(classify_rf_2018_EL_all, r_data_2018_EL_all, modelFit_rf_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_EL_all_cl_rf.tif")
Classify(classify_svm_2018_EL_all, r_data_2018_EL_all, modelFit_svm_2018_yr,  "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_EL_all_cl_svm.tif")
Classify(classify_nn_2018_EL_all, r_data_2018_EL_all, modelFit_nn_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_EL_all_cl_nn.tif")
Classify(classify_rf_2018_WA_all, r_data_2018_WA_all, modelFit_rf_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_WA_all_cl_rf.tif")
Classify(classify_svm_2018_WA_all, r_data_2018_WA_all, modelFit_svm_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_WA_all_cl_svm.tif")
Classify(classify_nn_2018_WA_all, r_data_2018_WA_all, modelFit_nn_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_WA_all_cl_nn.tif")
Classify(classify_rf_2018_SC_all, r_data_2018_SC_all, modelFit_rf_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_SC_all_cl_rf.tif")
Classify(classify_svm_2018_SC_all, r_data_2018_SC_all, modelFit_svm_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_SC_all_cl_svm.tif")
Classify(classify_nn_2018_SC_all, r_data_2018_SC_all, modelFit_nn_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_SC_all_cl_nn.tif")
Classify(classify_rf_2018_KR_all, r_data_2018_KR_all, modelFit_rf_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_KR_all_cl_rf.tif")
Classify(classify_svm_2018_KR_all, r_data_2018_KR_all, modelFit_svm_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_KR_all_cl_svm.tif")
Classify(classify_nn_2018_KR_all, r_data_2018_KR_all, modelFit_nn_2018_yr, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2018_Tiff/2018_KR_all_cl_nn.tif")

#Validation assessment
df_Validataion_2018_OS = readRDS("df_Validation_2018_OS.rds")
df_Validataion_2018_EL = readRDS("df_Validation_2018_EL.rds")
df_Validataion_2018_WA = readRDS("df_Validation_2018_WA.rds")
df_Validataion_2018_SC = readRDS("df_Validation_2018_SC.rds")
df_Validataion_2018_KR = readRDS("df_Validation_2018_KR.rds")

#Confusion Matrix RF with external validation data
p_Validation_2018_rf_OS<-predict(modelFit_rf,df_Validataion_2018_OS, type = "raw")
c_Validation_2018_rf_OS <- as.factor(df_Validataion_2018_OS$class)
confusionMatrix(p_Validation_2018_rf_OS, c_Validation_2018_rf_OS)

p_Validation_2018_rf_EL<-predict(modelFit_rf,df_Validataion_2018_EL, type = "raw")
c_Validation_2018_rf_EL <- as.factor(df_Validataion_2018_EL$class)
confusionMatrix(p_Validation_2018_rf_EL, c_Validation_2018_rf_EL)

p_Validation_2018_rf_WA<-predict(modelFit_rf,df_Validataion_2018_WA, type = "raw")
c_Validation_2018_rf_WA <- as.factor(df_Validataion_2018_WA$class)
confusionMatrix(p_Validation_2018_rf_WA, c_Validation_2018_rf_WA)

p_Validation_2018_rf_SC<-predict(modelFit_rf,df_Validataion_2018_SC, type = "raw")
c_Validation_2018_rf_SC <- as.factor(df_Validataion_2018_SC$class)
confusionMatrix(p_Validation_2018_rf_SC, c_Validation_2018_rf_SC)

p_Validation_2018_rf_KR<-predict(modelFit_rf,df_Validataion_2018_KR, type = "raw")
c_Validation_2018_rf_KR <- as.factor(df_Validataion_2018_KR$class)
confusionMatrix(p_Validation_2018_rf_KR, c_Validation_2018_rf_KR)

#Confusion Matrix SVM with external validation data
p_Validation_2018_svm_OS<-predict(modelFit_svm,df_Validataion_2018_OS, type = "raw")
c_Validation_2018_svm_OS <- as.factor(df_Validataion_2018_OS$class)
confusionMatrix(p_Validation_2018_svm_OS, c_Validation_2018_svm_OS)

p_Validation_2018_svm_EL<-predict(modelFit_svm,df_Validataion_2018_EL, type = "raw")
c_Validation_2018_svm_EL <- as.factor(df_Validataion_2018_EL$class)
confusionMatrix(p_Validation_2018_svm_EL, c_Validation_2018_svm_EL)

p_Validation_2018_svm_WA<-predict(modelFit_svm,df_Validataion_2018_WA, type = "raw")
c_Validation_2018_svm_WA <- as.factor(df_Validataion_2018_WA$class)
confusionMatrix(p_Validation_2018_svm_WA, c_Validation_2018_svm_WA)

p_Validation_2018_svm_SC<-predict(modelFit_svm,df_Validataion_2018_SC, type = "raw")
c_Validation_2018_svm_SC <- as.factor(df_Validataion_2018_SC$class)
confusionMatrix(p_Validation_2018_svm_SC, c_Validation_2018_svm_SC)

p_Validation_2018_svm_KR<-predict(modelFit_svm,df_Validataion_2018_KR, type = "raw")
c_Validation_2018_svm_KR <- as.factor(df_Validataion_2018_KR$class)
confusionMatrix(p_Validation_2018_svm_KR, c_Validation_2018_svm_KR)

#Confusion Matrix ANN with external validation data
p_Validation_2018_nn_OS<-predict(modelFit_nn,df_Validataion_2018_OS, type = "raw")
c_Validation_2018_nn_OS <- as.factor(df_Validataion_2018_OS$class)
confusionMatrix(p_Validation_2018_nn_OS, c_Validation_2018_nn_OS)

p_Validation_2018_nn_EL<-predict(modelFit_nn,df_Validataion_2018_EL, type = "raw")
c_Validation_2018_nn_EL <- as.factor(df_Validataion_2018_EL$class)
confusionMatrix(p_Validation_2018_nn_EL, c_Validation_2018_nn_EL)

p_Validation_2018_nn_WA<-predict(modelFit_nn,df_Validataion_2018_WA, type = "raw")
c_Validation_2018_nn_WA <- as.factor(df_Validataion_2018_WA$class)
confusionMatrix(p_Validation_2018_nn_WA, c_Validation_2018_nn_WA)

p_Validation_2018_nn_SC<-predict(modelFit_nn,df_Validataion_2018_SC, type = "raw")
c_Validation_2018_nn_SC <- as.factor(df_Validataion_2018_SC$class)
confusionMatrix(p_Validation_2018_nn_SC, c_Validation_2018_nn_SC)

p_Validation_2018_nn_KR<-predict(modelFit_nn,df_Validataion_2018_KR, type = "raw")
c_Validation_2018_nn_KR <- as.factor(df_Validataion_2018_KR$class)
confusionMatrix(p_Validation_2018_nn_KR, c_Validation_2018_nn_KR)
