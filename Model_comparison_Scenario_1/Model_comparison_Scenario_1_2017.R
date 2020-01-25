#Packages required to run the process
install.packages("mlbench")
install.packages("caret")
install.packages("rasterVis")

#Load the installed packages
library(mlbench)
library(caret)
library(rasterVis)

#Total Training data for classification
trainingData <- shapefile("C:/Users/Gift/Desktop/Data/Training/2017/2017_Classification.shp")
responseColumn <- "class"

#Raster Data for the Osterby study area of 2017
r_data_2017_OS <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_1.tif")
r_data_2017_EL <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_2.tif")
r_data_2017_WA <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_3.tif")
r_data_2017_SC <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_4.tif")
r_data_2017_KR <- brick("C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_5.tif")


#Function to Extract pixels values of the training samples found in Osterby study area and plot the area in data frame format
Extract <- function(new_trainData,input_raster){
  names(input_raster) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2", "CI", "RI", "SI", "NDVI", "NDWI", "BI","Elevation")
  plotRGB(input_raster * (input_raster >= 0), r = 4, g = 2, b = 1)
  new_trainData = data.frame(matrix(vector(), nrow = 0, ncol = length(names(input_raster)) + 1))   
  for (i in 1:length(unique(trainingData[[responseColumn]]))){
    categories <- unique(trainingData[[responseColumn]])[i]
    categoriesMap <- trainingData[trainingData[[responseColumn]] == categories,]
    dataVal <- extract(input_raster, categoriesMap)
    if(is(trainingData, "SpatialPointsDataFrame")){
      dataVal <- cbind(dataVal, class = as.numeric(rep(categories, nrow(dataVal))))
      new_trainData <- rbind(new_trainData, dataVal[complete.cases(dataVal),])
    }
    if(is(trainingData, "SpatialPolygonsDataFrame")){
      dataVal <- dataVal[!unlist(lapply(dataVal, is.null))]
      dataVal <- lapply(dataVal, function(x){cbind(x, class = as.numeric(rep(categories, nrow(x))))})
      df_data <- do.call("rbind", dataVal)
      new_trainData <- rbind(new_trainData, df_data)
    }
  }
  return(new_trainData)
}

#Extract the pixels values of the training samples found in Osterby study area in data frame format
df_training_2017_OS <- Extract(df_training_2017_OS,r_data_2017_OS )
df_training_2017_EL <- Extract(df_training_2017_EL,r_data_2017_EL )
df_training_2017_WA <- Extract(df_training_2017_WA,r_data_2017_WA )
df_training_2017_SC <- Extract(df_training_2017_SC,r_data_2017_SC )
df_training_2017_KR <- Extract(df_training_2017_SC,r_data_2017_KR )


#Training all 2017 data with the three models Random Forest, Support Vector Machine and Neural Network
#set control for model assessment
control <- trainControl(method="repeatedcv", number=10, repeats=3)

# Random Forest
set.seed(7)
modelFit_rf_2017_OS <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2017_OS, trControl=control)
set.seed(7)
modelFit_rf_2017_EL <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2017_EL, trControl=control)
set.seed(7)
modelFit_rf_2017_WA <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2017_WA, trControl=control)
set.seed(7)
modelFit_rf_2017_SC <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2017_SC, trControl=control)
set.seed(7)
modelFit_rf_2017_KR <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "rf", data = df_training_2017_KR, trControl=control)

#Support Vector Machine
set.seed(7)
modelFit_svm_2017_OS <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2017_OS, trControl=control)
set.seed(7)
modelFit_svm_2017_EL <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2017_EL, trControl=control)
set.seed(7)
modelFit_svm_2017_WA <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2017_WA, trControl=control)
set.seed(7)
modelFit_svm_2017_SC <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2017_SC, trControl=control)
set.seed(7)
modelFit_svm_2017_KR <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "svmLinear", data = df_training_2017_KR, trControl=control)

#Neural Network
set.seed(7)
modelFit_nn_2017_OS <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2017_OS, trControl=control)
set.seed(7)
modelFit_nn_2017_EL <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2017_EL, trControl=control)
set.seed(7)
modelFit_nn_2017_WA <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2017_WA, trControl=control)
set.seed(7)
modelFit_nn_2017_SC <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2017_SC, trControl=control)
set.seed(7)
modelFit_nn_2017_KR <- train(as.factor(class) ~ blue + green + red + NIR + SWIR1 + SWIR2 + CI + RI + SI + NDVI + BI + Elevation, method = "pcaNNet", data = df_training_2017_KR, trControl=control)

# Accuracy results of all the Models 
results_2017_OS <- resamples(list(RF_2017_OS=modelFit_rf_2017_OS, SVM_2017_OS=modelFit_svm_2017_OS, NN_2017_OS=modelFit_nn_2017_OS))
summary(results_2017_OS)

results_2017_EL <- resamples(list(RF_2017_EL=modelFit_rf_2017_EL, SVM_2017_EL=modelFit_svm_2017_EL, NN_2017_EL=modelFit_nn_2017_EL)) 
summary(results_2017_EL)

results_2017_WA <- resamples(list(RF_2017_WA=modelFit_rf_2017_WA, SVM_2017_WA=modelFit_svm_2017_WA, NN_2017_WA=modelFit_nn_2017_WA))
summary(results_2017_WA)

results_2017_SC <- resamples(list(RF_2017_SC=modelFit_rf_2017_SC, SVM_2017_SC=modelFit_svm_2017_SC, NN_2017_SC=modelFit_nn_2017_SC))
summary(results_2017_SC)

results_2017_KR <- resamples(list(RF_2017_KR=modelFit_rf_2017_KR, SVM_2017_KR=modelFit_svm_2017_KR, NN_2017_KR=modelFit_nn_2017_KR))
summary(results_2017_KR)

#bwplot showing different levels of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results_2017_OS, scales=scales)
bwplot(results_2017_EL, scales=scales)
bwplot(results_2017_WA, scales=scales)
bwplot(results_2017_SC, scales=scales)
bwplot(results_2017_KR, scales=scales)

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

#Classify and plot all models for Site 1
Classify(classify_rf_2017_OS, r_data_2017_OS, modelFit_rf_2017_OS, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_OS_cl_rf.tif")
Classify(classify_svm_2017_OS, r_data_2017_OS, modelFit_svm_2017_OS, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_OS_cl_svm.tif")
Classify(classify_nn_2017_OS, r_data_2017_OS, modelFit_nn_2017_OS, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_OS_cl_nn.tif")
Classify(classify_rf_2017_EL, r_data_2017_EL, modelFit_rf_2017_EL, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_EL_cl_rf.tif")
Classify(classify_svm_2017_EL, r_data_2017_EL, modelFit_svm_2017_EL,  "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_EL_cl_svm.tif")
Classify(classify_nn_2017_EL, r_data_2017_EL, modelFit_nn_2017_EL, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_EL_cl_nn.tif")
Classify(classify_rf_2017_WA, r_data_2017_WA, modelFit_rf_2017_WA, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_WA_cl_rf.tif")
Classify(classify_svm_2017_WA, r_data_2017_WA, modelFit_svm_2017_WA, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_WA_cl_svm.tif")
Classify(classify_nn_2017_WA, r_data_2017_WA, modelFit_nn_2017_WA, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_WA_cl_nn.tif")
Classify(classify_rf_2017_SC, r_data_2017_SC, modelFit_rf_2017_SC, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_SC_cl_rf.tif")
Classify(classify_svm_2017_SC, r_data_2017_SC, modelFit_svm_2017_SC, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_SC_cl_svm.tif")
Classify(classify_nn_2017_SC, r_data_2017_SC, modelFit_nn_2017_SC, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_SC_cl_nn.tif")
Classify(classify_rf_2017_KR, r_data_2017_KR, modelFit_rf_2017_KR, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_KR_cl_rf.tif")
Classify(classify_svm_2017_KR, r_data_2017_KR, modelFit_svm_2017_KR, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_KR_cl_svm.tif")
Classify(classify_nn_2017_KR, r_data_2017_KR, modelFit_nn_2017_SC, "C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_KR_cl_nn.tif")


#Function to export on mined area and save 
classify_rf_2017_OS_site <- classify_rf_2017_OS
classify_rf_2017_OS_site[classify_rf_2017_OS_site>1] <- NA
plot(classify_rf_2017_OS_site)

classify_rf_2017_EL_site <- classify_rf_2017_EL
classify_rf_2017_EL_site[classify_rf_2017_EL_site>1] <- NA
plot(classify_rf_2017_EL_site)

classify_rf_2017_WA_site <- classify_rf_2017_WA
classify_rf_2017_WA_site[classify_rf_2017_WA_site>1] <- NA
plot(classify_rf_2017_WA_site)

classify_rf_2017_SC_site <- classify_rf_2017_SC
classify_rf_2017_SC_site[classify_rf_2017_SC_site>1] <- NA
plot(classify_rf_2017_SC_site)

classify_rf_2017_KR_site <- classify_rf_2017_KR
classify_rf_2017_KR_site[classify_rf_2017_KR_site>1] <- NA
plot(classify_rf_2017_KR_site)

writeRaster(classify_rf_2017_OS_site,"C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_site/2017_site_OS.tif")
writeRaster(classify_rf_2017_EL_site,"C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_site/2017_site_EL.tif")
writeRaster(classify_rf_2017_WA_site,"C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_site/2017_site_WA.tif")
writeRaster(classify_rf_2017_SC_site,"C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_site/2017_site_SC.tif")
writeRaster(classify_rf_2017_KR_site,"C:/Users/Gift/Desktop/Data/Try_Th/Tiffs/2017_Tiff/2017_site/2017_site_KR.tif")
