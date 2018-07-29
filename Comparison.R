compare_20_37 = data.frame(ROC20 = c(0.8855,0.8842,0.8827,0.8921,0.8837),
                              Roc37 = c(0.8898,0.8912,0.8861,0.8974,0.8896),
                              Fmeasure20 = table[,2], Fmeasure37 =Table[1:5,2],
                              Accuracy20 = table[,3], Accuracy37= Table[1:5,3])
rownames(compare_20_37) = c("kNN","Random Forests","Support vector Machine",
                            "Gradient Boost", "Generalied Linear Model"   )
barplot(as.matrix(compare_20_37)[,1:2], beside = T, ylim = c(0.75,0.95), 
    legend.text = T, col= rainbow(5), args.legend = c(xjust = 0.5, yjust=0))

# comparing through the ROC curves
library(pROC)
#knn
plot.roc(KNNFit$pred$obs[KNNFit$pred$k == 125],KNNFit$pred$X0[KNNFit$pred$k == 125], col = "blue", main = "ROC curves for KNN")
plot.roc(knnFit$pred$obs[knnFit$pred$k == 101],knnFit$pred$X0[knnFit$pred$k == 101],add=T, col = "green")
plot.roc(KNNFit_imputed$pred$obs[KNNFit_imputed$pred$k == 125],KNNFit_imputed$pred$X0[KNNFit_imputed$pred$k == 125], add = T,col = "red")
#random forests
plot.roc(RFFit$pred$obs[RFFit$pred$mtry == 2],RFFit$pred$X0[RFFit$pred$mtry == 2], col = "blue", main = "ROC curves for Random Forests")
plot.roc(rfFit$pred$obs[rfFit$pred$mtry == 2],rfFit$pred$X0[rfFit$pred$mtry == 2], add= T, col = "green")
plot.roc(RFFit_imputed$pred$obs[RFFit_imputed$pred$mtry == 2],RFFit_imputed$pred$X0[RFFit_imputed$pred$mtry == 2], add= T, col = "red")
# Support Vector Machines
plot.roc(SVM_linearFit$pred$obs[SVM_linearFit$pred$C==0.05], SVM_linearFit$pred$X0[SVM_linearFit$pred$C==0.05],col = "blue", main = "ROC curves for SVM")
plot.roc(svm_linearFit_tuned$pred$obs[svm_linearFit_tuned$pred$C==0.05], svm_linearFit_tuned$pred$X0[svm_linearFit_tuned$pred$C==0.05], add = T,col = "green")
plot.roc(SVM_imputed$pred$obs[SVM_imputed$pred$C==0.05], SVM_imputed$pred$X0[SVM_imputed$pred$C==0.05], add = T,col = "red")
# Gradient Boost
plot.roc(GBMFit$pred$obs[GBMFit$pred$interaction.depth==6 & GBMFit$pred$n.trees==450 & GBMFit$pred$shrinkage==0.1& GBMFit$pred$n.minobsinnode == 10], GBMFit$pred$X0[GBMFit$pred$interaction.depth==6 & GBMFit$pred$n.trees==450 & GBMFit$pred$shrinkage==0.1& GBMFit$pred$n.minobsinnode == 10], col = "blue", main = "ROC curves for GBM")
plot.roc(gbmFit$pred$obs[gbmFit$pred$interaction.depth==10 & gbmFit$pred$n.trees==100 & gbmFit$pred$shrinkage==0.1& gbmFit$pred$n.minobsinnode == 10], gbmFit$pred$X0[gbmFit$pred$interaction.depth==10 & gbmFit$pred$n.trees==100 & gbmFit$pred$shrinkage==0.1& gbmFit$pred$n.minobsinnode == 10], add = T, col = "green")
plot.roc(GBMFit_imputed$pred$obs[GBMFit_imputed$pred$interaction.depth==6 & GBMFit_imputed$pred$n.trees==450 & GBMFit_imputed$pred$shrinkage==0.1& GBMFit_imputed$pred$n.minobsinnode == 10], GBMFit_imputed$pred$X0[GBMFit_imputed$pred$interaction.depth==6 & GBMFit_imputed$pred$n.trees==450 & GBMFit_imputed$pred$shrinkage==0.1& GBMFit_imputed$pred$n.minobsinnode == 10], add = T, col = "red")
# GLM
plot.roc(GLMFit$pred$obs, GLMFit$pred$X0, col = "blue", main="ROC curves for GLM")
plot.roc(glmFit$pred$obs, glmFit$pred$X0, add =T, col = "green")
plot.roc(GLMFit_imputed$pred$obs, GLMFit_imputed$pred$X0, add =T, col = "red")
#Nueral Network
plot.roc(nnetFit$pred$obs[nnetFit$pred$size==6&nnetFit$pred$decay==0.5], nnetFit$pred$X0[nnetFit$pred$size==6&nnetFit$pred$decay==0.5], col = "blue", main = "ROC curve for Neural Network")
plot.roc(nnetFit_imputed$pred$obs[nnetFit_imputed$pred$size==6&nnetFit_imputed$pred$decay==0.5], nnetFit_imputed$pred$X0[nnetFit_imputed$pred$size==6&nnetFit_imputed$pred$decay==0.5], add = T, col = "red")



## INTER comarison of models
Resamples_imputed = resamples(list(KNN=KNNFit_imputed, RF= RFFit_imputed,SVM= SVM_imputed,GBM = GBMFit_imputed, GLM =GLMFit_imputed, NNet= nnetFit_imputed))
summary(Resamples_imputed)
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(Resamples_imputed, layout = c(3, 1))
DifValues <- diff(Resamples_imputed)
summary(DifValues)
parallelplot(Resamples_imputed)
trellis.par.set(caretTheme())
dotplot(Resamples_imputed, metric = "ROC")

## Confusion Matrix of imputed Models
# predictions
temp_KNNImputed = KNNFit_imputed$pred[KNNFit_imputed$pred$k==125,]
predictions_KNNImputed = temp_KNNImputed$pred[order(temp_KNNImputed$rowIndex)]

temp_RFImputed = RFFit_imputed$pred[RFFit_imputed$pred$mtry==2,]
predictions_RFImputed = temp_RFImputed$pred[order(temp_RFImputed$rowIndex)]

temp_SVMImputed = SVM_imputed$pred[SVM_imputed$pred$C==0.05,]
prediction_SVMImputed = temp_SVMImputed$pred[order(temp_SVMImputed$rowIndex)]

temp_GBMImputed = GBMFit_imputed$pred[which(GBMFit_imputed$pred$interaction.depth==6 & GBMFit_imputed$pred$n.trees==450 & GBMFit_imputed$pred$shrinkage==0.1, GBMFit_imputed$pred$n.minobsinnode == 10),]
predictions_GBMImputed = temp_GBMImputed$pred[order(temp_GBMImputed$rowIndex)]

predictions_GLMImputed = GLMFit_imputed$pred$pred[order(GLMFit_imputed$pred$rowIndex)]

temp_NNetImputed = nnetFit_imputed$pred[nnetFit_imputed$pred$size==6&nnetFit_imputed$pred$decay==0.5,]
prediction_NNetImputed = temp_NNetImputed$pred[order(temp_NNetImputed$rowIndex)]

# confusion matrices
cm_GLM = confusionMatrix(predictions_GLMImputed,Adult_dummy_imputed[,"income"], positive = "X0")
cm_KNN = confusionMatrix(predictions_KNNImputed,Adult_dummy_imputed[,"income"], positive = "X0")
cm_GBM = confusionMatrix(predictions_GBMImputed,Adult_dummy_imputed[,"income"], positive = "X0")
cm_SVM = confusionMatrix(prediction_SVMImputed, Adult_dummy_imputed[,"income"], positive = "X0" )
cm_RF = confusionMatrix(predictions_RFImputed, Adult_dummy_imputed[,"income"], positive = "X0")
cm_NNet = confusionMatrix(prediction_NNetImputed,Adult_dummy_imputed[, "income"], positive = "X0" )
#Table for comparison
col1=c(cm_KNN$byClass["Balanced Accuracy"],cm_RF$byClass["Balanced Accuracy"], cm_SVM$byClass["Balanced Accuracy"] , cm_GBM$byClass["Balanced Accuracy"], cm_GLM$byClass["Balanced Accuracy"], cm_NNet$byClass["Balanced Accuracy"])
col2=c(cm_KNN$byClass["F1"],cm_RF$byClass["F1"], cm_SVM$byClass["F1"], cm_GBM$byClass["F1"], cm_GLM$byClass["F1"], cm_NNet$byClass["F1"])
col3=c(cm_KNN$overall["Accuracy"],cm_RF$overall["Accuracy"] , cm_SVM$overall["Accuracy"], cm_GBM$overall["Accuracy"], cm_GLM$overall["Accuracy"], cm_NNet$overall["Accuracy"])
Table = data.frame(BalancedAccuracy = col1, Fmeasure = col2, Accuracy = col3)
rownames(Table) = c("kNN", "Random Forests", "Support Vector Machine","Gradient Boost","Generalized Linear Model", "Neural Network")
Table
# stability of Models
# we are checking the stability of GLM since it takes not much time.
# 2- fold
set.seed(400)
ctrl2 = trainControl(method = "cv", 
                    number = 2, 
                    sampling = "smote",
                    summaryFunction= twoClassSummary, 
                    classProbs=T,
                    savePredictions = T)
GLMFit_2Fold = train(Adult_dummy_del[,results$optVariables], Adult_dummy_del[,"income"], 
               method = "glm", trControl = ctrl2, metric = "ROC", family="binomial")
GBMFit_2Fold = train(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"],
            method = "gbm",trControl = ctrl2, metric = "ROC", tuneGrid = expand.grid(n.trees = 450, interaction.depth =
                                                                                                                                                                             6, shrinkage = 0.1, n.minobsinnode = 10))
# 3-fold
ctrl3 = trainControl(method = "cv", 
                     number = 3, 
                     sampling = "smote",
                     summaryFunction= twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
GLMFit_3Fold = train(Adult_dummy_del[,results$optVariables], Adult_dummy_del[,"income"], 
                     method = "glm", trControl = ctrl3, metric = "ROC", family="binomial")
GBMFit_3Fold = train(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"], method = "gbm",trControl = ctrl3, metric = "ROC", tuneGrid = expand.grid(n.trees = 450, interaction.depth =
                                                                                                                                                                             6, shrinkage = 0.1, n.minobsinnode = 10))
# 4-fold
ctrl4 = trainControl(method = "cv", 
                     number = 4, 
                     sampling = "smote",
                     summaryFunction= twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
GLMFit_4Fold = train(Adult_dummy_del[,results$optVariables], Adult_dummy_del[,"income"], 
                     method = "glm", trControl = ctrl4, metric = "ROC", family="binomial")
GBMFit_4Fold = train(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"], method = "gbm",trControl = ctrl4, metric = "ROC", tuneGrid = expand.grid(n.trees = 450, interaction.depth =6, shrinkage = 0.1, n.minobsinnode = 10))
                                                                                                                                                                            
# 6-fold
ctrl6 = trainControl(method = "cv", 
                     number = 6, 
                     sampling = "smote",
                     summaryFunction= twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
GLMFit_6Fold = train(Adult_dummy_del[,results$optVariables], Adult_dummy_del[,"income"], 
                     method = "glm", trControl = ctrl6, metric = "ROC", family="binomial")
GBMFit_6Fold = train(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"], method = "gbm",trControl = ctrl6, metric = "ROC", tuneGrid = expand.grid(n.trees = 450, interaction.depth =6, shrinkage = 0.1, n.minobsinnode = 10))
# 7-fold
ctrl7 = trainControl(method = "cv", 
                     number = 7, 
                     sampling = "smote",
                     summaryFunction= twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)
GBMFit_7Fold = train(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"], method = "gbm",trControl = ctrl7, metric = "ROC", tuneGrid = expand.grid(n.trees = 450, interaction.depth =6, shrinkage = 0.1, n.minobsinnode = 10))

# Ensembling using Stacking

algorithmList <- c('knn', 'rf', 'gbm', 'glm', 'nnet')
models = caretList(Adult_dummy_imputed[,results$optVariables], Adult_dummy_imputed[,"income"],
         trControl=ctrl, methodList=algorithmList,tuneList = list(
           knn=caretModelSpec(method="knn", tuneGrid=data.frame(k=125)),
           rf=caretModelSpec(method="rf", tuneGrid=data.frame(.mtry=2)),
    gbm=caretModelSpec(method="gbm", tuneGrid=data.frame(n.trees = 450, interaction.depth = 6, shrinkage = 0.1, n.minobsinnode = 10)),
    nnet = caretModelSpec(method = "nnet", tuneGrid = data.frame(size = 6, decay = 0.5))),
    metric = "ROC")
# Support vector machine is highly correlated with RF and NNet. so removing SVM
modelCor(resamples(models))
submodels = models[c("knn","rf" , "gbm","nnet", "glm")]
class(submodels) = "caretList"
stack_rf <- caretStack(submodels, method="rf", metric="ROC", trControl=ctrl)
temp_stack_rf = stack_rf$ens_model$pred[stack_rf$ens_model$pred$mtry==2,]
predictions_stack_rf = temp_stack_rf$pred[order(temp_stack_rf$rowIndex)]
cm_stack_rf = confusionMatrix(predictions_stack_rf, Adult_dummy_imputed[,"income"])

stack_rf_1 = caretstack(submodels[c("knn", "nnet")], method = "rf", metric = "ROC", trControl = ctrl)