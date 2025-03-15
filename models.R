### REGRESSAO LOGISTICA

# library(corrplot)
# library(nnet)
# library(caret)
# library(MASS)
# library(ROCR)
# 
# treino_reduzido <-data_frame(treino_1)
# teste_reduzido <- data_frame(teste_1)
# 
# combined_data <- rbind(treino_reduzido, teste_reduzido)
# 
# modelo_RegLog<-multinom(Credit_Score ~ ., data = treino_reduzido)
# 
# modelo_RegLog_prev<-predict(modelo_RegLog,teste_reduzido)
# 
# confusion_matrix <- confusionMatrix(modelo_RegLog_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_RegLog, treino_reduzido, type = "class")
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# coefficients <- summary(modelo_RegLog)$coefficients
# standard_errors <- summary(modelo_RegLog)$standard.errors
# z_values <- coefficients / standard_errors
# p_values <- 2 * (1 - pnorm(abs(z_values)))
# p_values
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#    actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_RegLog, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
# 
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# 
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# modelo_Reg_menos <-multinom(Credit_Score ~ Age + Num_Bank_Accounts + Num_Credit_Card + Interest_Rate + Delay_from_due_date+
#                                               Num_of_Delayed_Payment + Changed_Credit_Limit + Num_Credit_Inquiries + Outstanding_Debt + 
#                                               Credit_History_Age + Payment_of_Min_Amount_Yes + Monthly_Balance, data = treino_reduzido)
# 
# modelo_Reg_menos_prev<-predict(modelo_Reg_menos,teste_reduzido)
# 
# 
# confusion_matrix <- confusionMatrix(modelo_Reg_menos_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# 
# treino_predicted<- predict(modelo_Reg_menos, treino_reduzido, type = "class")
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_Reg_menos, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# 
# mean(aucs) 
# aucs[1]
# aucs[2]
# aucs[3] 
# 
# 
# modelo_polr <- polr(Credit_Score ~ ., data=treino_reduzido)
# 
# 
# modelo_polr_perv<-predict(modelo_polr,teste_reduzido)
# 
# 
# confusion_matrix <- confusionMatrix(modelo_polr_perv, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_polr, treino_reduzido, type = "class")
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_polr, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# 
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=1)
# 
# modelo_cv <- caret::train(Credit_Score ~ ., 
#                              data = treino_reduzido, 
#                              method = "multinom",
#                              trControl= TrainingParameters)
# 
# modelo_cv_perv<-predict(modelo_cv,teste_reduzido)
# 
# confusion_matrix <- confusionMatrix(modelo_cv_perv, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# 
# treino_predicted<- predict(modelo_cv, treino_reduzido)
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_cv, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)
# 
# modelo_cv_10 <- caret::train(Credit_Score ~ ., 
#                           data = treino_reduzido, 
#                           method = "multinom",
#                           trControl= TrainingParameters
#                           )
# 
# modelo_cv_10_perv<-predict(modelo_cv_10,teste_reduzido)
# 
# 
# confusion_matrix <- confusionMatrix(modelo_cv_10_perv, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_cv_10, treino_reduzido)
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_cv_10, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# 
# modelo_stepAIC <- stepAIC(
#   polr(Credit_Score ~ ., data = treino_reduzido, method = "logistic"),
#   direction = "both"
# )
# 
# modelo_stepAIC_prev<-predict(modelo_stepAIC,teste_reduzido,type="class")
# 
# confusion_matrix <- confusionMatrix(modelo_stepAIC_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_stepAIC, treino_reduzido)
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_stepAIC, teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# modelo_stepAIC_tr <- stepAIC(
#   modelo_RegLog,
#   direction = "both"
# )
# 
# 
# modelo_stepAIC_tr_prev<-predict(modelo_stepAIC_tr,teste_reduzido,type="class")
# 
# confusion_matrix <- confusionMatrix(modelo_stepAIC_tr_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# 
# treino_predicted<- predict(modelo_stepAIC_tr,treino_reduzido)
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_stepAIC_tr , teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# grid <- expand.grid(
#   decay = c(0.0001, 0.001, 0.01, 0.1, 0.5, 0.6, 0.7, 1.0)
# )
# 
# modelo_grid <- train(Credit_Score ~ ., data = treino_reduzido, 
#                     method = "multinom", 
#                     trControl = trainControl(method = "cv", number = 10), 
#                     tuneGrid = grid,
#                     tuneLength=5)
# 
# modelo_grid_prev<-predict(modelo_grid,teste_reduzido)
# 
# confusion_matrix <- confusionMatrix(modelo_grid_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_grid, treino_reduzido)
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_grid , teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# modelo_threshold <- predict(modelo_RegLog, teste_reduzido, type = "prob")
# 
# # thresholds <- c(Poor = 0.3, Standard = 0.5, Good = 0.2) #Aleatório & Estratificado
# # thresholds <- c(Poor = 0.5, Standard = 0.3, Good = 0.2) #Downsampling
# 
# modelo_threshold_ajust <- unlist(apply(modelo_threshold, 1, function(prob) {
#   if (prob["Poor"] > thresholds["Poor"]) {
#     return("Poor")
#   } else if (prob["Standard"] > thresholds["Standard"]) {
#     return("Standard")
#   } else if (prob["Good"] > thresholds["Good"]) {
#     return("Good")
#   }
# }))
# modelo_threshold_ajust <- factor(modelo_threshold_ajust, levels = c("Poor", "Standard", "Good"))
# 
# 
# confusionMatrix(as.factor(modelo_threshold_ajust), teste_reduzido$Credit_Score)
# 
# 
# treino_predicted<- predict(modelo_RegLog, treino_reduzido, type = "prob")
# 
# modelo_threshold_ajust_treino <- unlist(apply(treino_predicted, 1, function(prob) {
#   if (prob["Poor"] > thresholds["Poor"]) {
#     return("Poor")
#   } else if (prob["Standard"] > thresholds["Standard"]) {
#     return("Standard")
#   } else if (prob["Good"] > thresholds["Good"]) {
#     return("Good")
#   }
# }))
# modelo_threshold_ajust_treino <- factor(modelo_threshold_ajust_treino, levels = c("Poor", "Standard", "Good")) 
# 
# confusionMatrix(as.factor(modelo_threshold_ajust_treino), treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   score = modelo_threshold[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# treino_scaled <- scale(treino_reduzido[-16])
# treino_scaled <- cbind(treino_scaled, treino_reduzido[16])
# teste_scaled <- scale(teste_reduzido[-16])
# teste_scaled <- cbind(teste_scaled,teste_reduzido[16])
# 
# modelo_scaled<-multinom(Credit_Score ~ ., data = treino_scaled)
# 
# modelo_scaled_prev<-predict(modelo_scaled,teste_scaled)
# 
# confusion_matrix <- confusionMatrix(modelo_RegLog_prev, teste_scaled$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_scaled, treino_scaled, type = "class")
# confusionMatrix(treino_predicted, treino_scaled$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_scaled$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_scaled , teste_scaled, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# modelo_menos_scaled <-multinom(Credit_Score ~ Age + Num_Bank_Accounts + Num_Credit_Card + Interest_Rate + Delay_from_due_date+
#                               Num_of_Delayed_Payment + Changed_Credit_Limit + Num_Credit_Inquiries + Outstanding_Debt + 
#                               Credit_History_Age + Payment_of_Min_Amount_Yes + Monthly_Balance, data = treino_scaled)
# 
# modelo_menos_scaled_prev<-predict(modelo_menos_scaled,teste_scaled)
# 
# confusion_matrix <- confusionMatrix(modelo_menos_scaled_prev, teste_scaled$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_menos_scaled, treino_scaled, type = "class")
# confusionMatrix(treino_predicted, treino_scaled$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_scaled$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_menos_scaled , teste_scaled, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=1)
# 
# modelo_menos_cv <- caret::train(Credit_Score ~ Age + Num_Bank_Accounts + Num_Credit_Card + Interest_Rate + Delay_from_due_date+
#                                   Num_of_Delayed_Payment + Changed_Credit_Limit + Num_Credit_Inquiries + Outstanding_Debt + 
#                                   Credit_History_Age + Payment_of_Min_Amount_Yes + Monthly_Balance, 
#                           data = treino_reduzido, 
#                           method = "multinom",
#                           trControl= TrainingParameters)
# 
# 
# modelo_menos_cv_prev<-predict(modelo_menos_cv,teste_reduzido)
# 
# confusion_matrix <- confusionMatrix(modelo_menos_cv_prev, teste_reduzido$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_menos_cv, treino_reduzido, type = "class")
# confusionMatrix(treino_predicted, treino_reduzido$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_reduzido$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_menos_cv , teste_reduzido, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=1)
# 
# modelo_menos_cv_scaled <- caret::train(Credit_Score ~ Age + Num_Bank_Accounts + Num_Credit_Card + Interest_Rate + Delay_from_due_date+
#                                   Num_of_Delayed_Payment + Changed_Credit_Limit + Num_Credit_Inquiries + Outstanding_Debt + 
#                                   Credit_History_Age + Payment_of_Min_Amount_Yes + Monthly_Balance, 
#                                 data = treino_scaled, 
#                                 method = "multinom",
#                                 trControl= TrainingParameters)
# 
# 
# modelo_menos_cv_scaled_prev<-predict(modelo_menos_cv_scaled,teste_scaled)
# 
# confusion_matrix <- confusionMatrix(modelo_menos_cv_scaled_prev, teste_scaled$Credit_Score)
# confusion_matrix
# 
# treino_predicted<- predict(modelo_menos_cv_scaled, treino_scaled, type = "class")
# confusionMatrix(treino_predicted, treino_scaled$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_scaled$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_menos_cv_scaled , teste_scaled, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]
# 
# 
# 
# normalização <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }
# 
# treino_normalizado <- as.data.frame(lapply(treino_reduzido[, 1:15], normalização))
# treino_normalizado <- cbind(treino_normalizado, treino_reduzido[16])
# teste_normalizado <- as.data.frame(lapply(teste_reduzido[, 1:15], normalização))
# teste_normalizado <- cbind(teste_normalizado, teste_reduzido[16])
# 
# modelo_normalizado<-multinom(Credit_Score ~ ., data = treino_normalizado)
# 
# modelo_normalizado_prev<-predict(modelo_normalizado,teste_normalizado[-16])
# 
# confusion_matrix <- confusionMatrix(modelo_normalizado_prev, teste_normalizado$Credit_Score)
# confusion_matrix
# 
# 
# treino_predicted<- predict(modelo_normalizado, treino_normalizado, type = "class")
# confusionMatrix(treino_predicted, treino_normalizado$Credit_Score)
# 
# lvls = levels(combined_data$Credit_Score)
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# for (type.id in 1:3) {
#   actual.class = teste_normalizado$Credit_Score == lvls[type.id]
#   
#   modelo_prob = predict(modelo_normalizado , teste_normalizado, type = "prob")
#   score = modelo_prob[, type.id]
#   
#   pred = prediction(score, actual.class)
#   perf = performance(pred, "tpr", "fpr")
#   
#   roc.x = unlist(perf@x.values)
#   roc.y = unlist(perf@y.values)
#   lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
#   
#   rgauc = performance(pred, "auc")
#   rgauc = unlist(slot(rgauc, "y.values"))
#   aucs[type.id] = rgauc
# }
# 
# lines(x=c(0,1), c(0,1))
# mean(aucs)
# aucs[1]
# aucs[2]
# aucs[3]


##########################################################################################################################################
######### ARVORE DE DECISAO

##CART STANDARD

# treino_1$Credit_Score <- factor(treino_1$Credit_Score, levels = c("Poor", "Standard", "Good"),ordered = TRUE)
# teste_1$Credit_Score <- factor(teste_1$Credit_Score, levels = c("Poor", "Standard", "Good"),ordered = TRUE)

# glimpse(treino_1)
# set.seed(100)

# library(rpart)
#
# Criação do Modelo de Árvore Decisão, a partir do conjunto treino (train_1), recorrendo ao Algoritmo CART
# model_tree<-rpart(formula = Credit_Score ~.,data=treino_1,method="class",control=rpart.control(xval = 10))
# summary(model_tree)
#
# xval= número de validações cruzadas
#
# Métricas para o model_tree (dados de treino)
# model_tree_prev_train<-predict(model_tree,newdat=treino_1,type="class")
# Criação da Matriz de Confusão
# confusionMatrix(model_tree_prev_train,treino_1$Credit_Score)
# model_tree_previsao<-predict(model_tree,newdat=teste_1,type="class")
# Criação da Matriz de Confusão
# confusionMatrix(model_tree_previsao,teste_1$Credit_Score)

#### PURNED 

# model_tree$cptable
# plotcp(model_tree)
# model_tree_new<-prune(model_tree,cp=0.01483661)

# Métricas para o model_tree (dados de treino)
# model_prune_prev_train<-predict(model_tree_new,newdat=treino_1,type="class")
# Criação da Matriz de Confusão
# confusionMatrix(model_prune_prev_train,treino_1$Credit_Score)

# model_tree_new_previsao<-predict(model_tree_new,newdat=teste_1,type="class")

# Criação da Matriz de Confusão 
# 
# confusionMatrix(model_tree_new_previsao,teste_1$Credit_Score)

#### 3) CV

# library(rpart)

# Configuración de control para la validación cruzada
# cv.control <- trainControl(method = "cv", number = 10)
# 
# # Crear el modelo usando validación cruzada
# model_cv <- train(
#   Credit_Score ~ .,  # Variable de respuesta
#   data = treino_1,  # Conjunto de entrenamiento
#   method = "rpart",  # Algoritmo CART
#   tuneLength = 5,  # Número de combinaciones de hiperparámetros a probar
#   trControl = cv.control  # Validación cruzada
# )

# model_cv
# Teste ao Modelo criado (model_cv) com base nos dados não vistos do conjunto teste (teste_1)
# Cálculo das previsões recorrendo ao modelo criado (model_cv)
# model_cv_previsao<-predict(model_cv,teste_1)

# Criação da Matriz de Confusão
# confusionMatrix(model_cv_previsao,teste_1$Credit_Score)

# VALIDACAO OVERFITTING

# model_cv_previsao_treino<-predict(model_cv,treino_1)
# Criação da Matriz de Confusão
# confusionMatrix(model_cv_previsao_treino,treino_1$Credit_Score)
# Output da melhor Árvore de Decisão
# best_tree<-model_cv$finalModel

####### BAGGING CV TREEBAG

# set.seed(100)
# 
# install.packages("MLmetrics")
# library(MLmetrics)
# 
# # Configuración de validación cruzada
# cv.control_tb <- trainControl(
#   method = "repeatedcv",      # Validación cruzada repetida
#   number = 10,               # Número de folds
#   # repeats = 3,               # Número de repeticiones
#   savePredictions = "final", # Guardar predicciones
#   classProbs = TRUE,         # Calcular probabilidades
#   summaryFunction = multiClassSummary  # Función resumen para clasificación múltiple
# )
# 
# # Entrenamiento del modelo con el método treebag (Bagging con árboles de decisión)
# model_bag2 <- train(
#   Credit_Score ~ .,           # Formula: predecir Credit_Score basado en el resto de variables
#   data = treino_1,            # Dataset de entrenamiento
#   method = "treebag",         # Método de bagging con árboles
#   tuneLength = 3,             # Número de combinaciones de hiperparámetros a probar
#   nbagg = 3,                # Número de árboles en el bagging
#   metric = "Accuracy",        # Métrica de evaluación (puedes usar "ROC" o "Accuracy")
#   trControl = cv.control_tb
# )
# 
# # Ver resumen del modelo
# model_bag2
# 
# model_bag2_previsao<-predict(model_bag2,teste_1)
# confusionMatrix(model_bag2_previsao,teste_1$Credit_Score)
# 
# # Representação gráfica das classes previstas (no: cliente não quebra contrato; yes: cliente quebra contrato)
# plot(teste_1$Credit_Score,model_bag2_previsao,main="Classificação prevista com a Árvore Bagging - Hipótese 2: Previstos vs Reais",xlab="Reais",ylab="Previstos")
# 
# #OVERFITTING
# 
# model_bag2_previsao_treino<-predict(model_bag2,treino_1)
# # Criação da Matriz de Confusão e Cálculo das Métricas mais usuais
# confusionMatrix(model_bag2_previsao_treino,treino_1$Credit_Score)

############ cv - random forest: ranger

treino_1$Credit_Score <- factor(treino_1$Credit_Score, 
                                levels = c("Poor", "Standard", "Good"), 
)

teste_1$Credit_Score <- factor(teste_1$Credit_Score, 
                               levels = c("Poor", "Standard", "Good")
)

set.seed(100)

cv.controlfa<-trainControl(
  method="cv",
  number=10,
  savePredictions="final",
  classProbs=TRUE,
  summaryFunction=multiClassSummary
)

# Ajustamento dos hiperparâmetros
tune_forest<-expand.grid(
  mtry =c(1,2),
  splitrule="gini",
  min.node.size=c(100,110,120)
)

model_forest_tune<-train(
  Credit_Score ~.,
  data=treino_1,
  method="ranger",
  metric="Accuracy",
  num.trees=800,
  tuneGrid=tune_forest,
  importance="impurity",
  tuneLength=5,
  trControl=cv.controlfa
)

model_forest_tune

model_forest_tune_previsao<-predict(model_forest_tune,teste_1)
# Representação gráfica das classes previstas (no: cliente não quebra contrato; yes: cliente quebra contrato)
plot(teste_1$Credit_Score,model_forest_tune_previsao,main="Classificação prevista com a Árvore Random Forest: Previstos vs Reais",xlab="Reais",ylab="Previstos")
# Criação da Matriz de Confusão e Cálculo das Métricas mais usuais
confusionMatrix(model_forest_tune_previsao,teste_1$Credit_Score)
#Importância das variáveis
plot(varImp(model_forest_tune),main="Importância das Variáveis com Florestas Aleatórias com Ajustamento")

#OVERFITTING TEST

model_forest_tune_previsao_2<-predict(model_forest_tune,treino_1)
# Representação gráfica das classes previstas (no: cliente não quebra contrato; yes: cliente quebra contrato)
confusionMatrix(model_forest_tune_previsao_2,treino_1$Credit_Score)


###############BOOSTING

# library(gbm)
# set.seed(100)
# 
# cv.controlbo<-trainControl(
#   method="cv",
#   number=10,
#   savePredictions="final",
#   classProbs=TRUE,
#   summaryFunction=multiClassSummary
# )
# 
# tune_gbm<-expand.grid(
#   interaction.depth = c(1,2),
#   n.trees =c(50,100,150),
#   shrinkage = c(0.01,0.03,0.05),
#   n.minobsinnode = c(10,15,20)
# )
# 
# 
# model_boosting_tune<-train(
#   Credit_Score ~.,
#   data=treino_1,
#   method="gbm",
#   metric="Accuracy",
#   tuneGrid=tune_gbm,
#   tuneLength=5,
#   trControl=cv.controlbo
# )
# 
# model_boosting_tune
# 
# model_boosting_tune_previsao_treino<-predict(model_boosting_tune,treino_1)
# model_boosting_tune_previsao<-predict(model_boosting_tune,teste_1)
# # Representação gráfica das classes previstas (no: cliente não quebra contrato; yes: cliente quebra contrato)
# plot(teste_1$Credit_Score,model_boosting_tune_previsao,main="Classificação prevista com a Árvore boosting: Previstos vs Reais",xlab="Reais",ylab="Previstos")
# 
# # Overfitting
# 
# confusionMatrix(model_boosting_tune_previsao,teste_1$Credit_Score)
# confusionMatrix(model_boosting_tune_previsao_treino,treino_1$Credit_Score)
# 
# library(caret)
# library(gbm)
# 
# plot(varImp(model_boosting_tune), main = "Importancia de Variables con GBM")


########## NOT TESTED YET C5
# set.seed(100)

# Train a model with above parameters. We will use C5.0 algorithm
# DecTreeModel <- train(Credit_Score ~ ., data = treino_1, 
#                       method = "C5.0",
#                       preProcess=c("scale","center"),
#                       trControl= TrainingParameters,
#                       na.action = na.omit
# )

# Predictions on test set
# DTPredictions <-predict(DecTreeModel, teste_1, na.action = na.pass)
# 
# # Print confusion matrix and results
# cmTree <-confusionMatrix(DTPredictions, teste_1$Credit_Score)
# print(cmTree)
# 
# #OVERFITTING
# 
# #Predictions on test set
# DTPredictions_treino <-predict(DecTreeModel, treino_1, na.action = na.pass)
# 
# # Print confusion matrix and results
# cmTree_treino <-confusionMatrix(DTPrediction_treinos, treino_1$Credit_Score)
# print(cmTree_treino)
# 
# # ### check variable importance for this model
# importance <- varImp(DecTreeModel, scale=FALSE)
# plot(importance)

######## KNN
# 
# install.packages("class")
# install.packages("caret")
# install.packages("e1071")
# 
# 
# 
# treino1_subset <- treino_1[c('Age','Monthly_Inhand_Salary','Num_Bank_Accounts','Num_Credit_Card','Interest_Rate','Num_Credit_Inquiries','Num_of_Loan','Changed_Credit_Limit','Num_of_Delayed_Payment','Delay_from_due_date','Monthly_Balance','Amount_invested_monthly','Credit_History_Age','Outstanding_Debt','Credit_Score')]
# 
# # Shuffle the dataset
# set.seed(123)
# treino1_subset <- treino1_subset[sample(1:nrow(treino1_subset)), ]
# treino1_subset$Credit_Score <- factor(treino1_subset$Credit_Score,
#                                       levels = c('Poor','Standard','Good'),
#                                       ordered = TRUE)
# 
# # Split the data into training and testing sets (80-20 split)
# trainIndex <- 1:round(0.8 * nrow(treino1_subset))
# trainData <- treino1_subset[trainIndex, ]
# testData <- treino1_subset[-trainIndex, ]
# 
# # Normalize numeric features
# normalize <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }
# 
# trainData[, 1:14] <- as.data.frame(lapply(trainData[, 1:14], normalize))
# testData[, 1:14] <- as.data.frame(lapply(testData[, 1:14], normalize))
# 
# 
# library(class)
# 
# # Specify the target variable
# trainLabels <- trainData$Credit_Score
# testLabels <- testData$Credit_Score
# 
# # Train the KNN model (k = 5 in this example)
# k <- 5
# knnModel <- knn(train = trainData[, 1:14], 
#                 test = testData[, 1:14], 
#                 cl = trainLabels, 
#                 k = k)
# 
# 
# 
# summary(knnModel)
# 
# confusionMatrix <- table(Predicted = knnModel, Actual = testLabels)
# print(confusionMatrix)
# 
# accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
# cat("Accuracy:", accuracy)
# 
# library(caret)
# 
# # Convert predictions to factors if needed
# knnModel <- factor(knnModel, levels = levels(testLabels))
# 
# # Calculate evaluation metrics
# confMatrix <- confusionMatrix(knnModel, testLabels)
# print(confMatrix)


################### REDES NEURONAIS
############### ANN

# library(ggplot2)
# library(lattice)
# library(caret)
# library(C50)
# library(kernlab)
# library(mlbench)
# library(randomForest)
# library(caretEnsemble)
# library(MASS)
# library(klaR)
# library(nnet)
# library(magrittr) # needs to be run every time you start R and want to use %>%
# library(dplyr) 
# library(arules)
# library(tidyverse)
# library(rsample)
# library(lattice)
# library(klaR)
# library(ggplot2)
# library(recipes)
# #library(keras)
# library(e1071)
# 
# treino_1$Credit_Score <- factor(treino_1$Credit_Score, 
#                                 levels = c("Poor", "Standard", "Good"))
# 
# teste_1$Credit_Score <- factor(teste_1$Credit_Score, 
#                                levels = c("Poor", "Standard", "Good"))
# 
# # Parámetros para control del entrenamiento
# TrainingParameters <- trainControl(
#   method = "cv",        # Validación cruzada
#   number = 10,         # Número de folds 
#   repeats = 3,
#   classProbs = TRUE,    # Calcular probabilidades
#   summaryFunction = multiClassSummary # Métricas para clasificación múltiple
# )
# 
# NNModel <- train(
#   Credit_Score ~ .,        # Fórmula: predice Credit_Score basado en todas las demás variables
#   data = treino_1,         # Datos de entrenamiento
#   method = "nnet",         # Red neuronal con nnet
#   trControl = TrainingParameters,
#   preProcess = c("center", "scale"), # Normalización de los datos
#   tuneGrid = expand.grid(
#     size = c(5, 10, 15),    # Número de neuronas en la capa oculta
#     decay = c(0.01, 0.1)    # Regularización
#   ),
#   na.action = na.omit,
#   maxit = 200              # Aumenta el número de iteraciones para mayor convergencia
# )
# 
# # Resumen del modelo
# print(NNModel)
# 
# NNPredictions <-predict(NNModel, teste_1)
# # Create confusion matrix
# cmNN <-confusionMatrix(NNPredictions, teste_1$Credit_Score)
# 
# NNPredictions_treino <-predict(NNModel, treino_1)
# # Create confusion matrix
# cmNN <-confusionMatrix(NNPredictions_treino, treino_1$Credit_Score)
# 
# print(cmNN)

# cat("Acurácia no Conjunto de Treino:", round(train_accuracy * 100, 2), "%\n")
# cat("Acurácia no Conjunto de Teste:", round(test_accuracy * 100, 2), "%\n")


############ MLP()

# set.seed(100)
# 
# # Normalizacao dos dados (min-max scaling)
# normalize <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }
# 
# treino_norm <- as.data.frame(lapply(treino_1[,-which(names(treino_1) == "Credit_Score")], normalize))
# teste_norm <- as.data.frame(lapply(teste_1[,-which(names(teste_1) == "Credit_Score")], normalize))
# 
# # Codificacao variavel resposta como numerica
# treino_targets <- as.numeric(factor(treino_1$Credit_Score, levels = c("Poor", "Standard", "Good"))) - 1
# teste_targets <- as.numeric(factor(teste_1$Credit_Score, levels = c("Poor", "Standard", "Good"))) - 1
# 
# library(RSNNS)
# 
# # Criar modelo MLP
# mlp_model <- mlp(
#   x = as.matrix(treino_norm),            # variaveis predictoras normalizadas
#   y = decodeClassLabels(treino_targets), # codificacao one-hot das variáveis
#   size = c(20, 10),                       # neuronios
#   maxit = 500,                           # Num iteracoes iteraciones
#   learnFuncParams = c(0.01, 0.1),         # apredizagem
#   linOut = FALSE                         # multiclass >> categoricas
# )
# 
# # Estimacao sobre o teste_1
# predictions <- predict(mlp_model, as.matrix(teste_norm))
# 
# # Convertir clases e probabilidades
# predicted_classes <- max.col(predictions) - 1  # Clases empiezan desde 0
# 
# # Evaluar o modelo
# library(caret)
# confusionMatrix(factor(predicted_classes, levels = c(0, 1, 2)),  factor(teste_targets, levels = c(0, 1, 2)))
# 
# ### sensitivity e specificity
# (sensitivity<-matriz_confusao[1,1]/sum(matriz_confusao[1,]))
# (specifity<-matriz_confusao[2,2]/sum(matriz_confusao[2,]))
# 
# 
# # OVERFITTING
# 
# # Predicoes sobre o treino_1
# predictions_treino <- predict(mlp_model, as.matrix(treino_norm))
# 
# # Convertir probabilidades e clases
# predicted_classes_treino <- max.col(predictions_treino) - 1  # Clases empiezan desde 0
# 
# # Validacao
# # confusionMatrix(factor(predicted_classes_treino, levels = c(0, 1, 2)),  factor(treino_targets, levels = c(0, 1, 2)))
# 
# 
# matriz_confusao_teste <- confusionMatrix(factor(predicted_classes, levels = c(0, 1, 2)), 
#                                          factor(teste_targets, levels = c(0, 1, 2)))
# matriz_confusao_treino <- confusionMatrix(factor(predicted_classes_treino, levels = c(0, 1, 2)), 
#                                           factor(treino_targets, levels = c(0, 1, 2)))
# (accuracy_teste<-sum(diag(matriz_confusao_teste))/sum(matriz_confusao_teste))
# (accuracy_treino<-sum(diag(matriz_confusao_treino))/sum(matriz_confusao_treino))
