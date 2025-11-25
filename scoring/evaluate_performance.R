library('pROC')
library('MLmetrics')

calculate_net_benefit<-function(y_true, y_pred, threshold){
  tp <- sum((y_true == 1) & (y_pred >= threshold))
  fp <- sum((y_true == 0) & (y_pred >= threshold))
  n <- length(y_true)
  # Calculate Net Benefit
  net_benefit <- (tp / n) - ((threshold / (1 - threshold)) * (fp / n))
  return (net_benefit)
}

calculate_ece<-function(probs, labels, n_bins=10){
  bin_edges <- seq(0, 1, 1/(n_bins + 1))
  ece <- 0
  for (i in seq(1,n_bins)){
    bin_mask <- (probs > bin_edges[i]) & (probs <= bin_edges[i + 1])
    bin_size <- sum(bin_mask)
    if (bin_size > 0){
      bin_acc <- mean(labels[bin_mask])
      bin_conf <- mean(probs[bin_mask])
      ece <- ece + bin_size * abs(bin_acc - bin_conf) / length(probs)
    }
  }
  return(ece)
}

evaluate_model<-function(labels, prediction_probability, threshold,dataset_label,inference_speed){
  # Compute confusion matrix and metrics
  prediction_labels<-as.numeric(prediction_probability>=threshold)
  tab<-ConfusionMatrix(prediction_labels,labels)
  tn<-tab[1,1]
  fp<-tab[1,2]
  fn<-tab[2,1]
  tp<-tab[2,2]
  accuracy = Accuracy(prediction_labels,labels)
  F1 <- F1_Score(labels, prediction_labels)
  
  # Additional metrics
  auc_score <- roc(labels, prediction_probability)$auc
  auprc_score <- PRAUC(prediction_probability,labels)
  net_benefit <- calculate_net_benefit(labels, prediction_probability, threshold)
  ece <- calculate_ece(prediction_probability,labels)
  sensitivity <- Sensitivity(labels,prediction_labels)
  specificity <- Specificity(labels,prediction_labels)
  composite<-0.6477*F1+0.3447*auprc_score+0.8514*net_benefit-0.8675*ece-0.05*inference_speed
  return(data.frame(dataset=dataset_label,AUC=round(auc_score,3),AUPRC=round(auprc_score,3),NetBenefit=round(net_benefit,3),ECE=round(ece,4),
                    TP=tp,FP=fp,FN=fn,TN=tn,F1=round(F1,3),
                    Sensitivity=round(sensitivity,3),Specificity=round(specificity,3),inference_speed=round(inference_speed,3),weighted_score=round(composite,2)))
}