#!/usr/bin/env python
import numpy as np, os, os.path, sys, warnings
from tqdm import tqdm
from sklearn.metrics import roc_auc_score, precision_recall_curve, auc, confusion_matrix

def calculate_net_benefit(y_true, y_pred, threshold):
    tp = np.sum((y_true == 1) & (y_pred >= threshold))
    fp = np.sum((y_true == 0) & (y_pred >= threshold))
    n = len(y_true)
    
    # Calculate Net Benefit
    net_benefit = (tp / n) - ((threshold / (1 - threshold)) * (fp / n))
    return net_benefit

# Function to calculate ECE (Estimated Calibration Error)
def calculate_ece(probs, labels, n_bins=10):
    bin_edges = np.linspace(0, 1, n_bins + 1)
    ece = 0
    for i in range(n_bins):
        bin_mask = (probs > bin_edges[i]) & (probs <= bin_edges[i + 1])
        bin_size = np.sum(bin_mask)
        if bin_size > 0:
            bin_acc = np.mean(labels[bin_mask])
            bin_conf = np.mean(probs[bin_mask])
            ece += bin_size * np.abs(bin_acc - bin_conf) / len(probs)
    return ece

def compute_confusion_matrix(labels, predictions):
    labels = np.array(labels).astype(int)
    predictions = np.array(predictions).astype(int)
    cm = np.zeros((2, 2))
    for i in range(len(labels)):
        cm[labels[i]][predictions[i]] += 1
    return cm[0, 0], cm[0, 1], cm[1, 0], cm[1, 1]

def compute_accuracy(tn, fp, fn, tp):
    return (tp + tn) / (tn + fp + fn + tp)

def compute_f1(tn, fp, fn, tp):
    denominator = 2 * tp + fp + fn
    return 2 * tp / denominator if denominator != 0 else 0.0

def evaluate_model(labels, prediction_probability, threshold, dataset_label, inference_speed):
    prediction_labels = prediction_probability >= threshold

    # Compute confusion matrix and metrics
    tn, fp, fn, tp = compute_confusion_matrix(labels, prediction_labels)
    accuracy = compute_accuracy(tn, fp, fn, tp)
    F1 = compute_f1(tn, fp, fn, tp)

    # Additional metrics
    auc_score = roc_auc_score(labels, prediction_probability)
    precision, recall, _ = precision_recall_curve(labels, prediction_probability)
    auprc = auc(recall, precision)
    net_benefit = calculate_net_benefit(labels, prediction_probability, threshold)
    ece = calculate_ece(prediction_probability, labels)
    sensitivity = tp / (tp + fn) if (tp + fn) > 0 else np.nan
    specificity = tn / (tn + fp) if (tn + fp) > 0 else np.nan

    # Simplified composite score
    composite = 0.6477*F1+0.3447*auprc+0.8514*net_benefit-0.8675*ece-0.05*inference_speed    

    # Scores 
    return {
        'AUC': round(auc_score,3),
        'AUPRC': round(auprc,3),
        'Net Benefit': round(net_benefit,3),
        'ECE': round(ece,4),
        'tp': round(tp,0),
        'fp': round(fp,0),
        'fn': round(fn,0),
        'tn': round(tn,0),
        'F1': round(F1,3),
        'Sensitivity': round(sensitivity,3),
        'Specificity': round(specificity,3),
        'Inference Speed': round(inference_speed,3),
        'Weighted Score': round(composite,2)
    }

