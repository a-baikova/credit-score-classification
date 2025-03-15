# 🏛️ Credit Score Classification 

## Project Overview  
This academic project uses this [Kaggle dataset](https://www.kaggle.com/datasets/parisrohan/credit-score-classification)  of 50,000 records to create a model for a global financial institution capable of classifying customers into different credit scores (Poor, Standard, Good). The model replaces manual risk assessment with automated solutions while prioritizing sensitivity in high-risk ("Poor") classifications.

## Methodology  
1. **Data Understanding**:  
   - Analyzed 28 variables including financial behavior, debt patterns, and payment history  
2. **Data Preparation**:  
   - Outlier treatment (e.g., age range 14-56 validation)  
   - Ethical feature selection (removed SSN/Name)  
   - Imputation: Forward-Backward Fill + Average Fill
   -  **Split Strategies**:  
       1. **Random Sets** (66.6% train / 33.3% test)  
       2. **Stratified Sets** (preserved class distribution)  
       3. **Random with undersampling** (balanced Poor/Standard/Good ratios)
3. **Modeling**:  
-    - Tested 4 approaches with 3 data split strategies:  
     - Logistic Regression  
     - Random Forest 
     - Neural Networks (MLP architectures)  
     - KNN 

## Tools & Libraries  
- R  
- RStudio  
- Libraries: `caret`, `randomForest`, `nnet`, `rpart`, `ggplot2`, `corrplot`

## Results  
This project tested 116 different model configurations across three sampling strategies to identify the optimal approach for automating risk assessment. After extensive experimentation, the Random Forest with cross-validation consistently delivered superior performance, achieving 85% test accuracy with random sampling, 78% with stratified sampling, and 67% with downsampling.

![image](https://github.com/user-attachments/assets/af909e12-65c8-48e2-9536-fe898ccc5bb1)
