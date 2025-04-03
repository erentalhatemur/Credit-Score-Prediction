# **Credit Score Prediction Project**

## **Project Description**
This project aims to predict individuals' credit scores using **Ordinal Logistic Regression**. The model analyzes the impact of various financial factors on credit scores and classifies them into categories: Good, Standard, and Poor.

## **Dataset**
Dataset used: `Credit_Score_Clean.csv`
- The dataset contains financial history information of individuals.
- Dependent variable: **Credit Score** (Good, Standard, Poor)
- Independent variables:
  - **Age**
  - **Num_Bank_Accounts** (Number of Bank Accounts)
  - **Num_Credit_Card** (Number of Credit Cards)
  - **Interest_Rate** (Interest Rate)
  - **Num_of_Loan** (Number of Loans)
  - **Num_of_Delayed_Payment** (Number of Delayed Payments)
  - **Changed_Credit_Limit** (Change in Credit Limit)
  - **Num_Credit_Inquiries** (Number of Credit Inquiries)
  - **Total_EMI_per_month** (Total Monthly EMI Payment)
  - **Amount_invested_monthly** (Amount Invested Monthly)

## **Methods and Analysis**
### **1. Data Preprocessing:**
- Outliers were removed using **IQR (Interquartile Range)** and **Z-score** methods.
- Missing data analysis was performed.
- Data types were converted into appropriate formats.

### **2. Modeling:**
- An initial **Ordinal Logistic Regression** model was built with all independent variables.
- Insignificant variables were removed for model improvement.
- **Stepwise Regression** was used to identify the optimal model.
- A weighted modeling approach was applied to handle class imbalance.

### **3. Model Evaluation:**
- Model performance was measured using **AIC (Akaike Information Criterion)** and **BIC (Bayesian Information Criterion)**.
- **Pseudo R-squared** was calculated to assess model fit.
- **Confusion Matrix** was used to evaluate prediction accuracy.
- **Brant Test** was performed to check if ordinal logistic regression assumptions hold.
- **VIF (Variance Inflation Factor)** was used to check for multicollinearity.

## **Setup and Usage**
### **Required Libraries**
Ensure the following R libraries are installed before running the project:
```r
install.packages(c("MASS", "VGAM", "pscl", "caret", "brant", "car"))
```

### **Execution Steps**
1. Place the `Credit_Score_Clean.csv` file in the project directory.
2. Open the `Credit_Score_Predict.R` script in R Studio or an R terminal.
3. Execute the script sequentially to train the model and make predictions.

### **Outputs**
- The final model's performance will be evaluated using a **Confusion Matrix**.
- The predicted credit scores will be compared with actual scores for accuracy assessment.
- The results will provide insights for further model improvements.

## **Future Improvements**
- Exploring **Machine Learning techniques** (Random Forest, XGBoost) for better accuracy.
- **Feature Engineering** to create new predictive variables.
- Testing the model on different datasets to evaluate its generalizability.

---

