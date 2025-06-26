# 📊 Data Analysis Projects in R

This repository showcases a variety of statistical and machine learning techniques implemented in R. Each project involves real-world or academic datasets, and highlights my ability to perform exploratory data analysis, dimensionality reduction, regression modeling, multivariate statistics, and classification. The code includes reproducible scripts and interpretable outputs that demonstrate a solid understanding of both statistical theory and applied data science.

---

## 🔍 Clustering and Dimensionality Reduction

**File:** `Clustering-and-Dimensionality-Reduction-R.pdf`  
**Techniques Used:**  
- Nonlinear Dimensionality Reduction: Multidimensional Scaling (MDS), Kernel PCA (polydot, laplace, RBF kernels)  
- Clustering: K-Means, Elbow Method, Visual Analysis  
- Data Visualization: Correlation plots, pairwise scatterplots  

**Functionality:**  
- Identified that PCA was inappropriate due to non-elliptical data distribution  
- Applied and compared nonlinear dimensionality reduction methods  
- Determined optimal number of clusters using WSS and visual interpretation  
- Compared cluster separability using reduced feature spaces  

---

## 📈 Regression Modelling on Retail Sales

**File:** `regression-modelling.pdf`  
**Dataset:** Carseats (ISLR2)  
**Techniques Used:**  
- Linear Regression (first-order and second-order models)  
- Model Selection via Residual Diagnostics and Adjusted R²  
- Train/Test Split and Model Evaluation (RMSE, NMSE)  
- Interaction Terms and Polynomial Features  

**Functionality:**  
- Built a linear model to predict sales from multiple features  
- Evaluated residuals to assess model fit and assumptions  
- Compared first- and second-order models to capture nonlinear trends  
- Achieved high model performance with meaningful business insights  

---

## 🧪 Multivariate Normal and Conditional Expectation

**File:** `conditional_expectation_mvn.pdf`  
**Focus:** Mathematical Derivations in Multivariate Statistics  
**Topics Covered:**  
- Properties of Multivariate Normal (MVN) distributions  
- Conditional Expectation and Variance  
- Matrix Algebra in Statistical Inference  
- Vector-Valued Linear Regression under MVN  

**Functionality:**  
- Derived expressions for E[Y|X], Var(Y|X), and regression coefficients  
- Compared regression of Y on X vs. X on Y to illustrate asymmetry  
- Provided interpretations in terms of covariance structures and independence  

---

## 📦 Insurance Cost Modeling and EDA

**File:** `insurance_cost_modelling.pdf`  
**Dataset:** `insurance.csv`  
**Techniques Used:**  
- Exploratory Data Analysis (EDA): Histograms, Boxplots, Skim Summary  
- Correlation Analysis and Visualization  
- Linear Regression (with interaction terms)  
- Feature Impact Interpretation  

**Functionality:**  
- Identified key cost drivers: smoking status, age, BMI  
- Created group-level summaries by sex, region, and smoker status  
- Built and interpreted multiple linear regression models  
- Highlighted how interactions like `bmi × smoker` amplify insurance costs  

---

## 🧬 Cancer Type Classification with the Khan Dataset

**File:** `khan-dataset-classifier-comparison.pdf`  
**Dataset:** Khan Gene Expression Dataset (ISLR2)  
**Techniques Used:**  
- High-Dimensional Classification:  
  - Linear Discriminant Analysis (LDA)  
  - Regularized Discriminant Analysis (RDA)  
  - Naive Bayes  
- Model Evaluation (Accuracy on Test Data)  
- Covariance Analysis using Frobenius Norm  

**Functionality:**  
- Handled 2,308-dimensional gene expression data with only 63 training samples  
- Justified LDA due to covariance matrix instability in high dimensions  
- Achieved 75% accuracy with LDA vs. 25% with RDA and Naive Bayes  
- Explained limitations of complex models under data scarcity  

---

## 📚 Conditional Expectation and Classification with Multivariate Analysis

**File:** `multivariate_detailed_classification.pdf`  
**Topics Covered:**  
- Conditional Expectation under MVN  
- Weighted Estimation (MSE and MAE loss minimization)  
- Multivariate Classification: Box’s M Test, LDA, QDA, Logistic Regression  
- ROC Analysis and Confusion Matrices  

**Functionality:**  
- Derived and interpreted conditional expectations using matrix notation  
- Used ROC curves and accuracy metrics to evaluate classifiers  
- Compared multivariate classification models under different assumptions  
- Visualized results using `ggplot2`, `patchwork`, and `pROC` libraries  

---

## 🧠 Skills Demonstrated

- R Programming  
- Data Cleaning & Wrangling  
- Linear and Nonlinear Modeling  
- Dimensionality Reduction  
- Classification and Discriminant Analysis  
- Exploratory and Inferential Statistics  
- Matrix Algebra in Statistics  
- Visualization with `ggplot2`, `corrplot`, `patchwork`, `pROC`

---

## 📁 How to Use This Repo

Each project is contained as a standalone file (PDF or RMarkdown) with code, outputs, and commentary.  
They reflect a variety of scenarios encountered in applied statistics and data science roles.

---

## 👤 Author

**İlke Kaş**  
PhD Researcher | Data Analyst | Machine Learning Enthusiast  
[LinkedIn](https://www.linkedin.com/in/ilkekas/) · [GitHub](https://github.com/ilke-kas)

