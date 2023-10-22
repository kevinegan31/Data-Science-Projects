import numpy as np
import pandas as pd
import os

# Set working directory
os.chdir('/Users/kevinegan/Documents/GitHub.nosync/Data-Science-Projects/Framingham_Heart_Study/Data/')
# Read in the dataset
framingham = pd.read_csv('framingham.csv')
framingham.head()
'''
Objectives:

Are patients with diabetes more likely to develop heart disease at an early age?
Is there a certain demographic group that is at higher risk of heart disease than others?
Does frequent exercise lower the risk of developing heart disease?
Are smokers more likely to develop heart disease than non-smokers?
'''

# Figures
import matplotlib.pyplot as plt
import seaborn as sns

# Checking education levels
sns.countplot(x='education',data=framingham)
# Changing the x and y-axis labels using matplotlib functions
plt.xlabel('Education Level')
plt.ylabel('Count')
# To show the plot if running in a script
plt.show()

# Let's observe education with cigs per day
sns.barplot(x='education', y='cigsPerDay',data=framingham)
plt.xlabel('Education Level')
plt.ylabel('Average Cigarettes per Day')
plt.show()

# Let's breakdown smoking status and education
sns.countplot(x='education',hue='currentSmoker',data=framingham)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.xlabel('Education Level')
plt.show()

# Education compared to BMI broken down between smoking status
sns.boxplot(x='education', y='BMI', hue='currentSmoker', data=framingham,
            hue_order=[0, 1])
plt.xlabel('Education Level')
plt.ylabel('BMI')
leg = plt.legend(title='Smoking Status')
leg.get_texts()[0].set_text('Non-Smoker')
leg.get_texts()[1].set_text('Smoker')
plt.show()


# Checking CHD vs the number of cigarettes smoked per day
sns.catplot(x='TenYearCHD',y='cigsPerDay',kind='bar',data=framingham)
plt.xlabel('TenYearCHD')
plt.ylabel('Cigarettes per day')
plt.show()

# Is there a relationship between CHD, age, and current smokers?
# Smokers tend to have a higher risk of CHD at an earlier age.
sns.boxplot(x='TenYearCHD',y='age',hue='currentSmoker',data=framingham)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.xlabel('TenYearCHD')
plt.ylabel('Age')
plt.show()

# Relationship between age, prevalent stroke, and ten year CHD
sns.boxplot(x='TenYearCHD',y='age',hue='prevalentStroke',data=framingham)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.xlabel('TenYearCHD')
plt.ylabel('Age')
plt.show()

# Relationship between age, diabetes, and ten year CHD
sns.boxplot(x='TenYearCHD',y='age',hue='diabetes',data=framingham)
plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)
plt.xlabel('TenYearCHD')
plt.ylabel('Age')
plt.show()

# Cholesterol levels and ten year CHD
sns.boxplot(x='TenYearCHD',y='totChol',data=framingham)
plt.ylim(80)
plt.xlabel('TenYearCHD')
plt.ylabel('Total Cholesterol')
plt.show()

# Systolic and diastolic blood pressure and ten year CHD
sns.catplot(x='TenYearCHD',y='sysBP',kind='bar',data=framingham)
plt.xlabel('TenYearCHD')
plt.ylabel('Systolic Blood Pressure')
plt.show()

sns.catplot(x='TenYearCHD',y='diaBP',kind='bar',data=framingham)
plt.xlabel('TenYearCHD')
plt.ylabel('Diastolic Blood Pressure')
plt.show()

# BMI and ten year CHD
sns.catplot(x='TenYearCHD',y='BMI',kind='bar',data=framingham)
plt.xlabel('TenYearCHD')
plt.ylabel('BMI')
plt.show()

# BP Meds and ten year CHD
sns.catplot(x='TenYearCHD',y='BPMeds',kind='bar',data=framingham)
plt.xlabel('TenYearCHD')
plt.ylabel('BP Meds')
plt.show()


# Model building
# Checking for null values
framingham.isnull().any()

sns.heatmap(framingham.isnull(), cbar=False)

missing_values = framingham.isnull().sum()
print(missing_values)

# Identify columns with missing values
cols_with_missing = framingham.columns[framingham.isnull().any()]

# Plot histograms for each of these columns
for col in cols_with_missing:
    plt.figure(figsize=(10, 6))
    sns.histplot(framingham[col], kde=True)
    plt.title(f'Distribution of {col}')
    plt.xlabel(col)
    plt.ylabel('Count')
    plt.show()

# Education
missing_proportion = framingham['education'].isnull().sum() / len(framingham)
print(f"Proportion of missing values in 'education': {missing_proportion:.2%}")

sns.countplot(x='education', hue='currentSmoker', data=framingham)
plt.show()


mode_value = framingham['education'].mode()[0]
framingham['education'].fillna(mode_value, inplace=True)

print(framingham['education'].isnull().sum())  # Should print 0 if all missing values are filled
plt.figure(figsize=(10, 6))
sns.histplot(framingham['education'], kde=True)
plt.title(f'Distribution of Education')
plt.xlabel('Education')
plt.ylabel('Count')
plt.show()


# cigsPerDay
# Filter the data
smokers_data = framingham[framingham['currentSmoker'] == 1]

# Plot the histogram
sns.histplot(smokers_data['cigsPerDay'], bins=30, kde=True)
plt.xlabel('Cigarettes per Day')
plt.ylabel('Frequency')
plt.title('Distribution of Cigarettes per Day for Current Smokers')
plt.show()

# For currentSmokers = 0, fill NA with 0
framingham.loc[framingham['currentSmoker'] == 0, 'cigsPerDay'] = framingham.loc[framingham['currentSmoker'] == 0, 'cigsPerDay'].fillna(0)

# For currentSmokers = 1, fill NA with the mean
mean_cigs = framingham.loc[framingham['currentSmoker'] == 1, 'cigsPerDay'].mean()
framingham.loc[framingham['currentSmoker'] == 1, 'cigsPerDay'] = framingham.loc[framingham['currentSmoker'] == 1, 'cigsPerDay'].fillna(mean_cigs)

# Plot the histogram
sns.histplot(smokers_data['cigsPerDay'], bins=30, kde=True)
plt.xlabel('Cigarettes per Day')
plt.ylabel('Frequency')
plt.title('Distribution of Cigarettes per Day for Current Smokers')
plt.show()

# Check if any missing values remain
print(framingham['cigsPerDay'].isnull().sum())  # Should print 0 if all missing values are filled

# BPMeds
sns.countplot(x='BPMeds', hue='education', data=framingham)
plt.show()

sns.countplot(x='BPMeds', hue='currentSmoker', data=framingham)
plt.show()

framingham['age'].min()
framingham['age'].max()
framingham_copy = framingham.copy()
bins = [30, 40, 50, 60, 70, 80]
labels = ['30-39', '40-49', '50-59', '60-69', '70-79']
framingham_copy['age_group'] = pd.cut(framingham_copy['age'], bins=bins, labels=labels, right=False)
sns.countplot(x='age_group', hue='BPMeds', data=framingham_copy)
plt.xlabel('Age Group')
plt.ylabel('Count')
plt.legend(title='BPMeds', labels=['Not on BP Meds', 'On BP Meds'])
plt.show()

mode_val = framingham['BPMeds'].mode()[0]
framingham['BPMeds'].fillna(mode_val, inplace=True)

# Check if any missing values remain
print(framingham['BPMeds'].isnull().sum())  # Should print 0 if all missing values are filled

# totChol
from scipy.stats import ttest_ind

# Get totChol values for smokers and non-smokers, excluding NaN values
smoker_totChol = framingham[framingham['currentSmoker'] == 1]['totChol'].dropna()
non_smoker_totChol = framingham[framingham['currentSmoker'] == 0]['totChol'].dropna()

# Perform t-test
t_stat, p_value = ttest_ind(smoker_totChol, non_smoker_totChol)

print(f"T-statistic: {t_stat}")
print(f"P-value: {p_value}")

# Check the significance
alpha = 0.05  # commonly used significance level
if p_value < alpha:
    print("The difference in total cholesterol levels between smokers and non-smokers is statistically significant.")
else:
    print("The difference in total cholesterol levels between smokers and non-smokers is not statistically significant.")

# Calculate mean total cholesterol for smokers and non-smokers
mean_totChol_smokers = framingham[framingham['currentSmoker'] == 1]['totChol'].mean()
mean_totChol_non_smokers = framingham[framingham['currentSmoker'] == 0]['totChol'].mean()

# Fill missing values for smokers
framingham.loc[(framingham['currentSmoker'] == 1) & (framingham['totChol'].isnull()), 'totChol'] = mean_totChol_smokers

# Fill missing values for non-smokers
framingham.loc[(framingham['currentSmoker'] == 0) & (framingham['totChol'].isnull()), 'totChol'] = mean_totChol_non_smokers

# Check if any missing values remain
print(framingham['totChol'].isnull().sum())  # Should print 0 if all missing values are filled

# BMI
# Get BMI values for smokers and non-smokers, excluding NaN values
smoker_bmi = framingham[framingham['currentSmoker'] == 1]['BMI'].dropna()
non_smoker_bmi = framingham[framingham['currentSmoker'] == 0]['BMI'].dropna()

# Perform t-test
t_stat, p_value = ttest_ind(smoker_bmi, non_smoker_bmi)

print(f"T-statistic: {t_stat}")
print(f"P-value: {p_value}")

# Check the significance
alpha = 0.05  # commonly used significance level
if p_value < alpha:
    print("The difference in BMI between smokers and non-smokers is statistically significant.")
else:
    print("The difference in BMI between smokers and non-smokers is not statistically significant.")

# Calculate mean BMI for smokers and non-smokers
mean_BMI_smokers = framingham[framingham['currentSmoker'] == 1]['BMI'].mean()
mean_BMI_non_smokers = framingham[framingham['currentSmoker'] == 0]['BMI'].mean()

# Fill missing values for smokers
framingham.loc[(framingham['currentSmoker'] == 1) & (framingham['BMI'].isnull()), 'BMI'] = mean_BMI_smokers

# Fill missing values for non-smokers
framingham.loc[(framingham['currentSmoker'] == 0) & (framingham['BMI'].isnull()), 'BMI'] = mean_BMI_non_smokers

# Check if any missing values remain
print(framingham['BMI'].isnull().sum())  # Should print 0 if all missing values are filled

# Glucose
# Get glucose values for smokers and non-smokers, excluding NaN values
smoker_glucose = framingham[framingham['currentSmoker'] == 1]['glucose'].dropna()
non_smoker_glucose = framingham[framingham['currentSmoker'] == 0]['glucose'].dropna()

# Perform t-test
t_stat, p_value = ttest_ind(smoker_glucose, non_smoker_glucose)

print(f"T-statistic: {t_stat}")
print(f"P-value: {p_value}")

# Check the significance
alpha = 0.05  # commonly used significance level
if p_value < alpha:
    print("The difference in glucose between smokers and non-smokers is statistically significant.")
else:
    print("The difference in glucose between smokers and non-smokers is not statistically significant.")

# Calculate mean glucose for smokers and non-smokers
mean_glucose_smokers = framingham[framingham['currentSmoker'] == 1]['glucose'].mean()
mean_glucose_non_smokers = framingham[framingham['currentSmoker'] == 0]['glucose'].mean()

# Fill missing values for smokers
framingham.loc[(framingham['currentSmoker'] == 1) & (framingham['glucose'].isnull()), 'glucose'] = mean_glucose_smokers

# Fill missing values for non-smokers
framingham.loc[(framingham['currentSmoker'] == 0) & (framingham['glucose'].isnull()), 'glucose'] = mean_glucose_non_smokers

# Check if any missing values remain
print(framingham['glucose'].isnull().sum())  # Should print 0 if all missing values are filled

# heartRate
# only one missing value, fill with mean
framingham['heartRate'].fillna(framingham['heartRate'].mean(), inplace=True)

# Dropping null values
#framingham = framingham.dropna()
framingham.isnull().sum()
print(framingham.shape)

framingham['TenYearCHD'].value_counts()

'''
A baseline model is a model that always predicts the majority class all the time.
'''
# Baseline accuracy:
# 3101/(3101+557)
# 0.8477310005467469

# Required libraries


from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from sklearn.model_selection import cross_val_score
from imblearn.over_sampling import RandomOverSampler
from imblearn.under_sampling import RandomUnderSampler
from imblearn.pipeline import Pipeline
X = framingham.drop('TenYearCHD',axis=1)
y = framingham['TenYearCHD']
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.35)

oversample = RandomOverSampler(sampling_strategy='minority')
X_over, y_over = oversample.fit_resample(X, y)
X_train, X_test, y_train, y_test = train_test_split(X_over,y_over,test_size=0.35)
steps_dt = [('under', RandomUnderSampler()), ('model', DecisionTreeClassifier())]
steps_rf = [('under', RandomUnderSampler()), ('model', RandomForestClassifier())]
steps_xgb = [('under', RandomUnderSampler()), ('model', XGBClassifier())]
pipeline_dt = Pipeline(steps=steps_dt)
pipeline_rf = Pipeline(steps=steps_rf)
pipeline_xgb = Pipeline(steps=steps_xgb)

# pipeline_dt.fit(X_train,y_train)

# Define your models
models = {
    'Decision Tree': pipeline_dt,
    'Random Forest': pipeline_rf,
    'XGBoost': pipeline_xgb
}

# Perform cross-validation for each model
for model_name, model in models.items():
    # Perform 5-fold cross-validation (you can adjust the number of folds)
    scores = cross_val_score(model, X_train, y_train, cv=5, scoring='accuracy')
    
    # Print the accuracy scores for each fold
    print(f"{model_name} Cross-Validation Accuracy Scores: {scores}")
    
    # Calculate and print the mean accuracy and standard deviation
    mean_accuracy = scores.mean()
    std_accuracy = scores.std()
    print(f"{model_name} Mean Accuracy: {mean_accuracy:.4f} (std: {std_accuracy:.4f})")

# Random Forest performs the best
# Fit the pipeline on your training data
pipeline_rf.fit(X_train, y_train)

# Make predictions on the test set
pipepred = pipeline_rf.predict(X_test)

from sklearn.metrics import classification_report,accuracy_score,precision_score,recall_score,f1_score
print(classification_report(y_test,pipepred))

# Macro average
precision_macro = precision_score(y_test, pipepred, average='macro')
recall_macro = recall_score(y_test, pipepred, average='macro')
f1_macro = f1_score(y_test, pipepred, average='macro')

# Weighted average
precision_weighted = precision_score(y_test, pipepred, average='weighted')
recall_weighted = recall_score(y_test, pipepred, average='weighted')
f1_weighted = f1_score(y_test, pipepred, average='weighted')
