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

# Dropping null values
framingham = framingham.dropna()

framingham.shape

framingham['TenYearCHD'].value_counts()

'''
A baseline model is a model that always predicts the majority class all the time.
'''
# Baseline accuracy:
# 3101/(3101+557)
# 0.8477310005467469


from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from imblearn.over_sampling import RandomOverSampler
from imblearn.under_sampling import RandomUnderSampler
from imblearn.pipeline import Pipeline

X = framingham.drop('TenYearCHD',axis=1)
y = framingham['TenYearCHD']
X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.35)

oversample = RandomOverSampler(sampling_strategy='minority')
X_over, y_over = oversample.fit_resample(X, y)
X_train, X_test, y_train, y_test = train_test_split(X_over,y_over,test_size=0.35)
steps = [('under', RandomUnderSampler()), ('model', DecisionTreeClassifier())]
pipeline = Pipeline(steps=steps)

pipeline.fit(X_train,y_train)

pipepred = pipeline.predict(X_test)

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



