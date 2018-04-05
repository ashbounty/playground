# This script performs a linear regression on the bikesharing dataset from 
# https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset

# Import packages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import style
import os
import zipfile
import urllib.request
from sklearn.linear_model import LinearRegression
from sklearn import preprocessing, model_selection

# Set working directory
if not os.path.exists('./input/'):
    os.makedirs('input')
os.chdir('./input')

# Initalization of variables for data import
url = 'https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip'
fname = 'bikesharing.zip'

# Download and unzip the data set
urllib.request.urlretrieve(url, fname)
zref = zipfile.ZipFile(fname, 'r')
zref.extractall()
zref.close()

# Import and clean the dataset
df = pd.read_csv('day.csv')
clean_up = {'season' : {1 : 'Spring', 2 : 'Summer', 3 : 'Fall', 4 : 'Winter'},
            'yr' : {0 : 2011, 1 : 2012},
            'weekday' : {0 : 'Monday', 1 : 'Tuesday', 2 : 'Wednesday', 3 : 'Thursday', 4 : 'Friday', 5 : 'Saturday',
                         6 : 'Sunday'},
            'mnth' : {1 : 'January', 2 : 'February', 3 : 'March', 4 : 'April', 5 : 'May', 6 : 'June', 7 : 'July',
                      8 : 'August', 9 : 'September', 10 : 'October', 11 : 'November', 12 : 'December'},
            'weathersit' : {1 : 'Very good', 2 : 'Good', 3 : 'Bad', 4 : 'Very Bad'}}
df.replace(clean_up, inplace=True)
for col in ['season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit']:
    df[col] = df[col].astype('category')

# Create dummy variables for categorical features
df = pd.get_dummies(df, columns=['season', 'mnth', 'weekday', 'weathersit'])

# Create the finale feature and target arrays
X = df.drop(['instant', 'dteday', 'registered', 'casual', 'cnt'], axis=1)
colnames = list(X)
X = preprocessing.scale(X)
y = df['cnt']

# Split the dataset into trainings and test data
X_train, X_test, y_train, y_test = model_selection.train_test_split(X, y, test_size=0.2)

# Train the linear model
lm = LinearRegression()
lm.fit(X_train, y_train)

# Output coefficients
print('{0:25}{1}'.format('Intercept', lm.intercept_))
for f, c in zip(colnames, lm.coef_):
    print('{0:25}{1}'.format(f, c))
print('\n')

# Output validation
print('{0:25}{1}'.format('Training data score:', lm.score(X_train, y_train)))
print('{0:25}{1}'.format('Test data score :', lm.score(X_test, y_test)))

# Prepare plots for predictions on trainings and test set
predictions_train = lm.predict(X_train)
predictions_test = lm.predict(X_test)
style.use('ggplot')
f, (ax1, ax2) = plt.subplots(1, 2, figsize=(8,4), sharex=True, sharey=True)

# Plot predicted against actual values on trainings set
ax1.scatter(y_train, predictions_train)
ax1.plot(ax1.get_xlim(), ax1.get_xlim(), c='black', linestyle='--', linewidth=0.5)
ax1.set_title('Trainings data')
ax1.set_xlabel('Actual')
ax1.set_ylabel('Predicted')

# Plot predicted against actual values on test set
ax2.scatter(y_test, predictions_test)
ax2.plot(ax2.get_xlim(), ax2.get_xlim(), c='black', linestyle='--', linewidth=0.5)
ax2.set_title('Test data')
ax2.set_ylabel('Predicted')
ax2.set_xlabel('Actual')
f.tight_layout()

# Save figure
plt.savefig('Plot.png')
