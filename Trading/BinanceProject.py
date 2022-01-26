# -*- coding: utf-8 -*-
"""
Created on Fri Dec 31 12:10:05 2021

@author: Kacie Gilbertson
"""
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import tensorflow as tf
import xgboost as xgb
from tensorflow import keras
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report, log_loss
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, PredefinedSplit
from BinanceDataConcat import * 
from Models import *
from DataFuncs import *
from testfuncs import *
from ensemblefuncs import *
import datetime as dt
from sklearn.linear_model import LogisticRegression

## TF stuff

tf.config.get_visible_devices(device_type=None)
tf.config.set_visible_devices([], 'GPU') # Disable gpu for simple models (not many weights to calculate)

### Code Area

BTC = 'BTCUSDT_2021-12-25.csv'
ETH = 'ETHUSDT_2021-12-26.csv'

first_date = '2017-12-01 00:00:00' #Where we want to start analysising the data - where we think the relevant data starts, also needs to be when all relevant coins have avaliable data
last_date = '2021-12-24 23:59:00' # Where we want to end analysis of the data - typically a common end point in all coin data

# Featues we want to use in further analysis
analysis_features = ['Open', 'High', 'Low', 'Close', 'Volume', 'No. Trades','Taker Buy Base Vol', 'Quote_Asset_Vol', 'hour', 'year']

BTC_df, BTC_data_use = FullLoad(BTC, analysis_features, first_date, last_date)
ETH_df, ETH_data_use = FullLoad(ETH, analysis_features, first_date, last_date) 

### Analysis on one minute base variables

BTC_df.describe()
ETH_df.describe()

### Grouped data into pools of 5 and doing some analysis on these variables

# feature_list = ['change', 'spread', 'up_spread', 'down_spread', 'vol_change','num_trade_change','vol_per_trade','volatility','maker_ratio', 'up_spread_c', 'down_spread_c', 'hour', 'year'] 

feature_list = ['change', 'up_spread', 'down_spread', 'vol_change','num_trade_change','vol_per_trade','maker_ratio', 'up_spread_c', 'down_spread_c'] 

BTC_five = GroupFunction(data = BTC_data_use, feature_list = feature_list, t = 10, window = 8, shift_con = False, shift_t = 5, complete = True, corr = False, no_zero = False)
ETH_five = GroupFunction(data = ETH_data_use, feature_list = feature_list, t = 10, window = 8, shift_con = False, shift_t = 5, complete = True, corr = False, no_zero = False)

BTC_five.columns # Column names for reference in analysis

# ML dataset

Lagged = TimeLagData(data = BTC_five, features = feature_list) 

Lagged.plot.scatter(x = 'change', y = 'change_1')
##### TrainTest Split Testing

# For ML

X_Train_ML, Y_Train_ML, X_Test_ML, Y_Test_ML, X_Valid_ML, Y_Valid_ML, Y_Valid_Actual_ML, Y_Train_Actual_ML, Y_Test_Actual_ML = TrainTest(Lagged, t = 5, split = 0.85, n_steps = 10, t_lag_1= 1, t_lag_2 = 1, shift_con = False, shift_t = 5, feature_list = feature_list, corr = False, no_zero = False, grouped = True, ML = True)

# For AI

X_Train, Y_Train, X_Test, Y_Test, X_Valid, Y_Valid, Y_Valid_Actual, Y_Train_Actual, Y_Test_Actual = TrainTest(BTC_five, t = 5, split = 0.85, n_steps = 10, t_lag_1= 1, t_lag_2 = 1, shift_con = False, shift_t = 5, feature_list = feature_list, corr = False, no_zero = False, grouped = True, ML = False)


# Plotting the splits - the fact that this doesn't work, does it indicate that there are missing values in both data sets? 

SplitPlot(BTC_data_use, feature_list, split = 0.9, t=60)

#### Setting up ML #####

### XGBoost

param_set = {'eta': [0.03], 'max_depth': [4], 'gamma': [1], 'subsample': [0.5], 'objective': ['binary:logistic'], 'n_estimators' : [350,400], 'n_jobs' : [-1]}

best_score, best_estimator = XGBoostTune(X_Train_ML, X_Valid_ML, Y_Train_ML, Y_Valid_ML, param_set)

param = {'eta': 0.03, 'max_depth': 4, 'gamma': 1, 'subsample': 0.5, 'objective': 'binary:logistic', 'n_estimators' : 350, 'n_jobs' : -1}

pred_vec, test_acc, train_acc, test_loss, train_loss, model = XGBoostModel(X_Train_ML, X_Valid_ML, Y_Train_ML, Y_Valid_ML, param, Lagged = Lagged, feature_list = feature_list)

### Random forest

pred_vec, test_acc, train_acc, test_loss, train_loss, model = RandomForestModel(X_Train_ML, X_Valid_ML, Y_Train_ML, Y_Valid_ML, Lagged = Lagged, feature_list = feature_list, n_estimators = 500)

###### AI #######

### MLP (Use ML data here)

pred_vec, test_acc, model = MLPModel(X_Train_ML, Y_Train_ML, X_Valid_ML, Y_Valid_ML, units_1 = 50, units_2 = 40, units_3 = 30, layers = 3, dropout = 0.25, activation = 'selu', epochs = 10)

### RNN

pred_vec, test_acc, model = RNNModel(X_Train, Y_Train, X_Valid, Y_Valid, units_1 = 50, units_2 = 50, layers = 1, dropout = 0.2, epochs = 10)

### LSTM

pred_vec, test_acc, model = LSTMModel(X_Train, Y_Train, X_Valid, Y_Valid, units_1 = 50, units_2 = 50, layers = 2, dropout = 0.2, epochs = 10)

### CNNLSTM
pred_vec, test_acc, y_proba, y_std, model = CNNLSTM(X_Train, Y_Train, X_Valid, Y_Valid, dropout = 0.1, epochs = 5)

## CNN

pred_vec, test_acc, model = CNNModel(X_Train, Y_Train, X_Test, Y_Test, dropout = 0.1, epochs = 5)


### Creating the Tests
    
Comp_Ret, account, trades = SimFunction(pred_vec[:,1], Y_Valid_Actual_ML, comm = 0.00075, plot = True)

# Test Function - Are the None defaults good?

t = TestFunction(X_Train_ML, Y_Train_ML, X_Valid_ML, Y_Valid_ML, Y_Valid_Actual_ML, model_name = 'XGB', model_type = 'ML', feature_list = feature_list, Lagged = Lagged, param = param)
t = TestFunction(X_Train, Y_Train, X_Valid, Y_Valid, Y_Valid_Actual, epochs = 5, n_exp = 3)


# Need to train models on train set, make predictions on validation. Then we need to train on train + validation and make predictions on test.


# If stacking is false then it only needs train, valid, but if stacking is true it needs train, valid and test
  
model_list = ['LSTM', 'RNN', 'MLP', 'XGB', 'RF']

acc_list_valid, acc_list_test, soft_df_valid, soft_df_test = Ensemble(X_Train, Y_Train, X_Valid, Y_Valid, Y_Valid_Actual, X_Train_ML, Y_Train_ML, X_Valid_ML, Y_Valid_ML, Y_Valid_Actual_ML, model_list, stacking = True, X_Test = X_Test, Y_Test = Y_Test, X_Test_ML = X_Test_ML, Y_Test_ML = Y_Test_ML, Y_Test_Actual = Y_Test_Actual, Y_Test_Actual_ML = Y_Test_Actual_ML, repeats = 2, epochs = 5, feature_list = feature_list, Lagged = Lagged, param = param)




