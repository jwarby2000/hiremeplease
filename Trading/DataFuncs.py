# -*- coding: utf-8 -*-

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
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
from sklearn.ensemble import RandomForestClassifier
from BinanceDataConcat import * 
from Models import *
# Function to load data

def LoadData(file, diff = True, downtime = True):
    data = pd.read_csv(file)
    print(file + ' loaded')
    data['Open_Time'] = pd.to_datetime(data['Open_Time']/1000, unit='s')
    if diff == True:
        data_np = data.values
        date_difference = np.empty(len(data_np))
        for i in range(len(data_np)):
            diff = data_np[i,0] - data_np[(i-1),0]
            date_difference[i] = diff.seconds
        data['diff'] = date_difference
        data['hour'] = data.Open_Time.dt.hour
        data['year'] = data.Open_Time.dt.year
        if downtime == False:
            data = data.loc[(data['diff'] == 60) | (data['diff'] == 120)] 
    return data

def FullLoad(file_name, analysis_features, first_date, last_date, diff = True, downtime = False):
    data = LoadData(file_name, diff = True, downtime = False)
    df = data.loc[(data['Open_Time']>=first_date) & (data['Open_Time']<=last_date), analysis_features]
    np = df.values
    return df, np

def DataFeatures(data, window = 10):
    data['change'] = data['close'].pct_change()
    data['spread'] = (data['high']-data['low'])/data['low']
    data['up_spread'] = (data['high']-data['open'])/data['open']
    data['down_spread'] = (data['open']-data['low'])/data['open']
    data['up_spread_c'] = (data['high']-data['close'])/data['close']
    data['down_spread_c'] = (data['close']-data['low'])/data['close']
    data['vol_change'] = data['volume'].pct_change()
    data['num_trade_change'] = data['trades'].pct_change()
    data['vol_per_trade'] = data['quote_vol']/data['trades']
    data['maker_buy_base_vol'] = data['volume'] - data['taker_buy_vol']
    data['maker_ratio'] = data['maker_buy_base_vol']/data['volume']
    data['volatility'] = data['change'].rolling(window = window).std()
    data['MA'] = data['change'].rolling(window = window).mean()
    return data

# Function to group 1 minute data into any interval (e.g 1m -> 5m). Note this function assumes the data is in numpy array due to speed issues with pandas dataframes

def GroupFunction(data, feature_list, t = 5, window = 10, shift_con = False, shift_t = 0, complete = True, corr = False, no_zero = False):
    groups = int(len(data[:,0])/t) # The amount of groups that will be created
    group_data = np.empty([groups, len(data[0])]) # Creating empty frame to fill
    
    # Aggregation of t minute observations into a single t length observation
    for i in range(0, groups):
        new_data = data[(i*t):((i+1)*t), :]
        group_data[i, 0] = new_data[0, 0] # Open
        group_data[i, 1] = new_data[-1, 3] # Close
        group_data[i, 2] = np.max(new_data[:, 1]) # High
        group_data[i, 3] = np.min(new_data[:, 2]) # Low
        group_data[i, 4] = np.sum(new_data[:, 4]) # Volume
        group_data[i, 5] = np.sum(new_data[:, 5]) # Number of Trades
        group_data[i, 6] = np.sum(new_data[:, 6]) # Taker buy base vol
        group_data[i, 7] = np.sum(new_data[:, 7]) # Quote (USD) volume
        group_data[i, 8] = new_data[0, 8] # hour
        group_data[i, 9] = new_data[0, 9] # year
    if corr == True: # 0's create nan when finding pct_change, therefore if looking for correlation metric to avoid this we make 0 -> 1. We cannot get rid of these entries as there may be different amounts for different coins
        group_data[:, 4] = np.where(group_data[:, 4] == 0, 1, group_data[:, 4]) 
        group_data[:, 5] = np.where(group_data[:, 5] == 0, 1, group_data[:, 5]) 
        group_data[:, 6] = np.where(group_data[:, 6] == 0, 1, group_data[:, 6]) 
        group_data[i, 7] = np.where(group_data[:, 7] == 0, 1, group_data[:, 7]) 
    data = pd.DataFrame(group_data, columns= ['open', 'close', 'high', 'low', 'volume', 'trades', 'taker_buy_vol', 'quote_vol', 'hour', 'year'])
    # If the data is incomplete, we do not want to calculate other columns yet
    if complete == False:
        return data.iloc[window:, :] 
    # If data is complete, we calculate other metrics
    else:
        data = data.loc[data['volume']>0]
        data = DataFeatures(data, window = window) # Function to calculate metrics
        # Removes entries that have no change 
        if no_zero == True:
            data = data.loc[data['change'] != 0]
        # Creates a variable that is the change for shift_t * t minutes into the future - used if we want to use 5 min data to predict 30 mins into future for eg
        if shift_con == True:
            data['Shifted'] = ((data['close'].shift(-shift_t)-data['close'])/data['close']).shift(1) # Shift back 1 to 
            feature_list = feature_list + ['Shifted']
            data = data.iloc[window:-shift_t, :]
            return data[feature_list] 
        return data.loc[window:,feature_list] 
    
def TakenFunction(data, t_lag = 1, n_steps = 50): # Create windows of the data to use for training for the neural network.
    batches = int((np.shape(data)[0]-(n_steps + 1))/t_lag)+1
    Taken = np.empty((batches, n_steps+1, np.shape(data)[1]))

    for i in range(batches):
        start = i*t_lag
        fin = n_steps +1 + start
        Taken[i, :, :] = data[start:fin,:]
        
    return Taken  

def TimeLagData(data, features, lags = 5):
    data = data[features]
    for k in features:
        for t in range(lags):
            data[k+'_'+str(t+1)] = data[k].shift(t+1)
    return data.iloc[lags:,:]

# Need to improve this to also take ML data, may for a specific amount of splits. ALso does not support shift_con for ML data right now.
def TrainTest(data, feature_list, t = 5, split = 0.85, n_steps = 10, t_lag_1= 1, t_lag_2 = 1, shift_con = False, shift_t = 5, corr = False, no_zero = False, grouped = False, ML = False):
    if grouped == False:
        data = GroupFunction(data = data, t = t, shift_con = shift_con, shift_t = shift_t, corr = corr, no_zero = no_zero, feature_list = feature_list)
    data = data.values
    train_index = int(split*len(data))
    valid_index = int((split + ((1-split)/2))*len(data))
    Train = data[:train_index, :]
    Valid = data[train_index:valid_index, :]
    Test = data[valid_index:, :]
    scaler = StandardScaler()
    Train_Scaled = scaler.fit_transform(Train)
    Valid_Scaled = scaler.transform(Valid)
    Test_Scaled = scaler.transform(Test)    
    
    if ML == True: #response variable 'change' is in first column
        X_Train = Train_Scaled[:, len(feature_list):]
        Y_Train = Train_Scaled[:, 0]
        X_Valid = Valid_Scaled[:, len(feature_list):]
        Y_Valid = Valid_Scaled[:, 0]
        X_Test = Test_Scaled[:, len(feature_list):]
        Y_Test = Test_Scaled[:, 0]
        
    else: #aka when not ML, sequential data for use in temporal ai such as lstm
        TTrain = TakenFunction(data = Train_Scaled, t_lag = t_lag_1, n_steps = n_steps)
        TValid = TakenFunction(data = Valid_Scaled, t_lag = t_lag_1, n_steps = n_steps)
        TTest = TakenFunction(data = Test_Scaled, t_lag = t_lag_2, n_steps = n_steps)
        
        if shift_con == False: 
            X_Train = TTrain[:, :n_steps,:]
            Y_Train = TTrain[:, n_steps, 0]
            X_Valid = TValid[:, :n_steps,:]
            Y_Valid = TValid[:, n_steps, 0]
            X_Test = TTest[:, :n_steps,:]
            Y_Test = TTest[:, n_steps, 0]
        else: 
            X_Train = TTrain[:, :n_steps,:-1]
            Y_Train = TTrain[:, (n_steps), -1]
            X_Valid = TValid[:, :n_steps,:-1]
            Y_Valid = TValid[:, (n_steps), -1]
            X_Test = TTest[:, :n_steps,:-1]
            Y_Test = TTest[:, (n_steps), -1]
            
    Y_Train_Actual = Y_Train*np.sqrt(scaler.var_[0]) + scaler.mean_[0]
    Y_Valid_Actual = Y_Valid*np.sqrt(scaler.var_[0]) + scaler.mean_[0]
    Y_Test_Actual = Y_Test*np.sqrt(scaler.var_[0]) + scaler.mean_[0] 
    Y_Train = np.where( (Y_Train*np.sqrt(scaler.var_[0]) + scaler.mean_[0])>0, 1, 0)
    print('Train Up Proportion ' ,sum(Y_Train)/len(Y_Train))
    if ML == False:
        Y_Train = keras.utils.to_categorical(Y_Train)
    Y_Valid = np.where( (Y_Valid*np.sqrt(scaler.var_[0]) + scaler.mean_[0])>0, 1, 0)
    print('Valid Up Proportion ' ,sum(Y_Valid)/len(Y_Valid))
    if ML == False:
        Y_Valid = keras.utils.to_categorical(Y_Valid)
    Y_Test = np.where( (Y_Test*np.sqrt(scaler.var_[0]) + scaler.mean_[0])>0, 1, 0)
    print('Test Up Proportion ' ,sum(Y_Test)/len(Y_Test))
    if ML == False:
        Y_Test = keras.utils.to_categorical(Y_Test)
        
    return X_Train, Y_Train, X_Test, Y_Test, X_Valid, Y_Valid, Y_Valid_Actual, Y_Train_Actual, Y_Test_Actual


# Visualing the data splits

def SplitPlot(data, feature_list, split, t):
    group_data = GroupFunction(data, feature_list = feature_list+['close'], t=t, shift_con = False)
    plot_data = group_data['close']
    train_index = int(split*len(plot_data))
    valid_index = int((split + ((1-split)/2))*len(plot_data))
    x_ticks = range(len(plot_data))
    Train = plot_data[:train_index]
    Valid = plot_data[train_index:valid_index]
    Test = plot_data[valid_index:]
    plt.plot(x_ticks[:train_index], Train)
    plt.plot(x_ticks[train_index:valid_index], Valid)
    plt.plot(x_ticks[valid_index:], Test)
