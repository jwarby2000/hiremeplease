# -*- coding: utf-8 -*-
"""
Created on Thu Jan  6 22:04:20 2022

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
from Models import *


def SimFunction(pred_vec, Actual_Returns, comm = 0.001, plot = True): 
    class_vec = np.where(pred_vec > 0.5, 1, 0)

    return_vec = np.array([])
    account_vec = np.array([])
    state = 2 #Dummy state for not currently holding a position
    trades = 0 #Amount of trades start at 0
    account = 1

    for i in range(len(pred_vec)):
        if class_vec[i] == 0:
           return_vec = np.append(return_vec, -Actual_Returns[i])
           if state != 0:
               trades += 1
               state = 0
               account = account*(1 - comm)
           account = account*(1 -Actual_Returns[i])
           account_vec = np.append(account_vec, account)
           
        elif class_vec[i] == 1:
           return_vec = np.append(return_vec, Actual_Returns[i]) 
           if state != 1: # If the previous state was not long
              trades += 1 # Make a trade (go long)
              state = 1 # Change state to long
              account = account*(1 - comm)
           account = account*(1+Actual_Returns[i])
           account_vec = np.append(account_vec, account)
       
    return_vec = return_vec + 1        
    Returns = pd.DataFrame(return_vec, columns = ['Returns'])
    Account_Value = pd.DataFrame(account_vec, columns = ['Account Value'])
    Comp_Ret = Returns.cumprod().iloc[-1,0]
    
    if plot == True:
        plt.figure()
        Returns.cumprod().plot()
        plt.show()
        plt.figure()
        Account_Value.plot()
        plt.figure()
    
    return Comp_Ret, account, trades

def TestFunction(X_Train, Y_Train, X_Test, Y_Test, Y_Test_Actual, n_exp = 5, epochs = 10, comm = 0.00075, model_name = 'LSTM', model_type = 'AI', units_1 = 50, units_2 = 50, units_3 = 50, units_4 = 50, units_5 = 50, units_d = 50, layers = 2, dropout = 0.2, optimizer = 'Adam', lr = 0.001, activation = 'selu', pool = False, n_estimators = 500, feature_list = None, Lagged = None, param = None):
    
    test_acc_vec = np.array([])
    test_loss_vec = np.array([])
    train_acc_vec = np.array([])
    train_loss_vec = np.array([])
    Comp_Ret_vec = np.array([])
    Account_vec = np.array([])
    trades_vec = np.array([])
    
    exps = 0
    
    while exps < n_exp:
        if model_type == 'AI':   
            if model_name == 'LSTM':
                pred_vec, test_acc, model = LSTMModel(X_Train, Y_Train, X_Test, Y_Test, units_1 = units_1, units_2 = units_2, units_d = units_d, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr)
                pred_vec = pred_vec[:,1]
            elif model_name == 'RNN':
                pred_vec, test_acc, model = RNNModel(X_Train, Y_Train, X_Test, Y_Test, units_1 = units_1, units_2 = units_2, units_d = units_d, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr)
                pred_vec = pred_vec[:,1]
            elif model_name == 'CNN':
                pred_vec, test_acc, model = CNNModel(X_Train, Y_Train, X_Test, Y_Test, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr)
                pred_vec = pred_vec[:,1]
            elif model_name == 'CNNLSTM':
                pred_vec, test_acc, y_proba, y_std, model = CNNLSTM(X_Train, Y_Train, X_Test, Y_Test, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr, pool = pool)
                pred_vec = pred_vec[:,1]
            elif model_name == 'MLP':
                pred_vec, test_acc, model = MLPModel(X_Train, Y_Train, X_Valid, Y_Valid, units_1 = units_1, units_2 = units_2, units_3 = units_3, units_4 = units_4, units_5 = units_5, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr, activation = activation)
            Comp_Ret, account, trades = SimFunction(pred_vec = pred_vec, Actual_Returns = Y_Test_Actual, comm = comm)
            test_loss, test_acc = model.evaluate(X_Test, Y_Test)
            train_loss, train_acc = model.evaluate(X_Train, Y_Train)
        elif model_type == 'ML':
            if model_name == 'XGB':
                pred_vec, test_acc, train_acc, test_loss, train_loss, model = XGBoostModel(X_Train, X_Test, Y_Train, Y_Test, param= param, Lagged = Lagged, feature_list = feature_list)
                pred_vec = pred_vec[:,1]
            elif model_name == 'RF':
                pred_vec, test_acc, train_acc, test_loss, train_loss, model = RandomForestModel(X_Train, X_Test, Y_Train, Y_Test, Lagged = Lagged, feature_list = feature_list, n_estimators = n_estimators)
                pred_vec = pred_vec[:,1]
            Comp_Ret, account, trades = SimFunction(pred_vec = pred_vec, Actual_Returns = Y_Test_Actual, comm = comm)
        if train_loss > 0.6930: #Instantly stuck in global minima
            continue
        else:
            exps = exps + 1
            test_acc_vec = np.append(test_acc_vec,test_acc)
            test_loss_vec = np.append(test_loss_vec,test_loss)
            train_acc_vec = np.append(train_acc_vec,train_acc)
            train_loss_vec = np.append(train_loss_vec,train_loss)
            Comp_Ret_vec = np.append(Comp_Ret_vec, Comp_Ret)
            Account_vec = np.append(Account_vec, account)
            trades_vec = np.append(trades_vec, trades)
    
    print('Av Test Accuracy is ', np.mean(test_acc_vec))
    print('Av Test Loss is ', np.mean(test_loss_vec))
    print('Av Train Accuracy is ', np.mean(train_acc_vec))
    print('Av Train Loss is ', np.mean(train_loss_vec))
    print('Av Compound Return is ', np.mean(Comp_Ret_vec))
    print('Av Account Value is ', np.mean(Account_vec))
    print('Av Trades is ', np.mean(trades_vec))
    
    return trades_vec