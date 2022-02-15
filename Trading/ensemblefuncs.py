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
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report, log_loss
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV, PredefinedSplit
from BinanceDataConcat import * 
from Models import *
from DataFuncs import *
from testfuncs import *
import datetime as dt
from sklearn.linear_model import LogisticRegression

def Ensemble(X_Train, Y_Train, X_Valid, Y_Valid, Y_Valid_Actual, X_Train_ML, Y_Train_ML, X_Valid_ML, Y_Valid_ML, Y_Valid_Actual_ML, model_list, stacking = True, X_Test = None, Y_Test = None, X_Test_ML = None, Y_Test_ML = None, Y_Test_Actual = None, Y_Test_Actual_ML = None, repeats = 1, epochs = 5, comm = 0.00075, units_1 = 50, units_2 = 50, units_3 = 50, units_4 = 50, units_5 = 50, units_d = 50, layers = 2, dropout = 0.2, optimizer = 'Adam', lr = 0.001, activation = 'selu', pool = False, n_estimators = 500, feature_list = None, Lagged = None, param = None):
    
    soft_df_valid = pd.DataFrame()
    hard_df_valid = pd.DataFrame()
    extra_valid = len(Y_Valid_ML) - len(Y_Valid) # ML datasets have set amount extra due to the way they are constructed
    y_class_valid = np.where(Y_Valid_Actual > 0, 1, 0)
    acc_list_valid = []
    
    if stacking == True:
        soft_df_test = pd.DataFrame()
        hard_df_test = pd.DataFrame() 
        extra_test = len(Y_Test_ML) - len(Y_Test) 
        y_class_test = np.where(Y_Test_Actual > 0, 1, 0)
        acc_list_test = []
        X_Train_Full = np.vstack([X_Train, X_Valid])
        Y_Train_Full = np.vstack([Y_Train, Y_Valid])
        X_Train_Full_ML = np.vstack([X_Train_ML, X_Valid_ML])
        Y_Train_Full_ML = np.vstack([Y_Train_ML.reshape(-1,1), Y_Valid_ML.reshape(-1,1)])
        
    if 'LSTM' in model_list:
        current_model = 'LSTM'
        for i in range(repeats):
            pred_vec, test_acc, model = LSTMModel(X_Train, Y_Train, X_Valid, Y_Valid, units_1 = units_1, units_2 = units_2, units_d = units_d, layers = layers, dropout = dropout, epochs = epochs, optimizer = optimizer, lr = lr)
            if i == 0:
                pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average
        
        if stacking == True:
            for i in range(repeats):
                pred_vec, test_acc, model = LSTMModel(X_Train_Full, Y_Train_Full, X_Test, Y_Test, units_1 = units_1, units_2 = units_2, units_d = units_d, layers = layers, dropout = dropout, epochs = epochs, optimizer = optimizer, lr = lr)
                if i == 0:
                    pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average
         
        
    if 'RNN' in model_list:
        current_model = 'RNN' 
        for i in range(repeats):
            pred_vec, test_acc, model = RNNModel(X_Train, Y_Train, X_Valid, Y_Valid, units_1 = units_1, units_2 = units_2, units_d = units_d, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr)   
            if i == 0:
                pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average             

        if stacking == True:
            for i in range(repeats):
                pred_vec, test_acc, model = RNNModel(X_Train_Full, Y_Train_Full, X_Test, Y_Test, units_1 = units_1, units_2 = units_2, units_d = units_d, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr)   
                if i == 0:
                    pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average

    if 'CNN' in model_list:
        current_model = 'CNN'
        test_acc = 0.5
        for i in range(repeats):
            while test_acc < 0.51:
                pred_vec, test_acc, model = CNNModel(X_Train, Y_Train, X_Valid, Y_Valid, epochs = epochs, dropout = dropout, optimizer = optimizer, lr = lr)
            if i == 0:
                pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average          

        if stacking == True:
            for i in range(repeats):
                while test_acc < 0.51:
                    pred_vec, test_acc, model = CNNModel(X_Train_Full, Y_Train_Full, X_Test, Y_Test, epochs = epochs, dropout = dropout, optimizer = optimizer, lr = lr)
                if i == 0:
                    pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average

    
    if 'CNNLSTM' in model_list:
        current_model = 'CNNLSTM'
        test_acc = 0.5
        for i in range(repeats):
            while test_acc < 0.51:
                pred_vec, test_acc, y_proba, y_std, model = CNNLSTM(X_Train, Y_Train, X_Valid, Y_Valid, epochs = epochs, dropout = dropout, optimizer = optimizer, lr = lr, pool = pool)
            if i == 0:
                pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
       
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average          

        if stacking == True:
            for i in range(repeats):
                while test_acc < 0.51:
                    pred_vec, test_acc, model = CNNLSTM(X_Train_Full, Y_Train_Full, X_Test, Y_Test, epochs = epochs, dropout = dropout, optimizer = optimizer, lr = lr, pool = pool)
                if i == 0:
                    pred_matrix = np.array(pred_vec[:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average

    
    if 'MLP' in model_list:
        current_model = 'MLP'
        for i in range(repeats):
            pred_vec, test_acc, model = MLPModel(X_Train_ML, Y_Train_ML, X_Valid_ML, Y_Valid_ML, units_1 = units_1, units_2 = units_2, units_3 = units_3, units_4 = units_4, units_5 = units_5, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr, activation = activation) 
            if i == 0:
                pred_matrix = np.array(pred_vec[extra_valid:]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[extra_valid:].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average          
        
        if stacking == True:
            for i in range(repeats):
                pred_vec, test_acc, model = MLPModel(X_Train_Full_ML, Y_Train_Full_ML, X_Test_ML, Y_Test_ML, units_1 = units_1, units_2 = units_2, units_3 = units_3, units_4 = units_4, units_5 = units_5, epochs = epochs, layers = layers, dropout = dropout, optimizer = optimizer, lr = lr, activation = activation)  
                if i == 0:
                    pred_matrix = np.array(pred_vec[extra_test:]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[extra_test:].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average
    
    if 'XGB' in model_list:
        current_model = 'XGB'
        for i in range(1): #do these need to be repeated?
            pred_vec, test_acc, train_acc, test_loss, train_loss, model = XGBoostModel(X_Train_ML, X_Valid_ML, Y_Train_ML, Y_Valid_ML, param= param, Lagged = Lagged, feature_list = feature_list)
            if i == 0:
                pred_matrix = np.array(pred_vec[extra_valid:, 1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[extra_valid:, 1].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average          

        if stacking == True:
            for i in range(1):
                pred_vec, test_acc, train_acc, test_loss, train_loss, model = XGBoostModel(X_Train_Full_ML, X_Test_ML, Y_Train_Full_ML, Y_Test_ML, param= param, Lagged = Lagged, feature_list = feature_list)
                if i == 0:
                    pred_matrix = np.array(pred_vec[extra_test:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[extra_test:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average

    if 'RF' in model_list:
        current_model = 'RF'
        for i in range(1): #do these need to be repeated? maybe this one as it is quite random but boosting is not as random
            pred_vec, test_acc, train_acc, test_loss, train_loss, model = RandomForestModel(X_Train_ML, X_Valid_ML, Y_Train_ML, Y_Valid_ML, Lagged = Lagged, feature_list = feature_list, n_estimators = n_estimators)              
            if i == 0:
                pred_matrix = np.array(pred_vec[extra_valid:,1]).reshape(-1,1)
            else:
                pred_matrix = np.hstack([pred_matrix, pred_vec[extra_valid:,1].reshape(-1,1)])
        
        pred_vec_average = np.mean(pred_matrix, axis = 1)
        class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
        test_acc_average = accuracy_score(class_vec_average, y_class_valid)
        acc_list_valid.append((current_model, test_acc_average))
        hard_df_valid[current_model] = class_vec_average
        soft_df_valid[current_model] = pred_vec_average   
 
        if stacking == True:
            for i in range(1):
                pred_vec, test_acc, train_acc, test_loss, train_loss, model = RandomForestModel(X_Train_Full_ML, X_Test_ML, Y_Train_Full_ML, Y_Test_ML, Lagged = Lagged, feature_list = feature_list, n_estimators = n_estimators)              
                if i == 0:
                    pred_matrix = np.array(pred_vec[extra_test:,1]).reshape(-1,1)
                else:
                    pred_matrix = np.hstack([pred_matrix, pred_vec[extra_test:,1].reshape(-1,1)])
        
            pred_vec_average = np.mean(pred_matrix, axis = 1)
            class_vec_average = np.where(pred_vec_average > 0.5, 1, 0)
            test_acc_average = accuracy_score(class_vec_average, y_class_test)
            acc_list_test.append((current_model, test_acc_average))
            hard_df_test[current_model] = class_vec_average
            soft_df_test[current_model] = pred_vec_average    
 
    # VALID    
    #Soft Vote
    probs = soft_df_valid.sum(axis = 1) 
    softs = np.where(probs > len(model_list)/2, 1, 0)
    test_acc = accuracy_score(y_class_valid, softs)
    acc_list_valid.append(('Soft Vote', test_acc))
        
    #Hard Vote
    votes = hard_df_valid.sum(axis = 1)
    hards = np.where(votes > len(model_list)/2, 1, 0)
    test_acc = accuracy_score(y_class_valid, hards)
    acc_list_valid.append(('Hard Vote', test_acc))
        
    print('Validation Set Accuracy')
    print(*acc_list_valid, sep ='\n')    
    
    if stacking == False:
        
        return acc_list_valid, soft_df_valid
    
    elif stacking == True:
        
        #Soft Vote
        probs = soft_df_test.sum(axis = 1) 
        softs = np.where(probs > len(model_list)/2, 1, 0)
        test_acc = accuracy_score(y_class_test, softs)
        acc_list_test.append(('Soft Vote', test_acc))
        
        #Hard Vote
        votes = hard_df_test.sum(axis = 1)
        hards = np.where(votes > len(model_list)/2, 1, 0)
        test_acc = accuracy_score(y_class_test, hards)
        acc_list_test.append(('Hard Vote', test_acc))
        
        print('Test Set Accuracy')
        print(*acc_list_test, sep ='\n')
        
        # Meta Learner
        
        stacked_model = LogisticRegression()
        stacked_model.fit(soft_df_valid, y_class_valid)
        stack_pred = stacked_model.predict(soft_df_test)
        stack_acc = accuracy_score(stack_pred, y_class_test)
        print('Stacked Accuracy: ', stack_acc)
        
        return acc_list_valid, acc_list_test, soft_df_valid, soft_df_test
