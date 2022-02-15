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
from DataFuncs import *

def XGBoostTune(X_Train, X_Test, Y_Train, Y_Test, param):
   split_index = [-1]*len(X_Test) + [0]*len(X_Test)
   X = np.concatenate((X_Train, X_Test), axis=0)
   y = np.concatenate((Y_Train, Y_Test), axis=0)
   pds = PredefinedSplit(test_fold = split_index)
   xgb_model = xgb.XGBClassifier()
   clf = GridSearchCV(xgb_model, param, cv=pds, 
                   scoring='neg_log_loss',
                   verbose=2)
   clf.fit(X,y)
   best_score = clf.best_score_
   best_estimator = clf.best_estimator_
   
   return best_score, best_estimator

def XGBoostModel(X_Train, X_Test, Y_Train, Y_Test, param, Lagged, feature_list):
    xgb_model = xgb.XGBClassifier(**param)
    xgb_model.fit(X_Train, Y_Train, eval_metric = ['error','logloss'], eval_set = [(X_Train, Y_Train),(X_Test, Y_Test)])
    average_gain = list(zip(Lagged.columns[len(feature_list):],xgb_model.get_booster().get_score(importance_type="gain").values()))
    total_gain = list(zip(Lagged.columns[len(feature_list):],xgb_model.get_booster().get_score(importance_type="total_gain").values()))
    print('AVERAGE GAIN PER VARIABLE:')
    print(*average_gain, sep="\n")
    print('TOTAL GAIN PER VARIABLE:')
    print(*total_gain, sep="\n")
    pred_vec = xgb_model.predict_proba(X_Test)
    y_pred = xgb_model.predict(X_Test)
    test_acc = accuracy_score(y_pred, Y_Test)
    train_pred = xgb_model.predict(X_Train)
    train_prob = xgb_model.predict_proba(X_Train)
    train_acc = accuracy_score(train_pred, Y_Train)
    train_loss = log_loss(Y_Train, train_prob[:,1])
    test_loss = log_loss(Y_Test, pred_vec[:,1])

    return pred_vec, test_acc, train_acc, test_loss, train_loss, xgb_model

def RandomForestModel(X_Train, X_Test, Y_Train, Y_Test, Lagged, feature_list, n_estimators = 500):
    rf_clf = RandomForestClassifier(n_estimators = n_estimators, n_jobs = -1, oob_score = True)
    rf_clf.fit(X_Train, Y_Train)
    pred_vec = rf_clf.predict_proba(X_Test)
    y_pred = rf_clf.predict(X_Test)
    test_acc = accuracy_score(y_pred, Y_Test)
    train_pred = rf_clf.predict(X_Train)
    train_prob = rf_clf.predict_proba(X_Train)
    train_acc = accuracy_score(train_pred, Y_Train)
    test_acc = accuracy_score(y_pred, Y_Test)
    train_pred = rf_clf.predict(X_Train)
    train_prob = rf_clf.predict_proba(X_Train)
    train_acc = accuracy_score(train_pred, Y_Train)
    train_loss = log_loss(Y_Train, train_prob[:,1])
    test_loss = log_loss(Y_Test, pred_vec[:,1])
    
    importances = list(zip(Lagged.columns[len(feature_list):], rf_clf.feature_importances_))
    print('Accuracy Score on Test Set:', accuracy_score(y_pred, Y_Test))
    print('\n')
    print('OOB Score on Train Set:', rf_clf.oob_score_)
    print('\n')
    print('Feature Importances:')
    print(*importances, sep = '\n')
    
    return pred_vec, test_acc, train_acc, test_loss, train_loss, rf_clf

def MLPModel(X_Train, Y_Train, X_Test, Y_Test, units_1 = 50, units_2 = 50, units_3 = 50, units_4 = 50, units_5 = 50, layers = 3, dropout = 0.5, activation = 'selu', epochs = 5, optimizer = 'Adam', lr = 0.001):
    input_dim = np.shape(X_Train)[1]
    if layers == 1:
       model = keras.models.Sequential()
       model.add(keras.layers.Dense(units_1, input_dim = input_dim, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(1, activation = 'sigmoid'))
    elif layers == 2:   
       model = keras.models.Sequential()
       model.add(keras.layers.Dense(units_1, input_dim = input_dim, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_2, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(1, activation = 'sigmoid'))
    elif layers == 3:   
       model = keras.models.Sequential()
       model.add(keras.layers.Dense(units_1, input_dim = input_dim, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_2, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_3, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(1, activation = 'sigmoid'))
    elif layers == 4:   
       model = keras.models.Sequential()
       model.add(keras.layers.Dense(units_1, input_dim = input_dim, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_2, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_3, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_4, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(1, activation = 'sigmoid'))
    elif layers == 5:   
       model = keras.models.Sequential()
       model.add(keras.layers.Dense(units_1, input_dim = input_dim, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_2, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_3, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_4, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(units_5, activation = activation))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(1, activation = 'sigmoid'))       
    else:
        print('Incorrect Layers')
    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSProp':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")
    model.compile(loss = "binary_crossentropy", optimizer = optimizer, metrics = ['accuracy'])
    history = model.fit(X_Train, Y_Train, epochs = epochs, validation_data = (X_Test, Y_Test))     
    pred_vec = model.predict(X_Test)
    train_prob = model.predict(X_Train)
    _, test_acc = model.evaluate(X_Test, Y_Test)
    
    return pred_vec, test_acc, model

### RNN

def RNNModel(X_Train, Y_Train, X_Test, Y_Test, units_1 = 50, units_2 = 50, units_d = 50, layers = 2, dropout = 0.5, epochs = 5, optimizer = 'Adam', lr = 0.001):
    input_shape = [np.shape(X_Train)[1], np.shape(X_Train)[2]]
    if layers == 1:
       model = keras.models.Sequential()
       model.add(keras.layers.SimpleRNN(units_1, input_shape = input_shape))
       model.add(keras.layers.Dense(units_d, activation = 'selu'))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(2, activation = 'softmax'))
    elif layers == 2:   
       model = keras.models.Sequential()
       model.add(keras.layers.SimpleRNN(units_1, return_sequences = True, input_shape = input_shape))
       model.add(keras.layers.SimpleRNN(units_2))
       model.add(keras.layers.Dense(units_d, activation = 'selu'))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(2, activation = 'softmax'))
    else:
       print('Incorrect Layers')
    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSProp':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")
    model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])
    history = model.fit(X_Train, Y_Train, epochs = epochs, validation_data = (X_Test, Y_Test))     
    pred_vec = model.predict(X_Test)
    train_prob = model.predict(X_Train)
    _, test_acc = model.evaluate(X_Test, Y_Test)

    return pred_vec, test_acc, model


### LSTM

def LSTMModel(X_Train, Y_Train, X_Test, Y_Test, units_1 = 50, units_2 = 50, units_d = 50, layers = 2, dropout = 0.5, epochs = 5, optimizer = 'Adam', lr = 0.001):
    input_shape = [np.shape(X_Train)[1], np.shape(X_Train)[2]]
    if layers == 1:
       model = keras.models.Sequential()
       model.add(keras.layers.LSTM(units_1, input_shape = input_shape))
       model.add(keras.layers.Dense(units_d, activation = 'selu'))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(2, activation = 'softmax'))
    elif layers == 2:   
       model = keras.models.Sequential()
       model.add(keras.layers.LSTM(units_1, return_sequences = True, input_shape = input_shape))
       model.add(keras.layers.LSTM(units_2))
       model.add(keras.layers.Dense(units_d, activation = 'selu'))
       model.add(keras.layers.Dropout(dropout))
       model.add(keras.layers.Dense(2, activation = 'softmax'))
    else:
       print('Incorrect Layers')
    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSProp':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")
    model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])
    history = model.fit(X_Train, Y_Train, epochs = epochs, validation_data = (X_Test, Y_Test))     
    pred_vec = model.predict(X_Test)
    train_prob = model.predict(X_Train)
    _, test_acc = model.evaluate(X_Test, Y_Test)

    return pred_vec, test_acc, model

### CNNLSTM

def CNNLSTM(X_Train, Y_Train, X_Test, Y_Test, dropout = 0.5, epochs = 5, pool = False, optimizer = 'Adam', lr = 0.001):
    X_Train_ = X_Train.reshape((np.shape(X_Train)[0], np.shape(X_Train)[1], np.shape(X_Train)[2], 1))
    X_Test_ = X_Test.reshape((np.shape(X_Test)[0], np.shape(X_Test)[1], np.shape(X_Test)[2],1))
    train_loss = 0.7 # Initalise at a high value so the while loop starts
    
    input_shape = [np.shape(X_Train_)[1], np.shape(X_Train_)[2],1]
    model = keras.models.Sequential()
    model.add(keras.layers.Conv2D(16, (1,3), activation = 'relu', padding = 'same', input_shape = input_shape))
    model.add(keras.layers.Conv2D(16, (1,2), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (1,3), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (1,2), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (1,3), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (1,2), activation = 'relu', padding = 'same'))
    if pool == True:
        model.add(keras.layers.MaxPooling2D(pool_size = (2,2), strides = (1,1)))
        model.add(keras.layers.Conv2D(1, (1,1), activation = 'relu', padding = 'same'))
        model.add(keras.layers.Reshape((np.shape(X_Train_)[1]-1,np.shape(X_Train_)[2]-1)))
    else:
        model.add(keras.layers.Conv2D(1, (1,1), activation = 'relu', padding = 'same'))
        model.add(keras.layers.Reshape((np.shape(X_Train_)[1],np.shape(X_Train_)[2])))
    model.add(keras.layers.LSTM(30))
    model.add(keras.layers.Dense(30, activation = 'selu'))
    model.add(keras.layers.Dropout(dropout))
    model.add(keras.layers.Dense(2, activation = 'softmax'))


    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSProp':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")
    model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])
    history = model.fit(X_Train_, Y_Train, epochs = epochs, validation_data = (X_Test_, Y_Test))      
    train_loss, train_acc = model.evaluate(X_Train_, Y_Train)
    
    pred_vec = model.predict(X_Test_)
    train_prob = model.predict(X_Train_)
    _, test_acc = model.evaluate(X_Test_, Y_Test)
    
    y_probas = np.stack([model(X_Test_, training = True) for sample in range(100)])
    y_proba = y_probas.mean(axis = 0)
    y_std = y_probas.std(axis = 0)
    
    return pred_vec, test_acc, y_proba, y_std, model

### CNN

def CNNModel(X_Train, Y_Train, X_Test, Y_Test, dropout = 0.5, epochs = 5, optimizer = 'Adam', lr = 0.001):
    X_Train_ = X_Train.reshape((np.shape(X_Train)[0], np.shape(X_Train)[1], np.shape(X_Train)[2], 1))
    X_Test_ = X_Test.reshape((np.shape(X_Test)[0], np.shape(X_Test)[1], np.shape(X_Test)[2],1))
    train_loss = 0.7 # Initalise at a high value so the while loop starts
    
    input_shape = [np.shape(X_Train_)[1], np.shape(X_Train_)[2],1]
    model = keras.models.Sequential()
    model.add(keras.layers.Conv2D(16, (3,3), activation = 'relu', padding = 'valid', input_shape = input_shape))
    model.add(keras.layers.Conv2D(16, (2,2), activation = 'relu', padding = 'valid'))
    model.add(keras.layers.Conv2D(8, (3,3), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (2,2), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (3,3), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Conv2D(8, (2,2), activation = 'relu', padding = 'same'))
    model.add(keras.layers.Flatten())
    model.add(keras.layers.Dense(30, activation = 'selu'))
    model.add(keras.layers.Dropout(dropout))
    model.add(keras.layers.Dense(2, activation = 'softmax'))

    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSProp':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")

    model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])    
    history = model.fit(X_Train_, Y_Train, epochs = epochs, validation_data = (X_Test_, Y_Test))      
    train_loss, train_acc = model.evaluate(X_Train_, Y_Train)    
    
    pred_vec = model.predict(X_Test_)
    train_prob  = model.predict(X_Train_)
    _, test_acc = model.evaluate(X_Test_, Y_Test)
    

    return pred_vec, test_acc, model

    #while train_loss > 0.6920: # Retrain if the model gets stuck in a local minima instantly and therefore cannot find a good solution
        #model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])    
        #history = model.fit(X_Train_, Y_Train, epochs = 2, validation_data = (X_Test_, Y_Test))      
        #train_loss, train_acc = model.evaluate(X_Train_, Y_Train)  


### Two Lane Model - Experimental

def TwoLaneModel(X_Train_1, X_Train_2, Y_Train, X_Test_1, X_Test_2, Y_Test, units = 30, dropout = 0.5, layers = 2, optimizer = 'Adam', lr = 0.001, epochs = 5):
    shape = [np.shape(X_Train_1)[1], np.shape(X_Train_1)[2]]
    input_t1 = keras.layers.Input(shape = shape, name = 't1 input')
    input_t2 = keras.layers.Input(shape = shape, name = 't2 input')
    if layers == 1:
        LSTMt1 = keras.layers.LSTM(units)(input_t1)
        LSTMt2 = keras.layers.LSTM(units)(input_t2)
        concat = keras.layers.concatenate([LSTMt1, LSTMt2])
    elif layers == 2:   
        LSTMt1_1 = keras.layers.LSTM(units, return_sequences = True)(input_t1)
        LSTMt2_1 = keras.layers.LSTM(units, return_sequences = True)(input_t2)
        LSTMt1_2 = keras.layers.LSTM(units)(LSTMt1_1)
        LSTMt2_2 = keras.layers.LSTM(units)(LSTMt2_1)
        concat = keras.layers.concatenate([LSTMt1_2, LSTMt2_2])
    elif layers == 3:
        LSTMt1_1 = keras.layers.LSTM(units, return_sequences = True)(input_t1)
        LSTMt2_1 = keras.layers.LSTM(units, return_sequences = True)(input_t2)
        LSTMt1_2 = keras.layers.LSTM(units, return_sequences = True)(LSTMt1_1)
        LSTMt2_2 = keras.layers.LSTM(units, return_sequences = True)(LSTMt2_1)
        LSTMt1_3 = keras.layers.LSTM(units)(LSTMt1_2)
        LSTMt2_3 = keras.layers.LSTM(units)(LSTMt2_2)
        concat = keras.layers.concatenate([LSTMt1_3, LSTMt2_3])
    else:
        print('Incorrect Layers')
    dense1 = keras.layers.Dense(units, activation = 'selu')(concat)
    dropout1 = keras.layers.Dropout(dropout)(dense1)  
    output = keras.layers.Dense(2, activation = 'softmax', name = 'output')(dropout1)
    
    model = keras.Model(inputs = [input_t1, input_t2], outputs = [output])
    if optimizer == 'Adam':
       optimizer = keras.optimizers.Adam(learning_rate=lr) 
    elif optimizer == 'RMSprop':
       optimizer = keras.optimizers.RMSprop(learning_rate=lr)
    elif optimizer == 'SGD':
       optimizer = keras.optimizers.SGD(learning_rate=lr)
    else:
        print(optimizer, "Not Found")
    model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])
    history = model.fit((X_Train_1, X_Train_2), Y_Train, epochs = epochs, validation_data = ((X_Test_1, X_Test_2), Y_Test))
    pred_vec = model.predict((X_Test_1, X_Test_2))
    train_prob = model.predict((X_Train_1, X_Train_2))
    _, test_acc = model.evaluate((X_Test_1, X_Test_2), Y_Test)
    

    return pred_vec, test_acc, model
