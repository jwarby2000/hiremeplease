import numpy as np
import pandas as pd
from sklearn.svm import SVC
import math

def getMyPosition(prcSoFar):
    nInst = 100
# Defining the Mapping Function    
    def mapping(x):
        y = 5*np.log((1+2*(x-0.5))/(1-2*(x-0.5)))
        return y

# Defining some placeholder variables
    nFeat, nNa = 2,10
    train_array = np.zeros((nInst, len(prcSoFar[1])-nNa, nFeat))
    model_list = list(np.repeat(0,nInst))
    currentpos = list(np.repeat(0,nInst))

# Fitting the models
    for i in range(50,100):
        prices = pd.Series(np.append(prcSoFar[i],float("NaN"))) 
        pct_changes = prices.pct_change() 
        # Fitting the SVC for each stock per day
        features = np.zeros((len(prcSoFar[1])+1, nFeat))
        features[:,0] = prices.shift(1) - prices.shift(1).rolling(10).mean()
        features[:,1] = np.where(pct_changes>=0, 0, 1) 
        train_array[i,:,:]= features[~np.any(np.isnan(features), axis = 1), :][:-1]
        classifier =  SVC(C = 5, gamma = 0.01, kernel = 'rbf', probability = True)
        model_list[i] = classifier.fit(train_array[i,:,:nFeat-1], train_array[i,:,nFeat-1])
        pred = model_list[i].predict_proba(features[-1,:nFeat-1].reshape(1,-1))[0,0]
        # Technical Analysis 
        if prices.shift(1).values[-1] <= (prices.shift(1).rolling(10).mean()-1.5*prices.shift(1).rolling(10).std()).values[-1]:
              pred = min(0.99,pred+0.1) 
        elif prices.shift(1).values[-1] >= (prices.shift(1).rolling(10).mean()+1.5*prices.shift(1).rolling(10).std()).values[-1]:
            pred = max(0.01,pred-0.1)
        # Decision on whether to create a long/short position ot to liquidate
        if pred >= 0.7:
            position = min(mapping(pred),10)
        elif pred <= 0.3:
            position = max(mapping(pred),-10)
        else:
            position = 0
        # Rounding down to ensure within 10k limit
        currentpos[i] = math.floor((position*1000)/prcSoFar[i, -1]) 

    return currentpos
