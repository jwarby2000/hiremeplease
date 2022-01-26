# Data exploration plotting is at the bottom of the script so that it is more readable


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import tensorflow as tf
from tensorflow import keras
from sklearn.preprocessing import MinMaxScaler, OrdinalEncoder
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_curve, roc_auc_score, confusion_matrix, ConfusionMatrixDisplay

cat = ['A1', 'A4', 'A5', 'A6', 'A7', 'A9', 'A10', 'A12', 'A13']
cont = ['A2', 'A3', 'A8', 'A11', 'A15']

def import_data():
    data = pd.read_csv('crx.data', header = None, na_values  = '?')
    data = data.dropna()
    names = ['A1','A2','A3','A4','A5','A6','A7','A8','A9','A10','A11','A12','A13','A14','A15','Y']
    data.columns = names
    X = data.iloc[:,:-1]
    data['Y'] = np.where(data['Y'] == '+', 1, 0)
    Y = data['Y']
    Y = keras.utils.to_categorical(Y)
    cat = ['A1', 'A4', 'A5', 'A6', 'A7', 'A9', 'A10', 'A12', 'A13']
    cont = ['A2', 'A3', 'A8', 'A11', 'A15']
    X_cont = X[cont].values
    X_cat = X[cat].values
    ordinal = OrdinalEncoder()
    X_cat = ordinal.fit_transform(X_cat)
    X = np.hstack([X_cont, X_cat])
    return data, X, Y

data, X, Y = import_data()

scaler = MinMaxScaler()

# Saving an example of the data

X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = 0.5, random_state = 1)
X_train_scaled = scaler.fit_transform(X_train)
X_test_scaled = scaler.transform(X_test)
X_concat = np.vstack([X_train_scaled, X_test_scaled])
Y_concat = np.vstack([Y_train, Y_test])
Concat = np.hstack([X_concat,Y_concat])
np.savetxt('data.csv', Concat, delimiter = ',')

 
# Model building
	
np.random.seed(1)
tf.random.set_seed(2)

#If you want to run this faster, you can change n_exp in the default to 1.
#This is the function that does all the modelling

def Model_Train(grad = 0, reg = 0, dropout = 0, units = 10, n_exp = 50, epochs = 100):

    Test_acc = np.zeros(n_exp)
    Train_acc = np.zeros(n_exp)
    pred_list = [0]*n_exp
    class_list = [0]*n_exp
    Y_test_list = [0]*n_exp

    for i in range(n_exp):
        X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = 0.5, random_state = i)
        X_train_scaled = scaler.fit_transform(X_train)
        X_test_scaled = scaler.transform(X_test)
        
        if grad == 0:
            model = keras.models.Sequential()
            model.add(keras.layers.Dense(units, input_dim = X_train_scaled.shape[1], activation = 'relu', kernel_regularizer=keras.regularizers.l2(reg)))
            model.add(keras.layers.Dropout(dropout))
            model.add(keras.layers.Dense(2, activation = 'softmax'))
            
            optimizer = keras.optimizers.SGD(learning_rate=0.01)
            model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])

            
        elif grad == 1:
            model = keras.models.Sequential()
            model.add(keras.layers.Dense(units, input_dim = X_train_scaled.shape[1], activation = 'relu', kernel_regularizer=keras.regularizers.l2(reg)))
            model.add(keras.layers.Dropout(dropout))
            model.add(keras.layers.Dense(2, activation = 'softmax'))
            
            optimizer = keras.optimizers.Adam(learning_rate=0.01)
            model.compile(loss = "categorical_crossentropy", optimizer = optimizer, metrics = ['accuracy'])

        else:
            print('Invalid Input')
            break
    
        history = model.fit(X_train_scaled, Y_train, epochs = epochs, validation_data = (X_test_scaled, Y_test))
        _, test_a= model.evaluate(X_test_scaled, Y_test)
        _, train_a = model.evaluate(X_train_scaled, Y_train)
        Train_acc[i] = train_a
        Test_acc[i] = test_a
        pred = model.predict(X_test_scaled)[:,1]
        pred_list[i] = pred
        pred_class = np.where(pred > 0.5, 1, 0)
        class_list[i] = pred_class
        Y_test_list[i] = Y_test[:,1]
    return Test_acc, Train_acc, pred_list, class_list, Y_test_list

SGD_test, SGD_train, SGD_pred, SGD_class, Y_test_list = Model_Train(grad = 0, reg = 0, dropout = 0, units = 50, epochs = 200)
Adam_test, Adam_train, Adam_pred, Adam_class, Y_test_list = Model_Train(grad = 1, reg = 0, dropout = 0, units = 50, epochs = 25)

Adam_l2_test, Adam_l2_train, Adam_l2_pred, Adam_l2_class, Y_test_list = Model_Train(grad = 1, reg = 0.01, dropout = 0, units = 50, epochs = 25)
Adam_drop_test, Adam_drop_train, Adam_drop_pred, Adam_drop_class, Y_test_list = Model_Train(grad = 1, reg = 0, dropout = 0.5, units = 50, epochs = 25)




# Plotting Data Exploration

plt.figure()
sns.heatmap(data.corr(), annot = True)
plt.savefig('CorMatrix.png')
plt.figure()
data['Y'].value_counts().plot(kind = 'bar')
plt.savefig('ResponseDist.png')
sns.set(font_scale = 1.5)
plt.figure()
sns.pairplot(data=data, hue='Y')
plt.savefig('PairPlot.png')

f, axes = plt.subplots(1, 3, figsize = (20,5), sharey = True)
data.groupby([cat[0],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[0])
axes[0].tick_params(axis='both', which='major', labelsize=20, rotation = 0)
axes[0].set_title(cat[0], fontsize = 20)
axes[0].set_xlabel(None)
data.groupby([cat[1],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[1])
axes[1].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[1].set_title(cat[1], fontsize = 20)
axes[1].set_xlabel(None)
data.groupby([cat[2],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[2])
axes[2].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[2].set_title(cat[2], fontsize = 20)
axes[2].set_xlabel(None)
plt.savefig('Cat1.png')
f, axes = plt.subplots(1, 3, figsize = (20,5), sharey = True)
data.groupby([cat[3],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[0])
axes[0].tick_params(axis='both', which='major', labelsize=20, rotation = 0)
axes[0].set_title(cat[3], fontsize = 20)
axes[0].set_xlabel(None)
data.groupby([cat[4],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[1])
axes[1].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[1].set_title(cat[4], fontsize = 20)
axes[1].set_xlabel(None)
data.groupby([cat[5],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[2])
axes[2].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[2].set_title(cat[5], fontsize = 20)
axes[2].set_xlabel(None)
plt.savefig('Cat2.png')
f, axes = plt.subplots(1, 3, figsize = (20,5), sharey = True)
data.groupby([cat[6],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[0])
axes[0].tick_params(axis='both', which='major', labelsize=20, rotation = 0)
axes[0].set_title(cat[6], fontsize = 20)
axes[0].set_xlabel(None)
data.groupby([cat[7],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[1])
axes[1].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[1].set_title(cat[7], fontsize = 20)
axes[1].set_xlabel(None)
data.groupby([cat[8],'Y']).size().groupby(level=0).apply(lambda x: 100 * x / x.sum()).unstack().plot(kind='bar',stacked=True, ax = axes[2])
axes[2].tick_params(axis='x', which='major', labelsize=20, rotation = 0)
axes[2].set_title(cat[8], fontsize = 20)
axes[2].set_xlabel(None)
plt.savefig('Cat3.png')



# Roc Curve plotting


Adam_fpr, Adam_tpr, _ = roc_curve(Y_test_list[2], Adam_pred[2])
Adam_l2_fpr, Adam_l2_tpr, _ = roc_curve(Y_test_list[2], Adam_l2_pred[2])
Adam_drop_fpr, Adam_drop_tpr, _ = roc_curve(Y_test_list[2], Adam_drop_pred[2])

roc_auc_score(Y_test_list[2], Adam_pred[2])
roc_auc_score(Y_test_list[2], Adam_l2_pred[2])
roc_auc_score(Y_test_list[2], Adam_drop_pred[2])


f, axes = plt.subplots(1, 3, figsize=(20, 5), sharey='row')


axes[0].plot(Adam_fpr, Adam_tpr, label = 'Adam')
axes[0].axline((0, 0), slope=1, color="black")
axes[0].set_title('Adam', fontsize = 20)
axes[0].set_ylabel('True Positive Rate', fontsize = 20)

axes[1].plot(Adam_l2_fpr, Adam_l2_tpr, label = 'Adam L2', color = 'orange')
axes[1].axline((0, 0), slope=1, color="black")
axes[1].set_title('Adam L2', fontsize = 20)
axes[1].set_xlabel('False Positive Rate',fontsize = 20)

axes[2].plot(Adam_drop_fpr, Adam_drop_tpr, label = 'Adam Drop', color = 'green')
axes[2].axline((0, 0), slope=1, color="black")
axes[2].set_title('Adam Drop', fontsize = 20)

plt.subplots_adjust(wspace=0.40, hspace=0.1)
plt.savefig('AUCs.png')
plt.show()


plt.plot(Adam_fpr, Adam_tpr, label = 'Adam')
plt.plot(Adam_l2_fpr, Adam_l2_tpr, label = 'Adam l2', color = 'orange')
plt.plot(Adam_drop_fpr, Adam_drop_tpr, label = 'Adam drop', color = 'green')
plt.axline((0, 0), slope=1, color="black")
# axis labels
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
# show the legend
plt.legend()
plt.savefig('ComAuc.png')


# Confusion Matricies Plotting

f, axes = plt.subplots(1, 3, figsize=(20, 5), sharey='row')

cm_1 = confusion_matrix(Y_test_list[2], Adam_class[2])
cm_display_1 = ConfusionMatrixDisplay(cm_1, display_labels=['Negative', 'Positive'])
cm_2 = confusion_matrix(Y_test_list[2], Adam_l2_class[2])
cm_display_2 = ConfusionMatrixDisplay(cm_2, display_labels=['Negative', 'Positive'])
cm_3 = confusion_matrix(Y_test_list[2], Adam_drop_class[2])
cm_display_3 = ConfusionMatrixDisplay(cm_3, display_labels=['Negative', 'Positive'])

cm_display_1.plot(ax = axes[0])
cm_display_1.ax_.set_title('Adam',fontsize = 20)
cm_display_1.im_.colorbar.remove()
cm_display_1.ax_.set_xlabel('')
cm_display_1.ax_.set_ylabel('True Label', fontsize = 20)

cm_display_2.plot(ax = axes[1])
cm_display_2.ax_.set_title('Adam l2',fontsize = 20)
cm_display_2.im_.colorbar.remove()
cm_display_2.ax_.set_xlabel('Predicted Label', fontsize = 20)
cm_display_2.ax_.set_ylabel('')

cm_display_3.plot(ax = axes[2])
cm_display_3.ax_.set_title('Adam dropout',fontsize = 20)
cm_display_3.im_.colorbar.remove()
cm_display_3.ax_.set_xlabel('')
cm_display_3.ax_.set_ylabel('')


plt.subplots_adjust(wspace=0.40, hspace=0.1)
f.colorbar(cm_display_3.im_, ax=axes)
plt.savefig('Confusion.png')
plt.show()
