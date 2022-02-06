Files related to Trading Project

This is a personal project of mine for Cryptocurreny price prediction for use in trading. Coding is done in Python.
Included within this repo:

- CNNLSTM Crypto Trading Research Paper.pdf -> Research Paper looking at accuracy and trading performance of CNN, LSTM and CNNLSTM models.
- BinanceProject.py -> Main Script that runs all the code
- BinanceDataConcat.py -> Script to concat data into master dataframe after new data is downloaded from the binance api
- Models.py -> Script containing all the models used (CNN, LSTM, CNNLSTM, MLP, RF, XGBoost)
- DataFuncs.py -> Script containing all functions that clean and split data
- ensemblefuncs.py -> Script for ensemble functions created from scratch (soft/hard voting, stacking)
- testfuncs.py -> Script for functions to test model performance (Accuracy and other metrics, trading performance on test set with variable commission sizes)

Note 1: I have experimented/continue to experiment with custom loss functions and custom neural network architectures however I have omitted this code as it is mine >:) I am open to talk about my future plans for the project though.

Note 2: If you would like to run the code the data is in the google drive below (was too large to upload to github):

https://drive.google.com/drive/folders/1PPVu2xVO_qJDBqUgz0lmO6SbnxXXJnOH?usp=sharing
