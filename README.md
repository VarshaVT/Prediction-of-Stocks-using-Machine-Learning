# Prediction-of-Stocks-using-Machine-Learning
It's a machine learning challenge given by Correlation One Company

Given the stocks dataset that consists of the (simulated) daily open-toclose changes of 
a set of 10  stocks: S1, S2, …, S10. 
Stock S1 trades in the U.S. as part of the S&P 500 Index, while stocks S2, S3, …,  S10 trade in Japan, 
as part of the Nikkei Index.   

The task is to build a model to forecast S1, as a function of S1, S2, …, S10 .  
You should build your model using the first 50 rows of the dataset.  
The remaining 50 rows of the dataset have values for S1  missing: 
you will use your model to fill these in.  

This project contains following files:
1. MachineLearningTest.pdf = This file includes the objective and questions related to this project
2. stock_returns.csv = This is a data file which includes 100 rows (50 train, 50 test) of stocks data.
3. MC.R = This file includes all the code related to this project in R Statistical language.
4. Machine Learning Challenge - Correlation One Final.pdf = This file contains answer to the test questions.
5. BestModel_KnnRegPrediction.csv = This is the prediction of stocks using KNN Regression Model.
6. AllModelPredictionWithDate.csv = This file has predictions of all other models with respect to date.
