# 10-fold cross validation for finding best hyperparameter for each type of dataset
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
import tensorflow.keras as keras
from keras.optimizers import Adam
from keras.models import Sequential
from keras.layers import Conv1D, MaxPooling1D, Flatten, Dense, Dropout
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score, mean_absolute_percentage_error, explained_variance_score
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold
import time
import argparse
import random
from itertools import combinations

# time counting
real_start_time = time.time()
# input data file name (1000 files for each type, run in parallel on eddie)
parser = argparse.ArgumentParser(description='input dataset file name') 
parser.add_argument('file',action="store",help="file (1 to 1000).(H0/HA1/HA2)(N/W/S).csv")
args = parser.parse_args()
file=args.file.replace(" ","").replace(".csv","")

#check of the file exist, if exist then exit (in case some files fail of generating output)
# import os,sys
# if os.path.exists(file+"_best_para_10fold_result.txt"):
#     print(file+"_best_para_10fold_result.txt","exist.")
#     sys.exit()

# build model
def build_model(conv,kernel,dense,learn,snp):
    model = Sequential()
    model.add(Conv1D(filters=conv, kernel_size=kernel,strides=1, activation='relu', input_shape=(snp, 1)))
    model.add(Flatten())
    model.add(Dense(dense, activation='relu'))
    model.add(Dense(dense, activation='relu'))
    model.add(Dense(1))
    model.compile(optimizer=Adam(learning_rate=learn), loss='mean_squared_error')
    return model

# file="1.HA1N"
# import simulated data (binary for classification)
data=pd.read_csv(file+".csv",delim_whitespace=False)
n,snp=data.shape
snp = snp-1
print(data.shape)
# split train and test set, 80% vs 20%
dataset=data.to_numpy()
X=dataset[:,0:snp]
y=dataset[:,snp]
X_train, X_test, y_train, y_test = train_test_split(X, y,train_size=0.8,random_state=1)
# number of feasures
print(X_train.shape)
# 10-fold cross-validation
# Define the hyperparameters to tune
# Read the CSV file of combinations of hyperparameter
df = pd.read_csv("combinations.csv")
# Display the contents of the DataFrame
print(df)
best_r2=-2
avg_r2=[]
best_para=[]
# run through all combinations of parameters
for i in range(df.shape[0]):
    conv,kernel,dense,learn,epoch,batch = [round(value) for value in list(df.iloc[i])]
    learn=list(df.loc[i])[3]
    # Initialize variables to store the best hyperparameters and accuracy
    r2s=[]
    # Perform 5-fold cross-validation on the training dataset
    kf = KFold(n_splits=10, shuffle=True, random_state=42)
    start_time = time.time()
    for tr_index, val_index in kf.split(X_train):
        # Create and compile the CNN model with the current hyperparameters
        model = build_model(conv,kernel,dense,learn,snp)
        # Split the data into training and validation sets for the current fold
        X_tr, X_val = X_train[tr_index], X_train[val_index]
        y_tr, y_val = y_train[tr_index], y_train[val_index]
        # reshape validation dataset
        X_tr2=np.reshape(X_tr, (X_tr.shape[0], snp, 1))
        X_val2=np.reshape(X_val, (X_val.shape[0], snp, 1))
        # Train the model on the training data
        model.fit(X_tr2, y_tr, epochs=epoch, batch_size=batch, verbose=0)
        # test the model
        predictions = model.predict(X_val2,verbose=0)
        # Evaluate the model on the validation data
        r2s.append(r2_score(y_val, predictions))
    # Compute the average result across all folds
    avg_r2.append(np.round(np.mean(r2s),4))
    print("row:",i,"r2:",avg_r2[-1],"para",list(df.iloc[i]))
    if avg_r2[-1] > best_r2:
        best_r2=avg_r2[-1]
        best_para=[conv,kernel,dense,learn,epoch,batch]

# best hyperparameter of this dataset
print("best r2:",best_r2)
print("best para:",best_para)
print('total runtime:',time.time() - real_start_time)

# store results of each hyperparameter
df["kFold_r2"]=avg_r2
df_sort = df.sort_values(by=["kFold_r2"],ascending=False)
df_sort.to_csv(file+"_10fold_result.csv", index=False)

# output result of best hyperparameter
best_dict={"para":best_para,"r2":best_r2}
best_df = pd.DataFrame(best_dict, columns=best_dict.keys())
best_df.to_csv(file+"_best_para_10fold_result.txt", index=False)





