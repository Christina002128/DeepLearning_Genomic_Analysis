# calculate Variable Importance (VI) score using best hyperparameter for each type of dataset
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
import time
import argparse
import random
from itertools import combinations
import csv
import os

real_start_time = time.time()
parser = argparse.ArgumentParser(description='input dataset file name') # creating an ArgumentParser object
parser.add_argument('file',action="store",help="file (1 to 1000).(H0/HA1/HA2)(N/W/S).csv")
args = parser.parse_args()
file=args.file.replace(" ","").replace(".csv","")

# in case of any rerun for paralell on eddie for 1000 files
#path="~/Documents/R_data/"
#file="1.HA2S"
# import os,sys
# if os.path.exists(file+"_pair_VI.csv"):
#     print(file+"_pair_VI.csv","exist.")
#     sys.exit()


# best performed parameters for each type of dataset
# # H0N
# conv,kernel,dense,learn,epoch,batch = [8, 5, 32, 0.0001, 50, 12]
# # H0W
# conv,kernel,dense,learn,epoch,batch = [8, 5, 32, 0.0001, 50, 12]
# # H0S
# conv,kernel,dense,learn,epoch,batch = [8, 5, 32, 0.0001, 50, 12]

# #HA1N
# conv,kernel,dense,learn,epoch,batch = [32, 10, 32, 0.0001, 50, 4]
# #HA1W
# conv,kernel,dense,learn,epoch,batch = [256, 5, 32, 0.0001, 50, 4]
# #HA1S 
# conv,kernel,dense,learn,epoch,batch = [500, 5, 32, 0.0001, 50, 4]

# #HA2N row 18 
# conv,kernel,dense,learn,epoch,batch = [64, 10, 32, 0.0001, 50, 4]
# #HA2W row 44 
#conv,kernel,dense,learn,epoch,batch = [256, 10, 32, 0.0001, 50, 4]
# #HA2S row 32
#conv,kernel,dense,learn,epoch,batch = [256, 5, 64, 0.0001, 50, 4]

print("para:")
print(conv,kernel,dense,learn,epoch,batch)

# import simulated data
data=pd.read_csv(file+".csv",delim_whitespace=False)
n,snp=data.shape
snp = snp-1
print(data.shape)
# split train and test set, 80% vs 20%
dataset=data.to_numpy()
X=dataset[:,0:snp]
y=dataset[:,snp]
X_train, X_test, y_train, y_test = train_test_split(X, y,train_size=0.8,random_state=1)
X_train2=np.reshape(X_train, (X_train.shape[0], snp, 1))
X_test2=np.reshape(X_test, (X_test.shape[0], snp, 1))

# Create and compile the CNN model with the current hyperparameters
model = Sequential()
model.add(Conv1D(filters=conv, kernel_size=kernel,strides=1, activation='relu', input_shape=(snp, 1)))
model.add(Flatten())
model.add(Dense(dense, activation='relu'))
model.add(Dense(dense, activation='relu'))
model.add(Dense(1))
# Compile the model
model.compile(optimizer=Adam(learning_rate=learn), loss='mean_squared_error')
# Train the model on the training data
model.fit(X_train2, y_train, epochs=epoch, batch_size=batch, verbose=0)
# test the model
predictions = model.predict(X_test2,verbose=0)

# Calculate mean absolute error (MAE)
ori_mae = mean_absolute_error(y_test, predictions)
# Calculate root mean squared error (RMSE)
ori_rmse = mean_squared_error(y_test, predictions, squared=False)
# Calculate Mean Absolute Percentage Error (MAPE)
ori_mape = mean_absolute_percentage_error(y_test, predictions)
# Calculate R-squared (R2)
ori_r2 = r2_score(y_test, predictions)
# Calculate explained variance (EV)
ori_ev = explained_variance_score(y_test, predictions)
# store results
result_dict={"MAE":ori_mae, "RMSE": ori_rmse, "MAPE": ori_mape, "R-squared": ori_r2, "EV": ori_ev}
print(result_dict)
with open(file+"_results.csv", 'w', newline='') as f:
    writer = csv.writer(f)
    for key, value in result_dict.items():
        writer.writerow([key, value])


# Initialize an array to store the importance scores
PVI_r2 = np.zeros(snp)
MVI_r2 = np.zeros(snp)

# Shuffle or ablate each feature column and compute the mae and mse
for feature in range(snp):
    # PVI permutation variable importance 
    # Make a copy of the original test data
    X_test2_shuffled = X_test2.copy()
    # Shuffle the feature column
    np.random.shuffle(X_test2_shuffled[:, feature])
    # Compute the prediction on shuffled data
    predictions = model.predict(X_test2_shuffled)
    shuffled_r2 = r2_score(y_test, predictions)
    # Compute the VI score as the difference in accuracies
    PVI_r2[feature] = ori_r2 - shuffled_r2
    
    # MVI microablation variable importance 
    # Create a copy of the original test data
    X_test2_ablated = X_test2.copy()
    # Set the feature column to zero
    X_test2_ablated[:, feature] = 0
    # Compute the prediction on shuffled data
    predictions = model.predict(X_test2_ablated)
    ablated_r2 = r2_score(y_test, predictions)
    # Compute the importance score as the difference in accuracies
    MVI_r2[feature] = ori_r2 - ablated_r2


# Normalize the importance scores
PVI_Nor_r2 = (PVI_r2-np.min(PVI_r2)) / (np.max(PVI_r2)-np.min(PVI_r2))
MVI_Nor_r2 = (MVI_r2-np.min(MVI_r2)) / (np.max(MVI_r2)-np.min(MVI_r2))

# store the importance scores for each feature
SNPs=list(range(1,snp+1))
VI_dict={'Feature':SNPs,'PVI':PVI_r2, 'MVI':MVI_r2,
"Normalised_PVI":PVI_Nor_r2,"Normalised_MVI":MVI_Nor_r2}
VI_df = pd.DataFrame(VI_dict)
# Sort the DataFrame by MVI column and then by PVI
sorted_VI_df = VI_df.sort_values(by=['MVI','PVI'],ascending=False)
# print result
print(sorted_VI_df.head(10))
VI_df.to_csv(file+"_VI.csv",sep='\t',index=False)


# for paired VI calculation (HA1)
a_list=list(range(snp))
a_list.remove(9)
a_list.remove(99)
a_list.remove(199)
a_list.remove(4)
index=random.sample(a_list, 10)
index.extend([9,99,199,4])
combos = list(combinations(index,2))
counts=len(combos)
print(counts)
PVI_pair_r2 = np.zeros(counts)
MVI_pair_r2 = np.zeros(counts)
i=0
for a,b in combos:
    # PVI
    X_test2_shuffled = X_test2.copy()
    # Shuffle the feature column
    np.random.shuffle(X_test2_shuffled[:, a])
    np.random.shuffle(X_test2_shuffled[:, b])
    # Compute the prediction on shuffled data
    predictions = model.predict(X_test2_shuffled)
    shuffled_r2 = r2_score(y_test, predictions)
    # Compute the importance score as the difference in accuracies
    PVI_pair_r2[i] = ori_r2 - shuffled_r2
    # MVI
    X_test2_ablated = X_test2.copy()
    # Set the feature column to zero
    X_test2_ablated[:, a] = 0
    X_test2_ablated[:, b] = 0
    # Compute the prediction on shuffled data
    predictions = model.predict(X_test2_ablated)
    ablated_r2 = r2_score(y_test, predictions)
    # Compute the importance score as the difference in accuracies
    MVI_pair_r2[i] = ori_r2 - ablated_r2  
    i=i+1

# paired VI
# Normalize the importance scores   ##### wrong!!!! corrected already in correct_nor.py
PVI_Nor_pair_r2 = (PVI_pair_r2-np.min(PVI_pair_r2)) / (np.max(PVI_pair_r2)-np.min(PVI_pair_r2))
MVI_Nor_pair_r2 = (MVI_pair_r2-np.min(MVI_pair_r2)) / (np.max(MVI_pair_r2)-np.min(MVI_pair_r2))

# output VI and pair_VI files
VI_pair_dict={'Indexes':combos,'PVI':PVI_pair_r2, 'MVI':MVI_pair_r2,
'Normalised_PVI':PVI_Nor_pair_r2, 'Normalised_MVI':MVI_Nor_pair_r2}
VI_pair_df = pd.DataFrame(VI_pair_dict)
sorted_pair_df = VI_pair_df.sort_values(by=['MVI','PVI'],ascending=False)
print(sorted_pair_df.head(10))
sorted_pair_df.to_csv(file+"_pair_VI.csv",sep='\t',index=False)
