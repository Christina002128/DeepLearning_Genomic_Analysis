# generate file to store all the combinations of candidate hyperparameters for the model

paras = {
    "conv_values": [8, 16, 32, 256],
    "kernel_values": [2, 5, 10],
    #"pool_values": [2, 3],
    "dense_values": [8, 32, 64],
    "learn_values": [0.001,0.0001],
    "epoch_values": [50, 70],
    "batch_values": [4, 12]
}
import pickle
import itertools
para_alls = list(itertools.product(*paras.values()))
print(len(para_alls))
for i in range(len(para_alls)):
    file=str(i+1)+'_paras.pkl'
    with open(file, 'wb') as f:
        pickle.dump(para_alls[i], f)

