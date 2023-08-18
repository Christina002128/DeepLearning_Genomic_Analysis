# calculate average of VI scores of 1000 replicates in each data type and store the results of all replicates
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import os,subprocess
import argparse
import random
# input dataset type
parser = argparse.ArgumentParser(description='input dataset file name')
parser.add_argument('file',action="store",help="(H0/HA1/HA2)(N/W/S)")
args = parser.parse_args()
file=args.file.replace(" ","").replace(".csv","")

#average of performance
perform=["MAE","RMSE","R-squared","EV"]
perform_val=[]
perform_std=[]
a_df = pd.DataFrame(columns=perform, index=range(1000))
for p in perform:
    subprocess.run("grep '"+p+"' *."+file+"_results.csv > "+file+"_"+p+".txt", shell=True)
    perform_df=pd.read_csv(file+"_"+p+".txt",header=None,delimiter=",")
    a_df[p]=perform_df[1]
    perform_val.append(perform_df[1].mean())
    perform_std.append(perform_df[1].std())
evaluate_df = pd.DataFrame({
    'name': perform,
    'mean': perform_val,
    'std': perform_std
})
print(evaluate_df)
evaluate_df.to_csv(file+"_evaluate_stats.csv",sep='\t',index=False)
a_df.to_csv(file+"_evaluate_results.csv",sep='\t',index=False)


# VI of variable 5, 10, 100, 200 and 3 not associated variables: 300,400,500
subprocess.run("awk 'FNR == 6' *."+file+"_VI.csv > "+file+"_5VI.txt", shell=True)
subprocess.run("awk 'FNR == 11' *."+file+"_VI.csv > "+file+"_10VI.txt", shell=True)
subprocess.run("awk 'FNR == 101' *."+file+"_VI.csv > "+file+"_100VI.txt", shell=True)
subprocess.run("awk 'FNR == 201' *."+file+"_VI.csv > "+file+"_200VI.txt", shell=True)

subprocess.run("awk 'FNR == 301' *."+file+"_VI.csv > "+file+"_300VI.txt", shell=True)
subprocess.run("awk 'FNR == 401' *."+file+"_VI.csv > "+file+"_400VI.txt", shell=True)
subprocess.run("awk 'FNR == 501' *."+file+"_VI.csv > "+file+"_500VI.txt", shell=True)



# Paired VI of (5,10),(5,100),(5,200),(10,100),(10,200),(100,200),and 3 random pairs: 'r1','r2','r3'
subprocess.run("grep '(9, 4)' *."+file+"_pair_VI.csv > "+file+"_5_10VI.txt", shell=True)
subprocess.run("grep '(99, 4)' *."+file+"_pair_VI.csv > "+file+"_5_100VI.txt", shell=True)
subprocess.run("grep '(199, 4)' *."+file+"_pair_VI.csv > "+file+"_5_200VI.txt", shell=True)
subprocess.run("grep '(9, 99)' *."+file+"_pair_VI.csv > "+file+"_10_100VI.txt", shell=True)
subprocess.run("grep '(9, 199)' *."+file+"_pair_VI.csv > "+file+"_10_200VI.txt", shell=True)
subprocess.run("grep '(99, 199)' *."+file+"_pair_VI.csv > "+file+"_100_200VI.txt", shell=True)

subprocess.run("awk 'FNR == 40' *."+file+"_pair_VI.csv > "+file+"_r1VI.txt", shell=True)
subprocess.run("awk 'FNR == 60' *."+file+"_pair_VI.csv > "+file+"_r2VI.txt", shell=True)
subprocess.run("awk 'FNR == 80' *."+file+"_pair_VI.csv > "+file+"_r3VI.txt", shell=True)

# dataframe for VI
vlist=[5,10,100,200,300,400,500,(5,10),(5,100),(5,200),(10,100),(10,200),(100,200),'r1','r2','r3']
file_list=[file+"_5VI.txt",file+"_10VI.txt",file+"_100VI.txt",file+"_200VI.txt",
    file+"_300VI.txt",file+"_400VI.txt",file+"_500VI.txt",
    file+"_5_10VI.txt",file+"_5_100VI.txt",file+"_5_200VI.txt",file+"_10_100VI.txt",file+"_10_200VI.txt",file+"_100_200VI.txt",
    file+"_r1VI.txt",file+"_r2VI.txt",file+"_r3VI.txt"]

# read files and store values into dataframe PVI, MVI, nor_PVI and nor_MVI
PVI_df = pd.DataFrame(columns=vlist, index=range(1000))
MVI_df = pd.DataFrame(columns=vlist, index=range(1000))
nor_PVI_df = pd.DataFrame(columns=vlist, index=range(1000))
nor_MVI_df = pd.DataFrame(columns=vlist, index=range(1000))
for i in range(len(file_list)):
    df=pd.read_csv(file_list[i],header=None,delimiter="\t")
    if df.shape[0]<1000: 
        print("error!",file_list[i],"has only",df.shape[0],"rows!")
    PVI_df[vlist[i]]=df[1]
    MVI_df[vlist[i]]=df[2]
    nor_PVI_df[vlist[i]]=df[3]
    nor_MVI_df[vlist[i]]=df[4]

# output files
PVI_df.to_csv(file+"_PVI.csv",sep='\t',index=False)
MVI_df.to_csv(file+"_MVI.csv",sep='\t',index=False)
nor_PVI_df.to_csv(file+"_nor_PVI.csv",sep='\t',index=False)
nor_MVI_df.to_csv(file+"_nor_MVI.csv",sep='\t',index=False)



