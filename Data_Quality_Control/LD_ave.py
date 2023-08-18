# Quality check for correlation rate of V5 and V10
# LD matrix average result for each type of dataset
# LD_ave.py
import pandas as pd
corla=["N","W","S"]
for f in corla:
    file_list = [str(i+1)+".H0"+f+"_LD.csv" for i in range(1000)]
    data_frames = [pd.read_csv(file) for file in file_list]
    
    concatenated_df=pd.concat(data_frames)
    matrix = pd.read_csv("1.H0"+f+"_LD.csv")
    for i in range(0,500):
        matrix.loc[i] = concatenated_df.loc[i].mean()
    matrix.to_csv("H0"+f+"_ave_LD.csv",sep='\t',index=False)

