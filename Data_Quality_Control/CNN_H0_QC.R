# use linear model to test the dataset H0
library(dplyr)
# n - number of files, paras - number of parameters, cor - correlation between 5 and 10
n = 500
# Get the list of files ending with "data.csv"
args <- commandArgs(trailingOnly = TRUE)
file_name <- args[1]
cor <- args[2]
cor <- as.numeric(cor)
#cor=0
#file_name="H0W.csv"
file_list <- list.files(pattern = file_name)
n=length(file_list)
act_para=c(cor,0,0)
cat(n,"files in total ends with",file_name,"\n")
# Create empty lists to store parameter values
correlations <- vector("numeric")
coverages <- vector("numeric")
# Loop over the files
for (file in file_list) {
  data <- as.data.frame(read.csv(file))

  corr <- c(cor(data[,5],data[,10]),cor(data[,5],data[,100]),
            cor(data[,10],data[,100]))
  correlations <- rbind(correlations,corr)
}

# create result table
result<-correlations
colnames(result)<-c("cor(v5,v10)","cor(v5,v100)","cor(v10,v100)")
rownames(result)<-c(1:n)
result_file=paste(substr(file_name, start = 1, stop = 4),"_QC.csv",sep="")
write.csv(result,result_file,row.names=F)

# bias
para=colMeans(result)
cat("mean:",round(para,4),"\n")
bias = round(para-act_para,4)
cat("bias:",bias,"\n")

final_result <- bias
print(final_result)

final_file=paste(substr(file_name, start = 1, stop = 3),"_QC_final.csv",sep="")
write.csv(final_result,final_file,row.names=F)

