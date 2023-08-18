# use linear model to test the dataset HA2
library(dplyr)
# n - number of files, paras - number of parameters, cor - correlation between 5 and 10
n = 500
# paras = 3
# Get the list of files ending with "data.csv"
args <- commandArgs(trailingOnly = TRUE)
file_name <- args[1]
cor <- args[2]
cor <- as.numeric(cor)
#cor=0
#file_name="HA2N.csv"
file_list <- list.files(pattern = file_name)
n=length(file_list)
act_para=c(0,5,5,10,cor,0,0)
cat(n,"files in total ends with",file_name,"\n")

# Create empty lists to store parameter values
intercepts <- vector("numeric")
coefficients <- vector("numeric")
correlations <- vector("numeric")
coverages <- vector("numeric")
# Loop over the files
for (file in file_list) {
  data <- as.data.frame(read.csv(file))
  # Fit the linear model
  model = lm(y~data[,10]*data[,100],data)
  lm_para=as.data.frame(model$coefficients)[c("data[, 10]","data[, 100]","data[, 10]:data[, 100]"),]
  # Append the parameter values to the lists
  intercepts <- c(intercepts, as.numeric(model$coefficients[1]))
  coefficients <- rbind(coefficients, lm_para)
  corr <- c(cor(data[,5],data[,10]),cor(data[,5],data[,100]),
            cor(data[,10],data[,100]))
  correlations <- rbind(correlations,corr)
  # coverage
  coverages <- cbind(coverages,confint(model))
}

#coverage check
ins <- vector("numeric")
covs <- vector("numeric")
for ( i in 1:n){
  ins[1] <- (act_para[1] >= coverages[1, 2*i-1] && act_para[1] <= coverages[1, 2*i])
  ins[2] <- (act_para[2] >= coverages[2, 2*i-1] && act_para[2] <= coverages[2, 2*i])
  ins[3] <- (act_para[3] >= coverages[3, 2*i-1] && act_para[3] <= coverages[3, 2*i])
  ins[4] <- (act_para[4] >= coverages[4, 2*i-1] && act_para[4] <= coverages[4, 2*i])
  covs <- cbind(covs,ins)
}
rownames(covs)<-c("intercept","data[, 10]","data[, 100]","data[, 200]")
colnames(covs)<-c(1:n)
coverage_result <- vector("numeric")
coverage_result[1] <- length(covs[1,covs[1,]==1])/length(covs[1,])
coverage_result[2] <- length(covs[2,covs[2,]==1])/length(covs[2,])
coverage_result[3] <- length(covs[3,covs[3,]==1])/length(covs[3,])
coverage_result[4] <- length(covs[4,covs[4,]==1])/length(covs[4,])
cat("coverage:",coverage_result,"\n")
names(coverage_result)<-c("cover_intercept","cover_beta10","cover_beta100","cover_beta10*100")

# create result table
result<-cbind(intercepts,coefficients,correlations)
colnames(result)<-c("intercept","beta10","beta100","beta10*100",
                    "cor(v5,v10)","cor(v5,v100)","cor(v10,v100)")
rownames(result)<-c(1:n)
result_file=paste(substr(file_name, start = 1, stop = 4),"_QC.csv",sep="")
write.csv(result,result_file,row.names=F)


# bias
para=colMeans(result)
cat("mean:",round(para,4),"\n")
bias = round(para-act_para,4)
cat("bias:",bias,"\n")

final_result=c(bias,round(coverage_result,4))
print(final_result)

final_file=paste(substr(file_name, start = 1, stop = 4),"_QC_final.csv",sep="")
write.csv(final_result,final_file,row.names=F)



