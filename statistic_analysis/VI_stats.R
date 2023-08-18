# n - number of files, paras - number of parameters, cor - correlation between 5 and 10
n = 500
# Get the list of files ending with "data.csv"
args <- commandArgs(trailingOnly = TRUE)
file_name <- args[1]
#file_name="HA1W"
result_file=paste(file_name,"_results.csv",sep="")
VI_file=paste(file_name,"_VI.csv",sep="")
pair_VI_file=paste(file_name,"_pair_VI.csv",sep="")

result_list <- list.files(pattern = result_file)
VI_list <- list.files(pattern = VI_file)
pair_VI_list <- list.files(pattern = pair_VI_file)

n=length(result_list)
cat(n,"files in total ends with",result_file,"\n")

coverages <- vector("numeric")
# Loop over the files
for (file in result_list) {
  data <- as.data.frame(read.csv(file))
  # Fit the linear model
  model = lm(y~data[,10]+data[,100]+data[,200],data)
  lm_para=as.data.frame(model$coefficients)[c("data[, 10]","data[, 100]","data[, 200]"),]
  # Append the parameter values to the lists
  intercepts <- c(intercepts, as.numeric(model$coefficients[1]))
  coefficients <- rbind(coefficients, lm_para)
  corr <- c(cor(data[,5],data[,10]),cor(data[,5],data[,100]),
            cor(data[,10],data[,100]))
  correlations <- rbind(correlations,corr)
  # coverage
  coverages <- cbind(coverages,confint(model))
}
