# 29Jun
# simulate 9000 datasets in total
# specify the number of output file - nf (1-1000 for each type of dataset)
args <- commandArgs(trailingOnly = TRUE)
nf <- args[1]
# nf=3
error=0
sd=1
# 9 types of dataset
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
dataset_names=paste(nf,'.',class_names,'.csv',sep='')
# observations, SNPs, correlation levels, errors(noise)
n=400 # try 1000 and 500
snp=500
cor_W=0.4*n
cor_S=0.8*n
# V5 and V10 variables correlation function
correlation<-function(cor_data,corr,n){
  # randomly set values to be correlated 
  index2=sample(1:n,corr)
  for(i in 1:corr){
    cor_data$V5[index2[i]]=cor_data$V10[index2[i]]
  }
  # calculate correlation
  cormat <- cor(cor_data)
  # draw correlation plot
  #melted_cormat <- melt(cormat)
  #head(melted_cormat)
  #ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  # geom_tile()
  return(cor_data)
}

# randomly generate the SNPs data
data=matrix(nrow=n,ncol=snp)
data<-as.data.frame(data)
for(i in 1:snp){
  data[,i]=rbinom(n,1,0.5)
}

# randomly generate the correlated SNPs data
corW_data=correlation(data,cor_W,n)
corS_data=correlation(data,cor_S,n)

# H0
# y is random
# H0N 
y_H0 = rnorm(n,10,sd)
data$y=y_H0
write.csv(data,dataset_names[1],row.names=F)
# check the data file:
# data1<-as.data.frame(read.csv("1.H0N.csv"))
# view(data1)
# H0W
corW_data$y=y_H0
write.csv(corW_data,dataset_names[2],row.names=F)
# H0S
corS_data$y=y_H0
write.csv(corS_data,dataset_names[3],row.names=F)

# HA1
# y=sum(pi*Xi) + e, 3 variables associated with y
y_HA1=5*data[,10]+ 5*data[,100] + 3*data[,200]+ rnorm(n,error,sd)
data$y=y_HA1
# HA1N
write.csv(data,dataset_names[4],row.names=F)
# HA1W
y_HA1W=5*corW_data[,10]+ 5*corW_data[,100] + 3*corW_data[,200]+ rnorm(n,error,sd)
corW_data$y=y_HA1W
write.csv(corW_data,dataset_names[5],row.names=F)
# HA1S
y_HA1S=5*corS_data[,10]+ 5*corS_data[,100] + 3*corS_data[,200]+ rnorm(n,error,sd)
corS_data$y=y_HA1S
write.csv(corS_data,dataset_names[6],row.names=F)


# HA2
# HA2N
#y=a + b + a*b + e
y_HA2=3*data[,10]+3*data[,100]+3*data[,10]*data[,100] + rnorm(n,error,sd)
data$y=y_HA2
write.csv(data,dataset_names[7],row.names=F)

# HA2W
y_HA2W=3*corW_data[,10]+3*corW_data[,100]+3*corW_data[,10]*corW_data[,100] + rnorm(n,error,sd)
corW_data$y=y_HA2W
write.csv(corW_data,dataset_names[8],row.names=F)

# HA2S
y_HA2S=3*corS_data[,10]+3*corS_data[,100]+3*corS_data[,10]*corS_data[,100] + rnorm(n,error,sd)
corS_data$y=y_HA2S
write.csv(corS_data,dataset_names[9],row.names=F)

