# quality control, check coverage and bias of each type of simulated dataset
file1=c("HA1N_QC.csv","HA1W_QC.csv","HA1S_QC.csv","HA2N_QC.csv","HA2W_QC.csv","HA2S_QC.csv")
file2=c("HA1N_QC_final.csv","HA1W_QC_final.csv","HA1S_QC_final.csv",
       "HA2N_QC_final.csv","HA2W_QC_final.csv","HA2S_QC_final.csv")

# HA1 bias
biases1 <- as.data.frame(read.csv(file1[1]))
biases2 <- as.data.frame(read.csv(file1[2]))
biases3 <- as.data.frame(read.csv(file1[3]))
file_name=substr(file1[1], start = 1, stop = 3)
# 10
beta10 = cbind(biases1[,2],biases2[,2],biases3[,2])
beta10= as.data.frame(beta10)
colnames(beta10)=c("HA1N","HA1W","HA1S")

hist1_png=paste(file_name,"_hist10.png",sep="")
png(hist1_png,width = 400,height = 400)
plot(density(beta10$HA1N), col = "blue", xlim = c(min(beta10$HA1N, beta10$HA1W, beta10$HA1S), max(beta10$HA1N, beta10$HA1W, beta10$HA1S)),
     ylim = c(0, 4.3),
    main = "Distribution of Coefficient of X10 in HA1 Datasets", xlab = "Coefficient of X10", ylab = "Density")
lines(density(beta10$HA1W), col = "red")
lines(density(beta10$HA1S), col = "green")
abline(v=mean(colMeans(beta10)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA1N","HA1W","HA1S"), col = c("blue", "red", "green"), lty = 1)
dev.off()
mean(colMeans(beta10))
sd(c(beta10$HA1N,beta10$HA1S,beta10$HA1W))
# 5.0±0.1

# 100
beta100 = cbind(biases1[,3],biases2[,3],biases3[,3])
beta100= as.data.frame(beta100)
colnames(beta100)=c("HA1N","HA1W","HA1S")
hist2_png=paste(file_name,"_hist100.png",sep="")
png(hist2_png,width = 400,height = 400)
plot(density(beta100$HA1N), col = "blue", xlim = c(min(beta100$HA1N, beta100$HA1W, beta100$HA1S), max(beta100$HA1N, beta100$HA1W, beta100$HA1S)),
     ylim = c(0, 4.3),
    main = "Distribution of Coefficient of X100 in HA1 Datasets", xlab = "Coefficient of X100", ylab = "Density")
lines(density(beta100$HA1W), col = "red")
lines(density(beta100$HA1S), col = "green")
abline(v=mean(colMeans(beta100)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA1N","HA1W","HA1S"), col = c("blue", "red", "green"), lty = 1)
dev.off()
mean(colMeans(beta100))
sd(c(beta100$HA1N,beta100$HA1S,beta100$HA1W))

# 5.0±0.1

# 200
beta200 = cbind(biases1[,4],biases2[,4],biases3[,4])
beta200= as.data.frame(beta200)
colnames(beta200)=c("HA1N","HA1W","HA1S")
hist3_png=paste(file_name,"_hist200.png",sep="")
png(hist3_png,width = 400,height = 400)
plot(density(beta200$HA1N), col = "blue", xlim = c(min(beta200$HA1N, beta200$HA1W, beta200$HA1S), max(beta200$HA1N, beta200$HA1W, beta200$HA1S)),
     ylim = c(0, 4.3),
     main = "Distribution of Coefficient of X200 in HA1 Datasets", xlab = "Coefficient of X200", ylab = "Density")
lines(density(beta200$HA1W), col = "red")
lines(density(beta200$HA1S), col = "green")
abline(v=mean(colMeans(beta200)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA1N","HA1W","HA1S"), col = c("blue", "red", "green"), lty = 1)
dev.off()

mean(colMeans(beta200))
sd(c(beta200$HA1N,beta200$HA1S,beta200$HA1W))

# 3.0±0.1

##############################################
# HA2 bias
biases1 <- as.data.frame(read.csv(file1[4]))
biases2 <- as.data.frame(read.csv(file1[5]))
biases3 <- as.data.frame(read.csv(file1[6]))
file_name=substr(file1[4], start = 1, stop = 3)

beta10 = cbind(biases1[,2],biases2[,2],biases3[,2])
beta10= as.data.frame(beta10)
colnames(beta10)=c("HA2N","HA2W","HA2S")
hist1_png=paste(file_name,"_hist10.png",sep="")
png(hist1_png,width = 400,height = 400)
plot(density(beta10$HA2N), col = "blue", xlim = c(min(beta10$HA2N, beta10$HA2W, beta10$HA2S), max(beta10$HA2N, beta10$HA2W, beta10$HA2S)),
     ylim = c(0, 3), 
     main = "Distribution of Coefficient of X10 in HA2 Datasets", xlab = "Coefficient of X10", ylab = "Density")
lines(density(beta10$HA2W), col = "red")
lines(density(beta10$HA2S), col = "green")
abline(v=mean(colMeans(beta10)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA2N","HA2W","HA2S"), col = c("blue", "red", "green"), lty = 1)
dev.off()

mean(colMeans(beta10))
sd(c(beta10$HA2N,beta10$HA2S,beta10$HA2W))
# 5.0±0.1

beta100 = cbind(biases1[,3],biases2[,3],biases3[,3])
beta100= as.data.frame(beta100)
colnames(beta100)=c("HA2N","HA2W","HA2S")
hist2_png=paste(file_name,"_hist100.png",sep="")
png(hist2_png,width = 400,height = 400)
plot(density(beta100$HA2N), col = "blue", xlim = c(min(beta100$HA2N, beta100$HA2W, beta100$ZS), max(beta100$HA2N, beta100$HA2W, beta100$HA2S)),
     ylim = c(0, 3),
     main = "Distribution of Coefficient of X100 in HA2 Datasets", xlab = "Coefficient of X100", ylab = "Density")
lines(density(beta100$HA2W), col = "red")
lines(density(beta100$HA2S), col = "green")
abline(v=mean(colMeans(beta100)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA2N","HA2W","HA2S"), col = c("blue", "red", "green"), lty = 1)
dev.off()
mean(colMeans(beta100))
sd(c(beta100$HA2N,beta100$HA2S,beta100$HA2W))
# 5.0±0.1

beta10_100 = cbind(biases1[,4],biases2[,4],biases3[,4])
beta10_100= as.data.frame(beta10_100)
colnames(beta10_100)=c("HA2N","HA2W","HA2S")
hist3_png=paste(file_name,"_hist10_100.png",sep="")
png(hist3_png,width = 400,height = 400)
plot(density(beta10_100$HA2N), col = "blue", xlim = c(min(beta10_100$HA2N, beta10_100$HA2W, beta10_100$HA2S), max(beta10_100$HA2N, beta10_100$HA2W, beta10_100$HA2S)),
     ylim = c(0, 3),
     main = "Distribution of Coefficient of X10*X100 in HA2 Datasets", xlab = "Coefficient of X10*X100", ylab = "Density")
lines(density(beta10_100$HA2W), col = "red")
lines(density(beta10_100$HA2S), col = "green")
abline(v=mean(colMeans(beta10_100)),col="black",lty="dashed")
# Add a legend
legend("topright", legend = c("HA2N","HA2W","HA2S"), col = c("blue", "red", "green"), lty = 1)
dev.off()
mean(colMeans(beta10_100))
sd(c(beta10_100$HA2N,beta10_100$HA2S,beta10_100$HA2W))
# 3.0±0.2

res=as.data.frame(read.csv("HA_QC_combined.csv",sep=","))
res[c(9,10,11),c(2:7)]=res[c(9,10,11),c(2:7)]*100
write.csv(res,"QC_combind2.csv",row.names=F,sep="\t")

#mean adn sd of all coverage
lists=c(as.numeric(res[9,c(2:7)]),as.numeric(res[10,c(2:7)]),as.numeric(res[11,c(2:7)]))
mean(lists)
sd(lists)



# LD plot
library(corrplot)
LD_file = paste("res_20July/",c("H0N","H0W","H0S"),"_ave_LD.csv",sep="")
cors=c("No","Weak","Strong")
for(i in 1:3){
xx=read.csv(LD_file[i],sep="\t")
rownames(xx)<- paste("X",c(1:500),sep="")
colnames(xx)<- paste("X",c(1:500),sep="")
ld_matrix=as.matrix(xx)
# Create the correlation plot
png(paste(LD_file[i],"_LD2.png",sep=""),height = 450,width = 450)
corrplot(ld_matrix[c(1:20),c(1:20)], method = "number", type = "upper", tl.srt=45,sig.level="label_sig",
         tl.cex = 0.8, tl.col = "black")
title(paste("LD Plot of Datasets with ",cors[i]," Correlation",sep=""), line = 3)

dev.off()
png(paste(LD_file[i],"_LD1.png",sep=""),height = 450,width = 450)
corrplot(ld_matrix, method = "color", type = "upper", 
         tl.cex = 0.2, tl.col = "black")
title(paste("Full LD Plot of Datasets with ",cors[i]," Correlation",sep=""), line = 3)
dev.off()
}

biases =NULL
for (i in 1:6){
  biases <- cbind(biases, as.data.frame(read.csv(file2[1]))[2:4,1])
}
max( abs(biases))
options(scipen = 999)
print(min( abs(biases)))
options(scipen = 0)

coverages =NULL
for (i in 1:6){
  coverages <- cbind(coverages, as.data.frame(read.csv(file2[1]))[9:11,1])
}
mean(coverages)
sd(coverages)
