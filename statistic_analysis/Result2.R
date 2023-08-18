#VI results
path="res_20July/"
type="_PVI.csv"
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
VI_file=paste(path,class_names,type,sep='')
VI_list=list()
for(i in 1:9){
  VI_list[[class_names[i]]]=as.data.frame(read.csv(VI_file[i],sep="\t",header = T))
}
for(i in 7:9){
  VI_list[[class_names[i]]]=as.data.frame(read.csv(paste("res_1Aug/",class_names[i],type,sep=""),sep="\t",header = T))
}


##### H0
# single PVI
png("H0sPVI.png")
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(VI_list[[class_names[i]]][1:7], names =colnames(VI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="PVI Scores", 
        main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.08,0.05))
axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
}
dev.off()
# pair PVI
pairname=c("X5_X10","X5_X100","X5_X200","X10_X100","X10_X200","X100_X200","random_1","random_2","random_3")

png("H0pPVI.png",height=480,width=520)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(VI_list[[class_names[i]]][8:16], names = pairname, xlab="Variables", ylab="PVI Scores", 
          main = paste("Paired PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.08,0.08))
  axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
}
dev.off()

##### HA1
# single PVI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
for(i in 4:6){
  boxplot(VI_list[[class_names[i]]][1:7], names =colnames(VI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="PVI Scores", 
          main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
dev.off()
# paired PVI
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))
for(i in 4:6){
  boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][2],VI_list[[class_names[i]]][8]),
          names =c("additive_PVI","paired_PVI"),xlab=class_names[i], ylab="PVI Scores", ylim=c(0.15,0.78),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
mtext("Comparison of Additive PVIs and Paired PVI of X5 and X10", side = 3, line =-2, outer = T)


##### HA2
# single PVI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
for(i in 7:9){
  boxplot(VI_list[[class_names[i]]][c(c(1:3),c(5:7))], names =colnames(VI_list[[class_names[i]]])[c(c(1:3),c(5:7))],xlab="Variables", ylab="PVI Scores", 
          main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
dev.off()

# paired PVI
# single additive vs paired VI 5+10, 10+100, 5+100
# do normalised or not normalised ??
par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))
for(i in 7:9){
boxplot(cbind(VI_list[[class_names[i]]][2]+VI_list[[class_names[i]]][3],VI_list[[class_names[i]]][11]),
        names =c("additive_PVI","paired_PVI"),xlab=class_names[i], ylab="PVI Scores", ylim=c(0.4,1.42),
        cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
mtext("Comparison of Additive PVIs and Paired PVI of X10 and X100", side = 3, line =-2, outer = T)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))
for(i in 7:9){
boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][2],VI_list[[class_names[i]]][8]),
        names =c("additive_PVI","paired_PVI"),xlab=class_names[i], ylab="PVI Scores", ylim=c(0.1,0.82),
        main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
mtext("Comparison of Additive PVIs and Paired PVI of X5 and X10", side = 3, line =-2, outer = T)

for(i in 7:9){
  boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][3],VI_list[[class_names[i]]][9]),
          names =c("additive_PVI","paired_PVI"),xlab=class_names[i], ylab="PVI Scores",ylim=c(0.17,0.92),
          main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
mtext("Comparison of Additive PVIs and Paired PVI of X5 and X100", side = 3, line =-2, outer = T)

# test
mean(VI_list[[class_names[8]]]$X10+VI_list[[class_names[8]]]$X100)
mean(VI_list[[class_names[8]]]$X.10..100.)
t.test(VI_list[[class_names[8]]]$X100+VI_list[[class_names[8]]]$X10,VI_list[[class_names[8]]]$X.10..100., alternative = "two.sided",paired = T,var.equal=T)

mean(VI_list[[class_names[5]]]$X5+VI_list[[class_names[5]]]$X100)
mean(VI_list[[class_names[5]]]$X.5..100.)
t.test(VI_list[[class_names[5]]]$X5+VI_list[[class_names[5]]]$X100,VI_list[[class_names[5]]]$X.5..100., alternative = "two.sided",paired = T,var.equal=T)

# average of all other paired VI results





# useless!!
# single VI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
boxplot(VI_list[[class_names[1]]][1:7], names = colnames(VI_list[[class_names[1]]])[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA1N Datasets")
boxplot(VI_list[[class_names[2]]][1:7], names = colnames(VI_list[[class_names[2]]])[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA1W Datasets")
boxplot(VI_list[[class_names[3]]][1:7], names = colnames(H0S)[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA1S Datasets")
# pair VI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
boxplot(HA1N[,c(8:16)], names = colnames(VI_list[[class_names[1]]])[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA1N Datasets")
boxplot(HA1W[,c(8:16)], names = colnames(VI_list[[class_names[2]]])[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA1W Datasets")
boxplot(HA1S[,c(8:16)], names = colnames(H0S)[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA1S Datasets")

# HA2
# single VI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
boxplot(VI_list[[class_names[i]]], names = colnames(VI_list[[class_names[1]]])[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA2N")
boxplot(VI_list[[class_names[i]]], names = colnames(VI_list[[class_names[2]]])[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA2W")
boxplot(VI_list[[class_names[i]]], names = colnames(H0S)[1:7],xlab="Variables", ylab="PVI Scores", main = "PVI Scores in HA2S")
# pair VI
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
boxplot(HA2N[,c(8:16)], names = colnames(VI_list[[class_names[1]]])[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA2N")
boxplot(HA2W[,c(8:16)], names = colnames(VI_list[[class_names[2]]])[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA2W")
boxplot(HA2S[,c(8:16)], names = colnames(H0S)[8:16],xlab="Variables", ylab="PVI Scores", main = "Paired PVI Scores in HA2S")


# HA1 5VI and 10VI
H1_X5X10 = cbind(HA1N[,1],HA1W[,1],HA1S[,1],HA1N[,2],HA1W[,2],HA1S[,2])
boxplot(H1_X5X10, names = cbind(class_names[4:6],class_names[4:6]),xlab="Variables", 
        ylab="PVI Scores",lwd=2, col = c("red","red","red","green","green","green"),
        main = "Effect of Correlation to VI in HA1")
abline(v=3.5,col="black",lty=2)
legend("topleft", legend = c("5_PVI","10_PVI"), border="black", fill = c("red", "green"))



### density plots


d=as.data.frame(read.csv("1.HA2S.csv",sep=",",header = T))
model=lm(y~V1+V2+V3+V4+V5+V6+V7+V8+V9+V300+V400+V500+V10+V100,d)
summary(model)




#### 
# set the typeI error as 0.05, so the power should be 
t1error10=NULL
t1error100=NULL
t1error200=NULL
t1error10_100=NULL
for(i in 1:3){
  t1error10 <- c(t1error10, quantile(VI_list[[class_names[i]]]$X10, 0.995))
t1error100 <- c(t1error100, quantile(VI_list[[class_names[i]]]$X100, 0.995))
t1error200 <- c(t1error200, quantile(VI_list[[class_names[i]]]$X200, 0.995))
t1error10_100 <- c(t1error10_100, quantile(VI_list[[class_names[i]]]$X.10..100., 0.995))
}
# calculate power
# HA1
power10=c()
power100=c()
power200=c()
for(i in 1:3){
  power10 <- c(power10, sum(VI_list[[class_names[3+i]]]$X10>t1error10[i])/1000)
  power100 <- c(power100, sum(VI_list[[class_names[3+i]]]$X100>t1error100[i])/1000)
power200 <- c(power200, sum(VI_list[[class_names[3+i]]]$X200>t1error200[i])/1000)
}
power10
power100
power200

# HA2
power10=c()
power100=c()
for(i in 1:3){
  power10 <- c(power10, sum(VI_list[[class_names[6+i]]]$X10>t1error10[i])/1000)
  power100 <- c(power100, sum(VI_list[[class_names[6+i]]]$X100>t1error100[i])/1000)
  #power200 <- c(power200, sum(VI_list[[class_names[6+i]]]$X200>t1error200[i])/1000)
}
power10
power100


###### power2
tag2=c("No","Weak","Strong")
# power HA1 , HA2
m=c(0,0,0,"HA1",0,0,"HA2")
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
#10 HA
for(i in c(4,7)){
  plot(density(VI_list[[class_names[i]]]$X10),lwd=2, col = "red", 
       xlim = c(t1error10[1]-0.01,max(VI_list[[class_names[i]]]$X10)),
       ylim=c(0,max(density(VI_list[[class_names[i+2]]]$X10)$y)),
       main=paste("Distribution of PVI Score of X10 in",m[i]), xlab = "PVI_X10", ylab = "Density")
  lines(density(VI_list[[class_names[i+1]]]$X10),lwd=2, col = "darkgreen")
  lines(density(VI_list[[class_names[i+2]]]$X10),lwd=2, col = "blue")
  abline(v=t1error10[1],col="red",lty=2,lwd=2)
  abline(v=t1error10[2],col="darkgreen",lty=2,lwd=2)
  abline(v=t1error10[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}
#100 HA
for(i in c(4,7)){
  plot(density(VI_list[[class_names[i]]]$X100),lwd=2, col = "red", 
       xlim = c(t1error100[1]-0.01,max(VI_list[[class_names[i]]]$X100)),
       ylim=c(0,max(density(VI_list[[class_names[i+2]]]$X100)$y)),
       main=paste("Distribution of PVI Score of X100 in",m[i]), xlab = "PVI_X100", ylab = "Density")
  lines(density(VI_list[[class_names[i+1]]]$X100),lwd=2, col = "darkgreen")
  lines(density(VI_list[[class_names[i+2]]]$X100),lwd=2, col = "blue")
  abline(v=t1error100[1],col="red",lty=2,lwd=2)
  abline(v=t1error100[2],col="darkgreen",lty=2,lwd=2)
  abline(v=t1error100[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}
# 200
for(i in c(4)){
  plot(density(VI_list[[class_names[i]]]$X200),lwd=2, col = "red", 
       xlim = c(t1error10[1]-0.01,max(VI_list[[class_names[i]]]$X200)),
       ylim=c(0,max(density(VI_list[[class_names[i+2]]]$X200)$y)),
       main=paste("Distribution of PVI Score of X200 in",m[i]), xlab = "PVI_X200", ylab = "Density")
  lines(density(VI_list[[class_names[i+1]]]$X200),lwd=2, col = "darkgreen")
  lines(density(VI_list[[class_names[i+2]]]$X200),lwd=2, col = "blue")
  abline(v=t1error200[1],col="red",lty=2,lwd=2)
  abline(v=t1error200[2],col="darkgreen",lty=2,lwd=2)
  abline(v=t1error200[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}

# 10_100
for(i in c(7)){
  plot(density(VI_list[[class_names[i]]]$X.10..100.),lwd=2, col = "red", 
       xlim = c(t1error10[1]-0.01,max(VI_list[[class_names[i]]]$X.10..100.)),
       ylim=c(0,max(density(VI_list[[class_names[i+2]]]$X.10..100.)$y)),
       main=paste("Distribution of Pairwise PVI Score of X10 & 100 in",m[i]), xlab = "Pairwise_PVI_X10&100", ylab = "Density")
  lines(density(VI_list[[class_names[i+1]]]$X.10..100.),lwd=2, col = "darkgreen")
  lines(density(VI_list[[class_names[i+2]]]$X.10..100.),lwd=2, col = "blue")
  abline(v=t1error10_100[1],col="red",lty=2,lwd=2)
  abline(v=t1error10_100[2],col="darkgreen",lty=2,lwd=2)
  abline(v=t1error10_100[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}

#power1
tag=c("N","W","S")
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  plot(density(VI_list[[class_names[4]]]$X10),lwd=2, col = "red", 
       xlim = c(t1error10[i]-0.01,max(VI_list[[class_names[4]]]$X10)),
       ylim=c(0,max(density(VI_list[[class_names[7]]]$X10)$y,density(VI_list[[class_names[4]]]$X10)$y)),
       main="", xlab = paste(tag[i],": PVI_X10",sep=""), ylab = "Density")
  lines(density(VI_list[[class_names[7]]]$X10),lwd=2, col = "darkgreen")
  abline(v=t1error10[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = c("H0N","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
  
  plot(density(VI_list[[class_names[4]]]$X100),lwd=2, col = "red", 
       xlim = c(t1error100[i]-0.01,max(VI_list[[class_names[4]]]$X100)),
       ylim=c(0,max(density(VI_list[[class_names[7]]]$X100)$y,density(VI_list[[class_names[4]]]$X100)$y)),
       main="", xlab = paste(tag[i],": PVI_X100",sep=""), ylab = "Density")
  lines(density(VI_list[[class_names[7]]]$X100),lwd=2, col = "darkgreen")
  abline(v=t1error100[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
  
  plot(density(VI_list[[class_names[4]]]$X200),lwd=2, col = "red", 
       xlim = c(t1error200[i]-0.01,max(VI_list[[class_names[4]]]$X200)),
       ylim=c(0,max(density(VI_list[[class_names[4]]]$X200)$y)),
       main="", xlab = paste(tag[i],": PVI_X200",sep=""), ylab = "Density")
  #lines(density(VI_list[[class_names[7]]]$X200),lwd=2, col = "darkgreeS")
  abline(v=t1error200[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
  
}



par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
# 10
plot(density(VI_list[[class_names[4]]]$X10),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X10),max(VI_list[[class_names[4]]]$X10)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X10)$y,density(VI_list[[class_names[4]]]$X10)$y)),
     main="", xlab = "PVI_X10", ylab = "Density",
)
lines(density(VI_list[[class_names[7]]]$X10),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[1]]]$X10),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[1]]]$X10),col="blue")
legend("topright", legend = c("H0N","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

plot(density(VI_list[[class_names[5]]]$X10),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X10),max(VI_list[[class_names[5]]]$X10)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X10)$y,density(VI_list[[class_names[5]]]$X10)$y)),
     main="", xlab = "PVI_X10", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X10),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X10),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[2]]]$X10),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

plot(density(VI_list[[class_names[6]]]$X10),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[3]]]$X10),max(VI_list[[class_names[6]]]$X10)),
     ylim=c(0,max(density(VI_list[[class_names[9]]]$X10)$y,density(VI_list[[class_names[6]]]$X10)$y)),
     main="", xlab = "PVI_X10", ylab = "Density",
)
lines(density(VI_list[[class_names[9]]]$X10),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[3]]]$X10),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[3]]]$X10),col="blue")
legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

# 100
plot(density(VI_list[[class_names[4]]]$X100),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X100),max(VI_list[[class_names[4]]]$X100)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X100)$y,density(VI_list[[class_names[4]]]$X100)$y)),
     main="", xlab = "PVI_X100", ylab = "Density",
)
lines(density(VI_list[[class_names[7]]]$X100),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[1]]]$X100),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[1]]]$X100),col="blue")
legend("topright", legend = c("H0N","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

plot(density(VI_list[[class_names[5]]]$X100),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X100),max(VI_list[[class_names[5]]]$X100)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X100)$y,density(VI_list[[class_names[5]]]$X100)$y)),
     main="", xlab = "PVI_X100", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X100),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X100),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[2]]]$X100),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

plot(density(VI_list[[class_names[6]]]$X100),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[3]]]$X100),max(VI_list[[class_names[6]]]$X100)),
     ylim=c(0,max(density(VI_list[[class_names[9]]]$X100)$y,density(VI_list[[class_names[6]]]$X100)$y)),
     main="", xlab = "PVI_X100", ylab = "Density",
)
lines(density(VI_list[[class_names[9]]]$X100),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[3]]]$X100),col="blue",lty=2,lwd=2)
#lines(density(VI_list[[class_names[3]]]$X100),col="blue")
legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)

mtext("Type I Error of PVIs (H0 vs HA)", side = 3, line =-2, outer = T)


# X.10..100.
plot(density(VI_list[[class_names[4]]]$X.10..100.),lwd=2, col = "red", 
     xlim = c(t1error10_100[i]-0.01,max(VI_list[[class_names[4]]]$X.10..100.)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X.10..100.)$y,density(VI_list[[class_names[4]]]$X.10..100.)$y)),
     main="", xlab = "PVI_X.10..100.", ylab = "Density")
lines(density(VI_list[[class_names[7]]]$X.10..100.),lwd=2, col = "darkgreen")
abline(v=t1error10_100[i],col="blue",lty=2,lwd=2)
legend("topright", legend = c("H0N","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)









################################ useless


# no need 5
plot(density(VI_list[[class_names[5]]]$X5),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X5),max(VI_list[[class_names[5]]]$X5)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X5)$y,density(VI_list[[class_names[5]]]$X5)$y)),
     main="", xlab = "PVI_X5", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X5),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X5),col="blue",lty=2,lwd=2)
lines(density(VI_list[[class_names[2]]]$X5),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

# no need 200
# V200
plot(density(VI_list[[class_names[1]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[1]]]$X200),max(mean(VI_list[[class_names[4]]]$X200))),
     main=" ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[4]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[7]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("VI_list[[class_names[1]]]", "HA1N", "HA2N"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)

plot(density(VI_list[[class_names[2]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[2]]]$X200),max(mean(VI_list[[class_names[5]]]$X200))),
     main=" ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[5]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[8]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("H0W", "HA1W", "HA2W"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)

plot(density(VI_list[[class_names[3]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[3]]]$X200),max(mean(VI_list[[class_names[6]]]$X200))),
     main=" ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[6]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[9]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("H0S", "HA1S", "HA2S"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)


# H0 compare to pair VI


# X.10..100.
plot(density(VI_list[[class_names[4]]]$X.10..100.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X.10..100.),max(VI_list[[class_names[4]]]$X.10..100.)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X.10..100.)$y,density(VI_list[[class_names[4]]]$X.10..100.)$y)),
     main="", xlab = "PVI_X.10..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[7]]]$X.10..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[1]]]$X.10..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[1]]]$X.10..100.),col="blue")
legend("topright", legend = c("VI_list[[class_names[1]]]","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[5]]]$X.10..100.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X.10..100.),max(VI_list[[class_names[5]]]$X.10..100.)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X.10..100.)$y,density(VI_list[[class_names[5]]]$X.10..100.)$y)),
     main="", xlab = "PVI_X.10..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X.10..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X.10..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[2]]]$X.10..100.),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[6]]]$X.10..100.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[3]]]$X.10..100.),max(VI_list[[class_names[6]]]$X.10..100.)),
     ylim=c(0,max(density(VI_list[[class_names[9]]]$X.10..100.)$y,density(VI_list[[class_names[6]]]$X.10..100.)$y)),
     main="", xlab = "PVI_X.10..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[9]]]$X.10..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[3]]]$X.10..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[3]]]$X.10..100.),col="blue")
legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

# X.5..10.
plot(density(VI_list[[class_names[4]]]$X.5..10.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X.5..10.),max(VI_list[[class_names[4]]]$X.5..10.)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X.5..10.)$y,density(VI_list[[class_names[4]]]$X.5..10.)$y)),
     main="", xlab = "PVI_X.5..10.", ylab = "Density",
)
lines(density(VI_list[[class_names[7]]]$X.5..10.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[1]]]$X.5..10.),col="blue",lty=2)
#lines(density(VI_list[[class_names[1]]]$X.5..10.),col="blue")
legend("topright", legend = c("VI_list[[class_names[1]]]","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[5]]]$X.5..10.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X.5..10.),max(VI_list[[class_names[5]]]$X.5..10.)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X.5..10.)$y,density(VI_list[[class_names[5]]]$X.5..10.)$y)),
     main="", xlab = "PVI_X.5..10.", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X.5..10.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X.5..10.),col="blue",lty=2)
#lines(density(VI_list[[class_names[2]]]$X.5..10.),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[6]]]$X.5..10.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[3]]]$X.5..10.),max(VI_list[[class_names[6]]]$X.5..10.)),
     ylim=c(0,max(density(VI_list[[class_names[9]]]$X.5..10.)$y,density(VI_list[[class_names[6]]]$X.5..10.)$y)),
     main="", xlab = "PVI_X.5..10.", ylab = "Density",
)
lines(density(VI_list[[class_names[9]]]$X.5..10.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[3]]]$X.5..10.),col="blue",lty=2)
#lines(density(VI_list[[class_names[3]]]$X.5..10.),col="blue")
legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)



# X.5..100.
plot(density(VI_list[[class_names[4]]]$X.5..10.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X.5..100.),max(VI_list[[class_names[4]]]$X.5..100.)),
     ylim=c(0,max(density(VI_list[[class_names[7]]]$X.5..100.)$y,density(VI_list[[class_names[4]]]$X.5..100.)$y)),
     main="", xlab = "PVI_X.5..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[7]]]$X.5..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[1]]]$X.5..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[1]]]$X.5..100.),col="blue")
legend("topright", legend = c("VI_list[[class_names[1]]]","HA1N","HA2N"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[5]]]$X.5..100.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[2]]]$X.5..100.),max(VI_list[[class_names[5]]]$X.5..100.)),
     ylim=c(0,max(density(VI_list[[class_names[8]]]$X.5..100.)$y,density(VI_list[[class_names[5]]]$X.5..100.)$y)),
     main="", xlab = "PVI_X.5..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[8]]]$X.5..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[2]]]$X.5..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[2]]]$X.5..100.),col="blue")
legend("topright", legend = c("H0W","HA1W","HA2W"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

plot(density(VI_list[[class_names[6]]]$X.5..100.),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[3]]]$X.5..100.),max(VI_list[[class_names[6]]]$X.5..100.)),
     ylim=c(0,max(density(VI_list[[class_names[9]]]$X.5..100.)$y,density(VI_list[[class_names[6]]]$X.5..100.)$y)),
     main="", xlab = "PVI_X.5..100.", ylab = "Density",
)
lines(density(VI_list[[class_names[9]]]$X.5..100.),lwd=2, col = "darkgreen")
abline(v=mean(VI_list[[class_names[3]]]$X.5..100.),col="blue",lty=2)
#lines(density(VI_list[[class_names[3]]]$X.5..100.),col="blue")
legend("topright", legend = c("H0S","HA1S","HA2S"),lwd=2, col = c("blue", "red", "darkgreen"), lty = 1)

# what if H0 vs HA1 plots 
plot(density(VI_list[[class_names[4]]]$X10),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X10,VI_list[[class_names[2]]]$X10,VI_list[[class_names[3]]]$X10),max(VI_list[[class_names[4]]]$X10,VI_list[[class_names[5]]]$X10,VI_list[[class_names[6]]]$X10)),
     ylim=c(0,max(density(VI_list[[class_names[4]]]$X10)$y,density(VI_list[[class_names[5]]]$X10)$y,density(VI_list[[class_names[6]]]$X10)$y)),
     main="", xlab = "PVI_X10", ylab = "Density",
)
lines(density(VI_list[[class_names[5]]]$X10),lwd=2, col = "orange")
lines(density(VI_list[[class_names[6]]]$X10),lwd=2, col = "lightblue")
#lines(density(VI_list[[class_names[1]]]$X10),lwd=2, col = "green")
#lines(density(VI_list[[class_names[2]]]$X10),lwd=2, col = "purple")
#lines(density(c(VI_list[[class_names[1]]]$X10,VI_list[[class_names[2]]]$X10,VI_list[[class_names[3]]]$X10)),lwd=2, col = "blue")
abline(v=mean(c(VI_list[[class_names[1]]]$X10,VI_list[[class_names[2]]]$X10,VI_list[[class_names[3]]]$X10)),col="blue",lty=2)
#abline(v=mean(VI_list[[class_names[4]]]$X10),col="red",lty=2)
#abline(v=mean(VI_list[[class_names[5]]]$X10),col="orange",lty=2)
#abline(v=mean(VI_list[[class_names[6]]]$X10),col="lightblue",lty=2)

#lines(density(VI_list[[class_names[2]]]$X10),col="blue")
legend("topright", legend = c("H0","HA1S","HA1W","HA1N"), 
       col = c("blue","lightblue","orange","red"), lty = 1)


plot(density(VI_list[[class_names[4]]]$X100),lwd=2, col = "red", 
     xlim = c(min(VI_list[[class_names[1]]]$X100,VI_list[[class_names[2]]]$X100,VI_list[[class_names[3]]]$X100),max(VI_list[[class_names[4]]]$X100,VI_list[[class_names[5]]]$X100,VI_list[[class_names[6]]]$X100)),
     ylim=c(0,max(density(VI_list[[class_names[4]]]$X100)$y,density(VI_list[[class_names[5]]]$X100)$y,density(VI_list[[class_names[6]]]$X100)$y)),
     main="", xlab = "PVI_X100", ylab = "Density",
)
lines(density(VI_list[[class_names[5]]]$X100),lwd=2, col = "orange")
lines(density(VI_list[[class_names[6]]]$X100),lwd=2, col = "lightblue")
#lines(density(VI_list[[class_names[1]]]$X100),lwd=2, col = "green")
#lines(density(VI_list[[class_names[2]]]$X100),lwd=2, col = "purple")
#lines(density(c(VI_list[[class_names[1]]]$X100,VI_list[[class_names[2]]]$X100,VI_list[[class_names[3]]]$X100)),lwd=2, col = "green")
abline(v=mean(c(VI_list[[class_names[1]]]$X100,VI_list[[class_names[2]]]$X100,VI_list[[class_names[3]]]$X100)),col="blue",lty=2)
#abline(v=mean(VI_list[[class_names[4]]]$X100),col="red",lty=2)
#abline(v=mean(VI_list[[class_names[5]]]$X100),col="orange",lty=2)
#abline(v=mean(VI_list[[class_names[6]]]$X100),col="lightblue",lty=2)

#lines(density(VI_list[[class_names[2]]]$X100),col="blue")
legend("topright", legend = c("H0","HA1S","HA1W","HA1N"), 
       col = c("blue","lightblue","orange","red"), lty = 1)


###### Type 1 error Wrong!!!
# For HA1, V10, V100, V200 all == 0
# For HA2, V10, V100 all == 0
# For V5, 0.001,0.004 very small error
#length(VI_list[[class_names[2]]]$X5[VI_list[[class_names[2]]]$X5>mean(VI_list[[class_names[5]]]$X5)])/1000
#length(VI_list[[class_names[2]]]$X5[VI_list[[class_names[2]]]$X5>mean(VI_list[[class_names[8]]]$X5)])/1000

par(mfrow = c(3, 4), mar = c(2, 2, 3, 2))
tags=c("No Correlation","Weak","Strong")
for(i in 1:3){
  plot(density(VI_list[[class_names[i]]]$X10),lwd=2, col = "blue", 
       xlim = c(min(VI_list[[class_names[i]]]$X10),0.5),
       main=" ",cex.main =1, xlab = "", ylab = "")
  abline(v=mean(VI_list[[class_names[i+3]]]$X10),col="red",lty=2)
  abline(v=mean(VI_list[[class_names[7]]]$X10),col="darkgreen",lty=2)
  
  plot(density(VI_list[[class_names[i]]]$X100),lwd=2, col = "blue", 
       xlim = c(min(VI_list[[class_names[i]]]$X100),0.5),
       main=" ",cex.main =1,xlab = "", ylab = "")
  abline(v=mean(VI_list[[class_names[i+3]]]$X100),col="red",lty=2)
  abline(v=mean(VI_list[[class_names[7]]]$X100),col="darkgreen",lty=2)
  
  plot(density(VI_list[[class_names[i]]]$X200),lwd=2, col = "blue", 
       xlim = c(min(VI_list[[class_names[i]]]$X200),0.18),
       main=" ",cex.main =1, xlab = "", ylab = "")
  abline(v=mean(VI_list[[class_names[i+3]]]$X200),col="red",lty=2)
  abline(v=mean(VI_list[[class_names[7]]]$X200),col="darkgreen",lty=2)
  
  plot(density(VI_list[[class_names[i]]]$X.10..100.),lwd=2, col = "blue", 
       xlim = c(min(VI_list[[class_names[i]]]$X.10..100.),0.9),
       main=" ",cex.main =1, xlab = "", ylab = "")
  abline(v=mean(VI_list[[class_names[i+3]]]$X.10..100.),col="red",lty=2)
  abline(v=mean(VI_list[[class_names[7]]]$X.10..100.),col="darkgreen",lty=2)
  legend("top", legend = c("H0N", "HA1N", "HA2N"), 
         col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
         cex=0.7)
}


### (no need
#V10
plot(density(VI_list[[class_names[1]]]$X10),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[1]]]$X10),0.5),
     main=" ",cex.main =1, xlab = "PVI_X10", ylab = "Density")
abline(v=mean(VI_list[[class_names[4]]]$X10),col="red",lty=2)
abline(v=mean(VI_list[[class_names[7]]]$X10),col="darkgreen",lty=2)
legend("top", legend = c("H0N", "HA1N", "HA2N"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[2]]]$X10),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[2]]]$X10),0.5),
     main=" ",cex.main =1, xlab = "PVI_X10", ylab = "Density")
abline(v=mean(VI_list[[class_names[5]]]$X10),col="red",lty=2)
abline(v=mean(VI_list[[class_names[8]]]$X10),col="darkgreen",lty=2)
legend("top", legend = c("H0W", "HA1W", "HA2W"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[3]]]$X10),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[3]]]$X10),0.5),
     main=" ",cex.main =1, xlab = "PVI_X10", ylab = "Density")
abline(v=mean(VI_list[[class_names[6]]]$X10),col="red",lty=2)
abline(v=mean(VI_list[[class_names[9]]]$X10),col="darkgreen",lty=2)
legend("top", legend = c("H0S", "HA1S", "HA2S"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
# V100
plot(density(VI_list[[class_names[1]]]$X100),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[1]]]$X100),0.5),
     main="b. ", xlab = "PVI_X100", ylab = "Density")
abline(v=mean(VI_list[[class_names[4]]]$X100),col="red",lty=2)
abline(v=mean(VI_list[[class_names[7]]]$X100),col="darkgreen",lty=2)
legend("top", legend = c("H0N", "HA1N", "HA2N"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[2]]]$X100),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[2]]]$X100),0.5),
     main="b. ", xlab = "PVI_X100", ylab = "Density")
abline(v=mean(VI_list[[class_names[5]]]$X100),col="red",lty=2)
abline(v=mean(VI_list[[class_names[8]]]$X100),col="darkgreen",lty=2)
legend("top", legend = c("H0W", "HA1W", "HA2W"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[3]]]$X100),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[3]]]$X100),0.5),
     main="b. ", xlab = "PVI_X100", ylab = "Density")
abline(v=mean(VI_list[[class_names[6]]]$X100),col="red",lty=2)
abline(v=mean(VI_list[[class_names[9]]]$X100),col="darkgreen",lty=2)
legend("top", legend = c("H0S", "HA1S", "HA2S"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)

## HA2 pair 10_100
plot(density(VI_list[[class_names[1]]]$X.10..100.),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[1]]]$X.10..100.),0.9),
     main="b. ", xlab = "PVI_X.10..100.", ylab = "Density")
abline(v=mean(VI_list[[class_names[4]]]$X.10..100.),col="red",lty=2)
abline(v=mean(VI_list[[class_names[7]]]$X.10..100.),col="darkgreen",lty=2)
legend("top", legend = c("H0N", "HA1N", "HA2N"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[2]]]$X.10..100.),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[2]]]$X.10..100.),0.9),
     main="b. ", xlab = "PVI_X.10..100.", ylab = "Density")
abline(v=mean(VI_list[[class_names[5]]]$X.10..100.),col="red",lty=2)
abline(v=mean(VI_list[[class_names[8]]]$X.10..100.),col="darkgreen",lty=2)
legend("top", legend = c("H0W", "HA1W", "HA2W"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[3]]]$X.10..100.),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[3]]]$X.10..100.),0.9),
     main="b. ", xlab = "PVI_X.10..100.", ylab = "Density")
abline(v=mean(VI_list[[class_names[6]]]$X.10..100.),col="red",lty=2)
abline(v=mean(VI_list[[class_names[9]]]$X.10..100.),col="darkgreen",lty=2)
legend("top", legend = c("H0S", "HA1S", "HA2S"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)


## HA1 V200
plot(density(VI_list[[class_names[1]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[1]]]$X200),0.18),
     main="b. ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[4]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[7]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("H0N", "HA1N", "HA2N"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[2]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[2]]]$X200),0.18),
     main="b. ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[5]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[8]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("H0W", "HA1W", "HA2W"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
plot(density(VI_list[[class_names[3]]]$X200),lwd=2, col = "blue", 
     xlim = c(min(VI_list[[class_names[3]]]$X200),0.18),
     main="b. ", xlab = "PVI_X200", ylab = "Density")
abline(v=mean(VI_list[[class_names[6]]]$X200),col="red",lty=2)
abline(v=mean(VI_list[[class_names[9]]]$X200),col="darkgreen",lty=2)
legend("top", legend = c("H0S", "HA1S", "HA2S"), 
       col = c("blue", "red", "darkgreen"), lty = c(1,2,2), bty = "n",
       cex=0.7)
### no need)
