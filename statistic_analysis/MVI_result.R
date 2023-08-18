#MVI results
path="res_20July/"
type="_MVI.csv"
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
VI_file=paste(path,class_names,type,sep='')
MVI_list=list()
for(i in 1:9){
  MVI_list[[class_names[i]]]=as.data.frame(read.csv(VI_file[i],sep="\t",header = T))
}

for(i in 7:9){
  MVI_list[[class_names[i]]]=as.data.frame(read.csv(paste("res_1Aug/",class_names[i],type,sep=""),sep="\t",header = T))
}

##### H0
# single VIs
png("H0sVI.png",height=500,width=700)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(VI_list[[class_names[i]]][1:7], names =colnames(VI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="PVI", 
          main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.08,0.1))
  axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
  boxplot(MVI_list[[class_names[i]]][1:7], names =colnames(MVI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="MVI", 
          main = paste("MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.08,0.1))
  axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
}
dev.off()

# pair VIs
pairname=c("X5_X10","X5_X100","X5_X200","X10_X100","X10_X200","X100_X200","random1","random2","random3")

png("H0pVI.png",height=500,width=1050)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(VI_list[[class_names[i]]][8:16], names = pairname, xlab="Variable Pairs", ylab="Pairwise PVI", 
          main = paste("Pairwise PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.08,0.08),cex.axis=0.8)
  axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
  boxplot(MVI_list[[class_names[i]]][8:16], names =pairname,xlab="Variable Pairs", ylab="Pairwise MVI",
          main = paste("Pairwise MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,0.16),cex.axis=0.8)
  axis(2, at = round(seq(-0.2, 1, by = 0.04),2),las=2)
}
dev.off()


##### HA1
# single VIs
png("HA1sVI.png",height=470,width=680)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 4:6){
  boxplot(VI_list[[class_names[i]]][1:7], names =colnames(VI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="PVI", 
          main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
  boxplot(MVI_list[[class_names[i]]][1:7], names =colnames(MVI_list[[class_names[i]]])[1:7],xlab="Variables", ylab="MVI", 
          main = paste("MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)}
dev.off()

# paired MVI (no neeed)
png("HA1pMVI.png")
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 7:9){
  boxplot(VI_list[[class_names[i]]][8:16], names = pairname, xlab="Variable Pairs", ylab="Pairwise PVI", 
          main = paste("Pairwise PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,1.5),cex.axis=0.8)
  axis(2, at = round(seq(-0.1, 4, by = 0.2),2),las=2)
  boxplot(MVI_list[[class_names[i]]][8:16], names =pairname,xlab="Variable Pairs", ylab="Pairwise MVI",
          main = paste("Pairwise MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,1.7),cex.axis=0.8)
  axis(2, at = round(seq(-0.1, 4, by = 0.2),2),las=2)
}
dev.off()

# additive PVI and MVI
png("HA1_add_VIs.png",height=400, width=500)
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
# 5 and 10
for(i in 4:6){
  boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][2],VI_list[[class_names[i]]][8]),
          names =c("Add_PVI","Pairwise_PVI"),ylim=c(0.1,0.8),ylab="PVI Scores", yaxt = "n",
          col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
mtext("Comparison of Addition of PVIs and Pairwise PVI of X5 & X10 in HA1\n    HA1N                                 HA1W                                  HA1S", 
      side = 3, line =-4, outer = T)

for(i in 4:6){
  boxplot(cbind(MVI_list[[class_names[i]]][1]+MVI_list[[class_names[i]]][2],MVI_list[[class_names[i]]][8]),
          names =c("Add_MVI","Pairwise_MVI"),ylim=c(0.1,0.8),ylab="MVI Scores", yaxt = "n",
          col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
mtext("Comparison of Addition of PVIs and Pairwise PVI of X5 & X10 in HA1\n    HA1N                                 HA1W                                  HA1S",
      side = 3, line =-25.5, outer = T)
dev.off()

# HA1 10 and 100, PVI no, MVI yes
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
for(i in 4:6){
  boxplot(cbind(VI_list[[class_names[i]]][2]+VI_list[[class_names[i]]][3],VI_list[[class_names[i]]][11]),
          names =c("Add(X10+X100)","Pair(X10&X100)"),xlab=class_names[i], ylab="PVI", ylim=c(0.3,1.35),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise PVI of X10&X100 in",class_names[i]))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
for(i in 4:6){
  boxplot(cbind(MVI_list[[class_names[i]]][2]+MVI_list[[class_names[i]]][3],MVI_list[[class_names[i]]][11]),
          names =c("Add(X10+X100)","Pair(X10&X100)"),xlab=class_names[i], ylab="MVI", ylim=c(0.2,1.63),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise MVI of X10&X100 in",class_names[i]))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}


# HA1 10 and 200, PVI no, MVI yes
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
for(i in 4:6){
  boxplot(cbind(VI_list[[class_names[i]]][2]+VI_list[[class_names[i]]][4],VI_list[[class_names[i]]][12]),
          names =c("Add(X10+X200)","Pair(X10&X200)"),xlab=class_names[i], ylab="PVI", ylim=c(0.15,1),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise PVI of X10&X200 in",class_names[i]))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
for(i in 4:6){
  boxplot(cbind(MVI_list[[class_names[i]]][2]+MVI_list[[class_names[i]]][4],MVI_list[[class_names[i]]][12]),
          names =c("Add(X10+X200)","Pair(X10&X200)"),xlab=class_names[i], ylab="MVI", ylim=c(0,1.2),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise MVI of X10&X200 in",class_names[i]))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}

# HA1 100 and 200, PVI no, MVI yes
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
for(i in 4:6){
  boxplot(cbind(VI_list[[class_names[i]]][3]+VI_list[[class_names[i]]][4],VI_list[[class_names[i]]][13]),
          names =c("Add_PVI","Pairwise_PVI"),xlab=class_names[i], ylab="PVI Scores", 
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
mtext("Comparison of Addition of PVIs and Pairwise PVI of X100 & X200 in HA1\n   HA1N                                  HA1W                                  HA1S", 
      side = 3, line =-4, outer = T)
for(i in 4:6){
  boxplot(cbind(MVI_list[[class_names[i]]][3]+MVI_list[[class_names[i]]][4],MVI_list[[class_names[i]]][13]),
          names =c("Add_MVI","Pairwise_MVI"),xlab=class_names[i], ylab="MVI Scores", 
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}

mtext("Comparison of Addition of MVIs and Pairwise MVI of X100 & X200 in HA1\n    HA1N                                 HA1W                                  HA1S", 
      side = 3, line =-25, outer = T)
dev.off()

# when there's no correlation, the PVI is linear, add=pair, but MVI is non-linear,add<pair
# see the PVI and MVI of 100&200 in HA1, PVI add=pair, MVI add<pair
# maybe MVI is more robust because the PVI is random shift
# when there is correlation, the both VI becomes non-linear, see 10&100, 10&200, both add<pair
# 8-10: 5x10,5x100,5x200,
# 11-13: 10x100,10x200,100x200
# [10, 8, 5, 3]
# 0.2, 0, -0.2, -0.4
# no correlation and no interaction, the add can be higher or lower than pair, higher coefficient more likely pair>add
# correlation reduce the VI of actual associatied variables
# MVI is more sensitive to correlation, make VI decrease, but PVI less sensitive
# 2 variables with low correlation but add<pair, then maybe interact

##### HA2
# single MVI
png("HA2sVI.png",height=470,width=680)
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 7:9){
  boxplot(VI_list[[class_names[i]]][c(c(1:3),c(5:7))], names =colnames(VI_list[[class_names[i]]])[c(c(1:3),c(5:7))],xlab="Variables", ylab="PVI", 
          main = paste("PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
  boxplot(MVI_list[[class_names[i]]][c(c(1:3),c(5:7))], names =colnames(MVI_list[[class_names[i]]])[c(c(1:3),c(5:7))],xlab="Variables", ylab="MVI", 
          main = paste("MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
dev.off()

# single additive vs paired VI 5+10, 10+100, 5+100
# do normalised or not normalised ??

# HA2 10-100,  PVI no, MVI yes
png("HA2_10_100_add_VIs.png",height=400, width=500)
par(mfrow = c(2, 3), mar = c(3, 4, 5, 2))
for(i in 7:9){
  boxplot(cbind(VI_list[[class_names[i]]][2]+VI_list[[class_names[i]]][3],VI_list[[class_names[i]]][11]),
          names =c("Add(X10+X100)","Pair(X10&X100)"),xlab=class_names[i], ylab="PVI", ylim=c(0.3,1.35),
          cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise PVI of X10&X100 in",class_names[i]))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
#mtext("Comparison of Addition of PVIs and Pairwise PVI of X10 & X100 in HA2\n       HA2N                                                 HA2W                                                 HA2S", 
#      side = 3, line =-4, outer = T)
for(i in 7:9){
boxplot(cbind(MVI_list[[class_names[i]]][2]+MVI_list[[class_names[i]]][3],MVI_list[[class_names[i]]][11]),
        names =c("Add(X10+X100)","Pair(X10&X100)"),xlab=class_names[i], ylab="MVI", ylim=c(0.2,1.63),
        cex.axis = 0.9,yaxt = "n",col=c("grey","orange"),main=paste("Pairwise MVI of X10&X100 in",class_names[i]))
axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
#mtext("Comparison of Addition of MVIs and Pairwise MVI of X10 & X100 in HA2\n       HA2N                                                HA2W                                                  HA2S", 
#      side = 3, line =-24, outer = T)
dev.off()

# HA2 5-10, both yes
png("HA2_5_10_add_VIs.png",height=400, width=500)
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
for(i in 7:9){
  boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][2],VI_list[[class_names[i]]][8]),
          names =c("Add_PVI","Pairwise_PVI"),xlab=class_names[i], ylab="PVI Scores", ylim=c(0.1,0.82),
          main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
mtext("Comparison of Addition of PVIs and Paired PVI of X5 & X10 in HA2\n       HA2N                                   HA2W                                    HA2S", 
      side = 3, line =-4, outer = T)
for(i in 7:9){
boxplot(cbind(MVI_list[[class_names[i]]][1]+MVI_list[[class_names[i]]][2],MVI_list[[class_names[i]]][8]),
        names =c("Add_MVI","Pairwise_MVI"),xlab=class_names[i], ylab="MVI Scores", ylim=c(0.1,0.82),
        main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)}
mtext("Comparison of Addition of MVIs and Paired MVI of X5 & X10 in HA2\n       HA2N                                   HA2W                                    HA2S", 
      side = 3, line =-25, outer = T)

# HA2 5-100, MVI yes, PVI no
png("HA2_5_100_add_VIs.png",height=400, width=500)
par(mfrow = c(2, 3), mar = c(2, 4, 5, 2))
for(i in 7:9){
  boxplot(cbind(VI_list[[class_names[i]]][1]+VI_list[[class_names[i]]][3],VI_list[[class_names[i]]][9]),
          names =c("Add_PVI","Pairwise_PVI"),xlab=class_names[i], ylab="PVI Scores",ylim=c(0.17,0.92),
          main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)}
mtext("Comparison of Addition of PVIs and Paired PVI of X5 & X100 in HA2\n        HA2N                                          HA2W                                          HA2S", 
      side = 3, line =-4, outer = T)
for(i in 7:9){
  boxplot(cbind(MVI_list[[class_names[i]]][1]+MVI_list[[class_names[i]]][3],MVI_list[[class_names[i]]][9]),
          names =c("Add_MVI","Pairwise_MVI"),xlab=class_names[i], ylab="MVI Scores",ylim=c(0.17,0.92),
          main = "",cex.axis = 0.9,yaxt = "n",col=c("grey","orange"))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)}
mtext("Comparison of Addition of MVIs and Paired MVI of X5 & X100 in HA2\n        HA2N                                          HA2W                                          HA2S",
      side = 3, line =-25, outer = T)
dev.off()


# paired t.test
# 10-100  PVI: no significant, MVI: pair>add,correlation make both drop but still pair>add
for(i in 4:6){
  su=t.test(VI_list[[class_names[i]]]$X100+VI_list[[class_names[i]]]$X10,VI_list[[class_names[i]]]$X.10..100., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
  su=t.test(MVI_list[[class_names[i]]]$X100+MVI_list[[class_names[i]]]$X10,MVI_list[[class_names[i]]]$X.10..100., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
}
# 10-200 PVI less sensitive, only pair>add in S, MVI sensitive, pair>add in W and S
for(i in 4:6){
  su=t.test(VI_list[[class_names[i]]]$X10+VI_list[[class_names[i]]]$X200,VI_list[[class_names[i]]]$X.10..200., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
  su=t.test(MVI_list[[class_names[i]]]$X10+MVI_list[[class_names[i]]]$X200,MVI_list[[class_names[i]]]$X.10..200., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
}

# 5-100 Same as 5-10, so interaction is similar to correlation in the model
for(i in 7:9){
  su=t.test(VI_list[[class_names[i]]]$X5+VI_list[[class_names[i]]]$X100,VI_list[[class_names[i]]]$X.5..100., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
  su=t.test(MVI_list[[class_names[i]]]$X5+MVI_list[[class_names[i]]]$X100,MVI_list[[class_names[i]]]$X.5..100., alternative = "two.sided",paired = F,var.equal=T)
  print(su)
}
# average of all other paired VI results


# As for the interaction between X10 and X100 in HA2, there's no difference between additive PVI and paired PVI,
# while the paired MVI is significantly higher than additive MVI no matter if the correlation exist, meaning that MVI has the ability to find interaction between two variables while paired PVI cannot.
# The paired MVI score of X5 and X10 is larger than the additive MVI score of X5 and X10 when correlation exist, similarly in the case of PVI,
# indicating paired MVI and PVI are both able to show the correlation between two variables.
# However, X5 and X100 have higher paired MVI score compared to additive MVI score when the correlation between X5 and X10 exists,
# which means that MVI itself cannot distinguish between the correlation and interaction.
# It's noticeable that the Comparison of Addition of PVIs and Pairwise PVI of X5 and X100 stays the same since it doesn't have the ability to detect interaction at the first place.
# As a result, when additive PVI and paired PVI are the same and paired MVI is higher than additive MVI, we can say the two variables are interacted with each other.
# To sum up, paired MVI and PVI are both affected by the correlation, while only paired MVI can reveal the interaction,
# which indicates a method to tell the interaction relationship between two variables using both MVI and PVI results.
# MVI set the column to 0, removed the impact of the variable from the model, while PVI shift the values of the variables
# ??? what about for each dataset, test of the pair higher than additive, calculate the power and type I error?


#### 
# set the typeI error as 0.05, so the power should be 
Mt1error10=NULL
Mt1error100=NULL
Mt1error200=NULL
Mt1error10_100=NULL
for(i in 1:3){
  Mt1error10 <- c(Mt1error10, quantile(MVI_list[[class_names[i]]]$X10, 0.995))
  Mt1error100 <- c(Mt1error100, quantile(MVI_list[[class_names[i]]]$X100, 0.995))
  Mt1error200 <- c(Mt1error200, quantile(MVI_list[[class_names[i]]]$X200, 0.995))
  Mt1error10_100 <- c(Mt1error10_100, quantile(MVI_list[[class_names[i]]]$X.10..100., 0.995))
}
# calculate power
# HA1
power10=c()
power100=c()
power200=c()
for(i in 1:3){
  power10 <- c(power10, sum(MVI_list[[class_names[3+i]]]$X10>Mt1error10[i])/1000)
  power100 <- c(power100, sum(MVI_list[[class_names[3+i]]]$X100>Mt1error100[i])/1000)
  power200 <- c(power200, sum(MVI_list[[class_names[3+i]]]$X200>Mt1error200[i])/1000)
}
power10
power100
power200

# HA2
power10=c()
power100=c()
power200=c()
for(i in 1:3){
  power10 <- c(power10, sum(MVI_list[[class_names[6+i]]]$X10>Mt1error10[i])/1000)
  power100 <- c(power100, sum(MVI_list[[class_names[6+i]]]$X100>Mt1error100[i])/1000)
  #power200 <- c(power200, sum(VI_list[[class_names[6+i]]]$X200>Mt1error200[i])/1000)
}
power10
power100

###### power
tag=c("N","W","S")
tag2=c("No","Weak","Strong")
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  plot(density(MVI_list[[class_names[i+3]]]$X10),lwd=2, col = "red", 
       xlim = c(Mt1error10[i]-0.01,max(MVI_list[[class_names[i+3]]]$X10)),
       ylim=c(0,max(density(MVI_list[[class_names[i+6]]]$X10)$y,density(MVI_list[[class_names[i+3]]]$X10)$y)),
       main=paste("Datasets with ",tag2[i]," Correlation",sep=""), xlab = "MVI Score of X10", ylab = "Density")
  lines(density(MVI_list[[class_names[i+6]]]$X10),lwd=2, col = "darkgreen")
  abline(v=Mt1error10[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+3,i+6)],lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
}
for(i in 1:3){  
  plot(density(MVI_list[[class_names[i+3]]]$X100),lwd=2, col = "red", 
       xlim = c(Mt1error100[i]-0.01,max(MVI_list[[class_names[i+3]]]$X100)),
       ylim=c(0,max(density(MVI_list[[class_names[i+6]]]$X100)$y,density(MVI_list[[class_names[i+3]]]$X100)$y)),
       main=paste("Datasets with ",tag2[i]," Correlation",sep=""), xlab ="MVI Score of X100", ylab = "Density")
  lines(density(MVI_list[[class_names[i+6]]]$X100),lwd=2, col = "darkgreen")
  abline(v=Mt1error100[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+3,i+6)],lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
}
for(i in 1:3){
  plot(density(MVI_list[[class_names[i+3]]]$X200),lwd=2, col = "red", 
       xlim = c(Mt1error200[i]-0.01,max(MVI_list[[class_names[i+3]]]$X200)),
       ylim=c(0,max(density(MVI_list[[class_names[i+3]]]$X200)$y)),
       main=paste("Datasets with ",tag2[i]," Correlation",sep=""), xlab ="MVI Score of X200", ylab = "Density")
  #lines(density(MVI_list[[class_names[7]]]$X200),lwd=2, col = "darkgreeS")
  abline(v=Mt1error200[i],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+3,i+6)],lwd=2, col = c("blue", "red", "darkgreen"), lty = c(3,1,1),cex=0.7)
}

# power HA1 , HA2
m=c(0,0,0,"HA1",0,0,"HA2")
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
#10 HA
for(i in c(4,7)){
  plot(density(MVI_list[[class_names[i]]]$X10),lwd=2, col = "red", 
       xlim = c(Mt1error10[1]-0.01,max(MVI_list[[class_names[i]]]$X10)),
       ylim=c(0,max(density(MVI_list[[class_names[i+2]]]$X10)$y)),
       main=paste("Distribution of MVI Score of X10 in",m[i]), xlab = "MVI_X10", ylab = "Density")
  lines(density(MVI_list[[class_names[i+1]]]$X10),lwd=2, col = "darkgreen")
  lines(density(MVI_list[[class_names[i+2]]]$X10),lwd=2, col = "blue")
  abline(v=Mt1error10[1],col="red",lty=2,lwd=2)
  abline(v=Mt1error10[2],col="darkgreen",lty=2,lwd=2)
  abline(v=Mt1error10[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}
#100 HA
for(i in c(4,7)){
  plot(density(MVI_list[[class_names[i]]]$X100),lwd=2, col = "red", 
       xlim = c(Mt1error100[1]-0.01,max(MVI_list[[class_names[i]]]$X100)),
       ylim=c(0,max(density(MVI_list[[class_names[i+2]]]$X100)$y)),
       main=paste("Distribution of MVI Score of X100 in",m[i]), xlab = "MVI_X100", ylab = "Density")
  lines(density(MVI_list[[class_names[i+1]]]$X100),lwd=2, col = "darkgreen")
  lines(density(MVI_list[[class_names[i+2]]]$X100),lwd=2, col = "blue")
  abline(v=Mt1error100[1],col="red",lty=2,lwd=2)
  abline(v=Mt1error100[2],col="darkgreen",lty=2,lwd=2)
  abline(v=Mt1error100[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}
# 200
for(i in c(4)){
  plot(density(MVI_list[[class_names[i]]]$X200),lwd=2, col = "red", 
       xlim = c(Mt1error10[1]-0.01,max(MVI_list[[class_names[i]]]$X200)),
       ylim=c(0,max(density(MVI_list[[class_names[i+2]]]$X200)$y)),
       main=paste("Distribution of MVI Score of X200 in",m[i]), xlab = "MVI_X200", ylab = "Density")
  lines(density(MVI_list[[class_names[i+1]]]$X200),lwd=2, col = "darkgreen")
  lines(density(MVI_list[[class_names[i+2]]]$X200),lwd=2, col = "blue")
  abline(v=Mt1error200[1],col="red",lty=2,lwd=2)
  abline(v=Mt1error200[2],col="darkgreen",lty=2,lwd=2)
  abline(v=Mt1error200[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}

# 10_100
for(i in c(7)){
  plot(density(MVI_list[[class_names[i]]]$X.10..100.),lwd=2, col = "red", 
       xlim = c(Mt1error10[1]-0.01,max(MVI_list[[class_names[i]]]$X.10..100.)),
       ylim=c(0,max(density(MVI_list[[class_names[i+2]]]$X.10..100.)$y)),
       main=paste("Distribution of Pairwise MVI Score of X10 & 100 in",m[i]), xlab = "Pairwise_MVI_X10&100", ylab = "Density")
  lines(density(MVI_list[[class_names[i+1]]]$X.10..100.),lwd=2, col = "darkgreen")
  lines(density(MVI_list[[class_names[i+2]]]$X.10..100.),lwd=2, col = "blue")
  abline(v=Mt1error10_100[1],col="red",lty=2,lwd=2)
  abline(v=Mt1error10_100[2],col="darkgreen",lty=2,lwd=2)
  abline(v=Mt1error10_100[3],col="blue",lty=2,lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)],lwd=2, col = c("red", "darkgreen","blue"), lty = c(1,1,1),cex=0.7)
}
for(i in c(4:6)){
su=t.test(VI_list[[class_names[i]]][1],VI_list[[class_names[i]]][5])
print(su$p.value)
}
for(i in c(4:6)){
  su=t.test(MVI_list[[class_names[i]]][1],MVI_list[[class_names[i]]][7])
  print(su$p.value)
}

# compair of 5, 10 in HA1
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
for(i in 1:2){
  boxplot(cbind(VI_list[[class_names[4]]][i],VI_list[[class_names[5]]][i],VI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Datasets", ylab="PVI", 
          main = paste("PVI Scores of ",colnames(VI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:2){
  boxplot(cbind(MVI_list[[class_names[4]]][i],MVI_list[[class_names[5]]][i],MVI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Datasets", ylab="MVI", 
          main = paste("MVI Scores of ",colnames(VI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
# PVI, all significant in each plot
for(k in c(1:3)){
  for(i in c(4,7)){
  su=t.test(VI_list[[class_names[i+1]]][k],VI_list[[class_names[i+2]]][k])
  print(su$p.value)
}
}
# MVI, all significant in each plot
for(k in c(1:3)){
  for(i in c(4,7)){
    su=t.test(MVI_list[[class_names[i+1]]][k],MVI_list[[class_names[i+2]]][k])
    print(su$p.value)
  }
}


# MVI, X100, 4vs5,4vs6 significant
t.test(MVI_list[[class_names[5]]][3],MVI_list[[class_names[6]]][3])


# compair of 5, 10, 100 in HA2
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

for(i in 1:3){
  boxplot(cbind(VI_list[[class_names[7]]][i],VI_list[[class_names[8]]][i],VI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Datasets", ylab="PVI", 
          main = paste("PVI Scores of ",colnames(MVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(MVI_list[[class_names[7]]][i],MVI_list[[class_names[8]]][i],MVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Datasets", ylab="MVI", 
          main = paste("MVI Scores of ",colnames(MVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}

# PVI in X5, 10, 100
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(cbind(VI_list[[class_names[4]]][i],VI_list[[class_names[5]]][i],VI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Dataset Type", ylab="PVI", 
          main = paste("PVI Scores of ",colnames(VI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(VI_list[[class_names[7]]][i],VI_list[[class_names[8]]][i],VI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PVI", 
          main = paste("PVI Scores of ",colnames(MVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
# MVI
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(cbind(MVI_list[[class_names[4]]][i],MVI_list[[class_names[5]]][i],MVI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Dataset Type", ylab="MVI", 
          main = paste("MVI Scores of ",colnames(VI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(MVI_list[[class_names[7]]][i],MVI_list[[class_names[8]]][i],MVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MVI", 
          main = paste("MVI Scores of ",colnames(MVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}


# MVI and PVI are similar when the correlation get stronger, they reduce similar amount, when there's not interaction
# X10
for(i in c(4,7)){
  su=t.test(VI_list[[class_names[i+2]]][2]-VI_list[[class_names[i]]][2],MVI_list[[class_names[i+2]]][2]-MVI_list[[class_names[i]]][2])
  print(su$p.value)
  #print(su$estimate)
}
# X5
for(i in c(4,7)){
  su=t.test(VI_list[[class_names[i+2]]][1]-VI_list[[class_names[i]]][1],MVI_list[[class_names[i+2]]][1]-MVI_list[[class_names[i]]][1])
  print(su$p.value)
  #print(su$estimate)
}
# X100
for(i in c(7)){
  su=t.test(VI_list[[class_names[i+2]]][3]-VI_list[[class_names[i]]][3],MVI_list[[class_names[i+2]]][3]-MVI_list[[class_names[i]]][3])
  print(su$p.value)
  #print(su$estimate)
}
# when correlation and interaction both exist, MVI and PVI react slightly different

# pairwise result
ttags=c("X5 & X10","X5 & X100",0,"X10 & X100")
# compair of 5*10,5*100, 10x100 in HA2
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in c(8,9,11)){
  boxplot(cbind(VI_list[[class_names[7]]][i],VI_list[[class_names[8]]][i],VI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PVI", 
          main = paste("Pairwise PVI of ",ttags[i-7]," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.4))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
for(i in c(8,9,11)){
  boxplot(cbind(MVI_list[[class_names[7]]][i],MVI_list[[class_names[8]]][i],MVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MVI", 
          main = paste("Pairwise MVI of ",ttags[i-7]," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.7))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
# HA1
ttags=c("X5 & X10","X5 & X100",0,"X10 & X100")
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in c(8,9,11)){
  boxplot(cbind(VI_list[[class_names[4]]][i],VI_list[[class_names[5]]][i],VI_list[[class_names[6]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PVI", 
          main = paste("Pairwise PVI of ",ttags[i-7]," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.4))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
for(i in c(8,9,11)){
  boxplot(cbind(MVI_list[[class_names[4]]][i],MVI_list[[class_names[5]]][i],MVI_list[[class_names[6]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MVI", 
          main = paste("Pairwise MVI of ",ttags[i-7]," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.7))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}


# HA1/10 vs HA2/9
for(i in 4:6){
su=t.test((VI_list[[class_names[i]]]$X.10..100.),VI_list[[class_names[i+3]]]$X.10..100., alternative = "two.sided",paired = F,var.equal=T)
print(su)
}
