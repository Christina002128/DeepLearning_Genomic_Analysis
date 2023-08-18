# Normalised comparison
#norVI results
path="res_20July/"
type="_nor_PVI.csv"
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
norVI_file=paste(path,class_names,type,sep='')
norVI_list=list()
for(i in 1:9){
  norVI_list[[class_names[i]]]=as.data.frame(read.csv(norVI_file[i],sep="\t",header = T))
}

for(i in 7:9){
  norVI_list[[class_names[i]]]=as.data.frame(read.csv(paste("res_1Aug/",class_names[i],type,sep=""),sep="\t",header = T))
}
#MnorVI results
path="res_20July/"
type="_nor_MVI.csv"
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
norVI_file=paste(path,class_names,type,sep='')
MnorVI_list=list()
for(i in 1:9){
  MnorVI_list[[class_names[i]]]=as.data.frame(read.csv(norVI_file[i],sep="\t",header = T))
}

for(i in 7:9){
  MnorVI_list[[class_names[i]]]=as.data.frame(read.csv(paste("res_1Aug/",class_names[i],type,sep=""),sep="\t",header = T))
}



#
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))
for(i in 7:9){
  boxplot(norVI_list[[class_names[i]]][8:16], names = pairname, xlab="Variable Pairs", ylab="Pairwise PVI", 
          main = paste("Nor Pairwise PVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,1.1),cex.axis=0.8)
  axis(2, at = round(seq(-0.1, 4, by = 0.2),2),las=2)
  boxplot(MnorVI_list[[class_names[i]]][8:16], names =pairname,xlab="Variable Pairs", ylab="Pairwise MVI",
          main = paste("Nor Pairwise MVI Scores in ",class_names[i]," Datasets",sep=""),yaxt = "n",ylim=c(-0.1,1.1),cex.axis=0.8)
  axis(2, at = round(seq(-0.1, 4, by = 0.2),2),las=2)
}

# HA1/10 vs HA2/9
for(i in 4:6){
  su=t.test((norVI_list[[class_names[i]]]$X10+norVI_list[[class_names[i]]]$X100),norVI_list[[class_names[i+3]]]$X10+norVI_list[[class_names[i+3]]]$X100, alternative = "two.sided",paired = F,var.equal=T)
  print(su)
}


# compair of 5, 10, 100 in HA2
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

for(i in 1:3){
  boxplot(cbind(norVI_list[[class_names[7]]][i],norVI_list[[class_names[8]]][i],norVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Datasets", ylab="PnorVI", 
          main = paste("PnorVI Scores of ",colnames(MnorVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(MnorVI_list[[class_names[7]]][i],MnorVI_list[[class_names[8]]][i],MnorVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Datasets", ylab="MnorVI", 
          main = paste("MnorVI Scores of ",colnames(MnorVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n")
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}

# PnorVI in X5, 10, 100
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(cbind(norVI_list[[class_names[4]]][i],norVI_list[[class_names[5]]][i],norVI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Dataset Type", ylab="PnorVI", 
          main = paste("PnorVI Scores of ",colnames(norVI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.01,1))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(norVI_list[[class_names[7]]][i],norVI_list[[class_names[8]]][i],norVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PnorVI", 
          main = paste("PnorVI Scores of ",colnames(MnorVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.01,1))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
# MnorVI
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(cbind(MnorVI_list[[class_names[4]]][i],MnorVI_list[[class_names[5]]][i],MnorVI_list[[class_names[6]]][i]), names =class_names[4:6],xlab="Dataset Type", ylab="MnorVI", 
          main = paste("MnorVI Scores of ",colnames(norVI_list$H0N[i])," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}
for(i in 1:3){
  boxplot(cbind(MnorVI_list[[class_names[7]]][i],MnorVI_list[[class_names[8]]][i],MnorVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MnorVI", 
          main = paste("MnorVI Scores of ",colnames(MnorVI_list$H0N[i])," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(-0.1,0.8))
  axis(2, at = round(seq(0, 1, by = 0.2),2),las=2)
}


# MnorVI and PnorVI are similar when the correlation get stronger, they reduce similar amount, when there's not interaction
# X10
for(i in c(4,7)){
  su=t.test(norVI_list[[class_names[i+2]]][2]-norVI_list[[class_names[i]]][2],MnorVI_list[[class_names[i+2]]][2]-MnorVI_list[[class_names[i]]][2])
  print(su$p.value)
  #print(su$estimate)
}
# X5
for(i in c(4,7)){
  su=t.test(norVI_list[[class_names[i+2]]][1]-norVI_list[[class_names[i]]][1],MnorVI_list[[class_names[i+2]]][1]-MnorVI_list[[class_names[i]]][1])
  print(su$p.value)
  #print(su$estimate)
}
# X100
for(i in c(7)){
  su=t.test(norVI_list[[class_names[i+2]]][3]-norVI_list[[class_names[i]]][3],MnorVI_list[[class_names[i+2]]][3]-MnorVI_list[[class_names[i]]][3])
  print(su$p.value)
  #print(su$estimate)
}
# when correlation and interaction both exist, MnorVI and PnorVI react slightly different

# pairwise result
ttags=c("X5 & X10","X5 & X100",0,"X10 & X100")
# compair of 5*10,5*100, 10x100 in HA2
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in c(8,9,11)){
  boxplot(cbind(norVI_list[[class_names[7]]][i],norVI_list[[class_names[8]]][i],norVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PnorVI", 
          main = paste("Pairwise PnorVI of ",ttags[i-7]," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.4))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
for(i in c(8,9,11)){
  boxplot(cbind(MnorVI_list[[class_names[7]]][i],MnorVI_list[[class_names[8]]][i],MnorVI_list[[class_names[9]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MnorVI", 
          main = paste("Pairwise MnorVI of ",ttags[i-7]," in HA2",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1.7))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
# HA1
ttags=c("X5 & X10","X5 & X100",0,"X10 & X100")
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in c(8,9,11)){
  boxplot(cbind(norVI_list[[class_names[4]]][i],norVI_list[[class_names[5]]][i],norVI_list[[class_names[6]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="PnorVI", 
          main = paste("Pairwise PnorVI of ",ttags[i-7]," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
for(i in c(8,9,11)){
  boxplot(cbind(MnorVI_list[[class_names[4]]][i],MnorVI_list[[class_names[5]]][i],MnorVI_list[[class_names[6]]][i]), names =class_names[7:9],xlab="Dataset Type", ylab="MnorVI", 
          main = paste("Pairwise MnorVI of ",ttags[i-7]," in HA1",sep=""),col = c("coral", "greenyellow","deepskyblue"),
          yaxt = "n",ylim=c(0,1))
  axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
