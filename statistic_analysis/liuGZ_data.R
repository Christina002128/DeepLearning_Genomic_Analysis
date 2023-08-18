# Liu Result
files <- list.files(path="LiuGZ_result/lgzdata",pattern = "result")
liu_f<-paste("LiuGZ_result/lgzdata/",files,sep="")
liu_list=list()
for(i in 1:9){
  liu_list[[i]] <-as.data.frame(read.csv(liu_f[i],sep=",",header = T))
  liu_list[[i]] <- liu_list[[i]][,c(c(3052:3055),c(2:11),51,101,151,201,251,252)]
}

liu_class <- gsub("_m1_r_result.csv", "", files)
names(liu_list) <-liu_class

liu_r2=c()
liu_mae=c()
liu_rmse=c()
for(i in 1:9){
  liu_r2 = cbind(liu_r2,as.numeric(liu_list[[i]][,1]))
  liu_rmse = cbind(liu_rmse,as.numeric(liu_list[[i]][,2]))
  liu_mae = cbind(liu_mae,as.numeric(liu_list[[i]][,4]))
}
colnames(liu_mae) <-liu_class
liu_mae <- liu_mae[,c(1,4,7,2,5,8,3,6,9)]
colnames(liu_r2) <-liu_class
liu_r2 <- liu_r2[,c(1,4,7,2,5,8,3,6,9)]
colnames(liu_rmse) <-liu_class
liu_rmse <- liu_rmse[,c(1,4,7,2,5,8,3,6,9)]

# performance

par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
boxplot(liu_r2, names = class_names, xlab="Dataset Types", ylab="R2", main = "R2 Scores (DBN)",
        col=boxcolors,yaxt = "n",cex.axis = 0.8)
axis(2, at = round(seq(-2, 1, by = 0.4),2),las=2)
boxplot(liu_rmse, names = class_names, xlab="Dataset Types", ylab="RMSE", main = "RMSE Scores (DBN)",
        col=boxcolors,yaxt = "n",cex.axis = 0.8)
axis(2, at = round(seq(-2, 5, by = 0.4),2),las=2)

boxplot(liu_mae, names = class_names, xlab="Dataset Types", ylab="MAE", main = "MAE Scores (DBN)",
        col=boxcolors,yaxt = "n",cex.axis = 0.8)
axis(2, at = round(seq(-2, 5, by = 0.4),2),las=2)


#ttest
for(i in c(1,4,7)){
  su=t.test(liu_r2[,i],liu_r2[,i+1],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_r2[,i],liu_r2[,i+2],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_r2[,i+2],liu_r2[,i+1],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}

for(i in c(1)){
  su=t.test(c(liu_r2[,i],liu_r2[,i+1],liu_r2[,i+2]),
            c(liu_r2[,i+3],liu_r2[,i+4],liu_r2[,i+5]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(c(liu_r2[,i],liu_r2[,i+1],liu_r2[,i+2]),
            c(liu_r2[,i+6],liu_r2[,i+7],liu_r2[,i+8]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(c(liu_r2[,i+6],liu_r2[,i+7],liu_r2[,i+8]),
            c(liu_r2[,i+3],liu_r2[,i+4],liu_r2[,i+5]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}

#MVI
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
class=c("H0N","HA1N","HA2N","H0W","HA1W","HA2W","H0S","HA1S","HA2S")
for(i in c(1,4,7)){
  boxplot(liu_list[[i]][,c(c(5:9),16,18,19)], names = paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="MVI",
          main = paste("MVI Scores in ",class[i],sep=""),yaxt = "n",ylim=c(-0.1,0.1))
  axis(2, at = round(seq(-2, 2, by = 0.05),2),las=2)}
for(i in c(2,5,8)){
  boxplot(liu_list[[i]][,c(c(5:9),16,18,19)], names =paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="MVI", col=c("orange","lightgreen","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("MVI Scores in ",class[i],sep=""))
  #ran=(max(liu_list[[i]][c(c(5:9),16,18,19)])-min(liu_list[[i]][c(c(5:9),16,18,19)]))/10
  #axis(2, at = round(seq(-2, 2, by =round(ran,2)),2),las=2)
}
for(i in c(3,6,9)){
  boxplot(liu_list[[i]][,c(c(5:9),16,18,19)], names =paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="MVI", col=c("orange","orange","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("MVI Scores in ",class[i],sep=""))
  #axis(2, at = round(seq(-2, 2, by = 0.1/(i-6)),2),las=2)
}

# MVI t test
for(i in c(1,4,6,2,5,8)){
  su=t.test(liu_list[[i]][,5],liu_list[[i]][,c(6:14)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_list[[i]][,6:14],liu_list[[i]][,c(14:19)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
for(i in c(3,6,9)){
  su=t.test(liu_list[[i]][,c(5)],liu_list[[i]][,c(6)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_list[[i]][,c(5:6)],liu_list[[i]][,c(7:14)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_list[[i]][,c(7:14)],liu_list[[i]][,c(14:19)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}


# add vs pair
par(mfrow = c(1, 3), mar = c(3, 4, 5, 2))
for(i in c(3,6,9)){
  boxplot(cbind(liu_list[[i]][5]+liu_list[[i]][6],liu_list[[i]][20]),
          names =c("Add(X1+X2)","Pair(X1&X2)"),ylab="MVI", # ylim=c(0.2,1.63),yaxt = "n",
          cex.axis = 0.9,col=c("grey","orange"),main=paste("Pairwise MVI in",class_names[i],"(DBN)"))
  #axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
dev.off()
# pair ttest
for(i in c(3,6,9)){
  su=t.test(liu_list[[i]][1]+liu_list[[i]][2],liu_list[[i]][,20],var.equal=T,paired=F,alternative = c("two.sided"))
  print(su)
}
for(i in c(3,6,9)){
  a1=liu_list[[i]][1]+liu_list[[i]][2]-liu_list[[i]][,20]
  su=t.test(x=a1,alternative = c("two.sided"))
  print(su)
}



## PVI
liu_Plist=list()
for(i in 1:9){
  liu_Plist[[i]] <-as.data.frame(read.csv(liu_f[i],sep=",",header = T))[1527:3055]
  liu_Plist[[i]] <- liu_Plist[[i]][,c(c(1520:1523),c(1:10),50,100,150,200,250,251)]
}
names(liu_Plist) <-liu_class
#PVI
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
class=c("H0N","HA1N","HA2N","H0W","HA1W","HA2W","H0S","HA1S","HA2S")
for(i in c(1,4,7)){
  boxplot(liu_Plist[[i]][,c(c(5:9),16,18,19)], names = paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="PVI",
          main = paste("PVI Scores in ",class[i],sep=""),yaxt = "n",ylim=c(-0.1,0.1))
  axis(2, at = round(seq(-2, 2, by = 0.05),2),las=2)}
for(i in c(2,5,8)){
  boxplot(liu_Plist[[i]][,c(c(5:9),16,18,19)], names =paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="PVI", col=c("orange","lightgreen","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("PVI Scores in ",class[i],sep=""))
  #ran=(max(liu_Plist[[i]][c(c(5:9),16,18,19)])-min(liu_Plist[[i]][c(c(5:9),16,18,19)]))/10
  #axis(2, at = round(seq(-2, 2, by =round(ran,2)),2),las=2)
}
for(i in c(3,6,9)){
  boxplot(liu_Plist[[i]][,c(c(5:9),16,18,19)], names =paste("X",c(c(1:5),100,200,250),sep=""),
          xlab="Variables", ylab="PVI", col=c("orange","orange","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("PVI Scores in ",class[i],sep=""))
  #axis(2, at = round(seq(-2, 2, by = 0.1/(i-6)),2),las=2)
}

# PVI t test
for(i in c(1,4,6)){
  su=t.test(liu_Plist[[i]][,5:14],liu_Plist[[i]][,c(14:19)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
for(i in c(1,4,6,2,5,8)){
  su=t.test(liu_Plist[[i]][,5],liu_Plist[[i]][,c(6:14)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_Plist[[i]][,6:14],liu_Plist[[i]][,c(14:19)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
for(i in c(3,6,9)){
  su=t.test(liu_Plist[[i]][,c(5)],liu_Plist[[i]][,c(6)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_Plist[[i]][,c(5:6)],liu_Plist[[i]][,c(7:14)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(liu_Plist[[i]][,c(7:14)],liu_Plist[[i]][,c(14:19)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
# add vs pair
par(mfrow = c(1, 3), mar = c(3, 4, 5, 2))
for(i in c(3,6,9)){
  boxplot(cbind(liu_list[[i]][5]+liu_list[[i]][6],liu_list[[i]][20]),
          names =c("Add(X1+X2)","Pair(X1&X2)"),ylab="PVI", # ylim=c(0.2,1.63),yaxt = "n",
          cex.axis = 0.9,col=c("grey","orange"),main=paste("Pairwise PVI in",class[i],"(DBN)"))
  #axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
}
dev.off()
# pair ttest
for(i in c(3,6,9)){
  su=t.test(liu_Plist[[i]][1]+liu_Plist[[i]][2],liu_Plist[[i]][,20],var.equal=T,paired=F,alternative = c("two.sided"))
  print(su)
}
for(i in c(3,6,9)){
  a1=liu_Plist[[i]][1]+liu_Plist[[i]][2]-liu_Plist[[i]][,20]
  su=t.test(x=a1,alternative = c("two.sided"))
  print(su)
}
