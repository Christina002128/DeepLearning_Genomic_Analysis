#xueying data
files <- list.files(path="Xueying_result/Original_performance",pattern = ".txt")
xue_f<-paste("Xueying_result/Original_performance/",files,sep="")

# R2 score
updated_vector <- gsub("single_assoc", "HA1", files)
updated_vector <- gsub("no_assoc", "H0", updated_vector)
updated_vector <- gsub("interact_assoc", "HA2", updated_vector)
updated_vector <- gsub("_corr_original.txt", "", updated_vector)
updated_vector <- gsub("_no", "N", updated_vector)
updated_vector <- gsub("_weak", "W", updated_vector)
updated_vector <- gsub("_strong", "S", updated_vector)
xue_r2=matrix( nrow = 1000,ncol = 9)
colnames(xue_r2) <-updated_vector 
for(i in 1:9){
  xue_r2[,i]=as.matrix(read.csv(xue_f[i],sep="\t",header = T))
}
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
order_xue_r2 <- xue_r2[,class_names]

# R2 score
class=c("H0","HA1","HA2")
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))
boxplot(order_xue_r2, names = class_names,xlab="Dataset Types", ylab="R2", main = "R2 Scores (mlpSGD)",
        col=boxcolors,cex.axis = 0.9,yaxt = "n")
axis(2, at = round(seq(-1.2, 1, by = 0.2),2),las=2)
boxplot(liu_r2, names = class_names, xlab="Dataset Types", ylab="R2", main = "R2 Scores (DBN)",
        col=boxcolors,yaxt = "n",cex.axis = 0.8)
axis(2, at = round(seq(-2, 1, by = 0.4),2),las=2)
#ttest
for(i in c(1,4,7)){
su=t.test(order_xue_r2[,i],order_xue_r2[,i+1],alternative = "two.sided",var.equal=T,paired=F)
print(su$p.value)
su=t.test(order_xue_r2[,i],order_xue_r2[,i+2],alternative = "two.sided",var.equal=T,paired=F)
print(su$p.value)
su=t.test(order_xue_r2[,i+2],order_xue_r2[,i+1],alternative = "two.sided",var.equal=T,paired=F)
print(su$p.value)
}

for(i in c(1)){
  su=t.test(c(order_xue_r2[,i],order_xue_r2[,i+1],order_xue_r2[,i+2]),
            c(order_xue_r2[,i+3],order_xue_r2[,i+4],order_xue_r2[,i+5]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(c(order_xue_r2[,i],order_xue_r2[,i+1],order_xue_r2[,i+2]),
            c(order_xue_r2[,i+6],order_xue_r2[,i+7],order_xue_r2[,i+8]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(c(order_xue_r2[,i+6],order_xue_r2[,i+7],order_xue_r2[,i+8]),
            c(order_xue_r2[,i+3],order_xue_r2[,i+4],order_xue_r2[,i+5]),
            alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}


# PVI
filesPVI <- list.files(path="Xueying_result/VIM_results",pattern = "PVI.txt")
xue_f_PVI<-paste("Xueying_result/VIM_results/",filesPVI,sep="")

updated_vector2 <- gsub("single_assoc", "HA1", filesPVI)
updated_vector2 <- gsub("no_assoc", "H0", updated_vector2)
updated_vector2 <- gsub("interact_assoc", "HA2", updated_vector2)
updated_vector2 <- gsub("_corr_PVI.txt", "", updated_vector2)
updated_vector2 <- gsub("_no", "N", updated_vector2)
updated_vector2 <- gsub("_weak", "W", updated_vector2)
updated_vector2 <- gsub("_strong", "S", updated_vector2)
xue_PVI=list()
for(i in 1:9){
  xue_PVI[[updated_vector2[i]]]=as.data.frame(read.csv(xue_f_PVI[i],sep="\t",header = T))
}
# MVI
filesMVI <- list.files(path="Xueying_result/VIM_results",pattern = "MVI.txt")
xue_f_MVI<-paste("Xueying_result/VIM_results/",filesMVI,sep="")

updated_vector2 <- gsub("single_assoc", "HA1", filesMVI)
updated_vector2 <- gsub("no_assoc", "H0", updated_vector2)
updated_vector2 <- gsub("interact_assoc", "HA2", updated_vector2)
updated_vector2 <- gsub("_corr_MVI.txt", "", updated_vector2)
updated_vector2 <- gsub("_no", "N", updated_vector2)
updated_vector2 <- gsub("_weak", "W", updated_vector2)
updated_vector2 <- gsub("_strong", "S", updated_vector2)
xue_MVI=list()
for(i in 1:9){
  xue_MVI[[updated_vector2[i]]]=as.data.frame(read.csv(xue_f_MVI[i],sep="\t",header = T))
}


# PVI 
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(xue_PVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="PVI",
          main = paste("PVI Scores in ",class_names[i],sep=""),yaxt = "n",ylim=c(-0.7,0.81))
  axis(2, at = round(seq(-2, 2, by = 0.2),2),las=2)}
for(i in 4:6){
  boxplot(xue_PVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="PVI", col=c("orange","lightgreen","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("PVI Scores in ",class_names[i],sep=""),yaxt = "n",ylim=c(-0.7,0.81))
  axis(2, at = round(seq(-2, 2, by = 0.2),2),las=2)}
for(i in 7:9){
  boxplot(xue_PVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="PVI", col=c("orange","orange","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("PVI Scores in ",class_names[i],sep=""),yaxt = "n",ylim=c(min(xue_PVI[[class_names[i]]][c(c(1:5),50,100,150)]),0.81))
  axis(2, at = round(seq(-2, 2, by = 0.2),2),las=2)}

#MVI
par(mfrow = c(3, 3), mar = c(4, 4, 3, 2))
for(i in 1:3){
  boxplot(xue_MVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="MVI",
          main = paste("MVI Scores in ",class_names[i],sep=""),yaxt = "n",ylim=c(-0.7,0.81))
  axis(2, at = round(seq(-2, 2, by = 0.2),2),las=2)}
for(i in 4:6){
  boxplot(xue_MVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="MVI", col=c("orange","lightgreen","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("MVI Scores in ",class_names[i],sep=""))
  #ran=(max(xue_MVI[[class_names[i]]][c(c(1:5),50,100,150)])-min(xue_MVI[[class_names[i]]][c(c(1:5),50,100,150)]))/10
  #axis(2, at = round(seq(-2, 2, by =round(ran,2)),2),las=2)
  }
for(i in 7:9){
  boxplot(xue_MVI[[class_names[i]]][c(c(1:5),50,100,150)], names =paste("X",c(c(1:5),50,100,150),sep=""),
          xlab="Variables", ylab="MVI", col=c("orange","orange","lightgreen","lightgreen","lightgreen","grey","grey","grey"),
          main = paste("MVI Scores in ",class_names[i],sep=""))
  #axis(2, at = round(seq(-2, 2, by = 0.1/(i-6)),2),las=2)
  }

# PVI t test
for(i in c(1:6)){
  su=t.test(xue_PVI[[class_names[i]]][1],xue_PVI[[class_names[i]]][c(2:20)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(xue_PVI[[class_names[i]]][2:20],xue_PVI[[class_names[i]]][c(21:150)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
for(i in c(7:9)){
  su=t.test(xue_PVI[[class_names[i]]][c(1:2)],xue_PVI[[class_names[i]]][c(3:20)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(xue_PVI[[class_names[i]]][c(3:20)],xue_PVI[[class_names[i]]][c(21:150)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}


# MVI t test
for(i in c(1:6)){
  su=t.test(xue_MVI[[class_names[i]]][1],xue_MVI[[class_names[i]]][c(2:20)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(xue_MVI[[class_names[i]]][2:20],xue_MVI[[class_names[i]]][c(21:150)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
for(i in c(7:9)){
  su=t.test(xue_MVI[[class_names[i]]][c(1:2)],xue_MVI[[class_names[i]]][c(3:20)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
  su=t.test(xue_MVI[[class_names[i]]][c(3:20)],xue_MVI[[class_names[i]]][c(21:150)],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}


par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 4:6){
  boxplot(xue_PVI[[class_names[i]]][1:25], names =colnames(xue_PVI[[class_names[i]]])[1:25],xlab="Variables", ylab="MVI", 
          main = paste("PVI Scores in ",class_names[i],"",sep=""),yaxt = "n")
  axis(2, at = round(seq(-1, 1, by = 0.2),2),las=2)}
for(i in 4:6){
  boxplot(xue_MVI[[class_names[i]]][1:25], names =colnames(xue_MVI[[class_names[i]]])[1:25],xlab="Variables", ylab="MVI", 
          main = paste("MVI Scores in ",class_names[i],"",sep=""),yaxt = "n")
  axis(2, at = round(seq(-1, 1, by = 0.2),2),las=2)}
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
for(i in 7:9){
  boxplot(xue_PVI[[class_names[i]]][1:25], names =colnames(xue_PVI[[class_names[i]]])[1:25],xlab="Variables", ylab="MVI", 
          main = paste("PVI Scores in ",class_names[i],"",sep=""),yaxt = "n")
  axis(2, at = round(seq(-1, 1, by = 0.2),2),las=2)}
for(i in 7:9){
  boxplot(xue_MVI[[class_names[i]]][1:25], names =colnames(xue_MVI[[class_names[i]]])[1:25],xlab="Variables", ylab="MVI", 
          main = paste("MVI Scores in ",class_names[i],"",sep=""),yaxt = "n")
  axis(2, at = round(seq(-1, 1, by = 0.2),2),las=2)}


# pairwise MVI
xue_pairMVI = matrix( nrow = 1000,ncol = 3)
filesxp <- list.files(path="Xueying_result/Archive/",pattern = ".txt")
xue_fxp<-paste("Xueying_result/Archive/",filesxp,sep="")
for(i in 1:3){
  xue_pairMVI[,i]=as.matrix(read.csv(xue_fxp[i],sep="\t",header = T))
}
colnames(xue_pairMVI)=c("HA2N","HA2S","HA2W")
xue_pairMVI <- xue_pairMVI[,c("HA2N","HA2W","HA2S")]
par(mfrow = c(1,1), mar = c(4, 4, 3, 2))
boxplot(xue_pairMVI, xlab="Variables", ylab="Pairwise_MVI", 
        main = "Pairwise MVI Scores in HA2",yaxt = "n")
axis(2, at = round(seq(-1, 1, by = 0.2),2),las=2)


# add vs pair
par(mfrow = c(1, 3), mar = c(3, 4, 5, 2))
for(i in 7:9){
  boxplot(cbind(xue_MVI[[class_names[i]]][1]+xue_MVI[[class_names[i]]][2],xue_pairMVI[,i-6]),
          names =c("Add(X1+X2)","Pair(X1&X2)"),xlab=class_names[i], ylab="MVI", # ylim=c(0.2,1.63),yaxt = "n",
          cex.axis = 0.9,col=c("grey","orange"),main=paste("Pairwise MVI in",class_names[i],"(mlpSGD)"))
  #axis(2, at = round(seq(0, 2, by = 0.2),2),las=2)
  }
#mtext("Comparison of Addition of MVIs and Pairwise MVI of X10 & X100 in HA2\n       HA2N                                                HA2W                                                  HA2S", 
#      side = 3, line =-24, outer = T)
dev.off()

for(i in 7:9){
su=t.test(xue_MVI[[class_names[i]]][1]+xue_MVI[[class_names[i]]][2],xue_pairMVI[,i-6],var.equal=T,paired=F,alternative = c("two.sided"))
print(su)
}
for(i in 7:9){
  a1=xue_MVI[[class_names[i]]][1]+xue_MVI[[class_names[i]]][2]-xue_pairMVI[,i-6]
  su=t.test(x=a1,alternative = c("two.sided"))
  print(su)
}
