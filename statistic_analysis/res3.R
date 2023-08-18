#Result3
path="res_20July/"
class_names=c("H0N","H0W","H0S","HA1N","HA1W","HA1S","HA2N","HA2W","HA2S")
eval_file = paste(path,class_names,"_evaluate_results.csv",sep="")
eval_list=list()
for(i in 1:9){
  eval_list[[class_names[i]]]=as.data.frame(read.csv(eval_file[i],sep="\t",header = T))
}

for(i in 7:9){
 eval_list[[class_names[i]]]=as.data.frame(
   read.csv(paste("res_1Aug/",class_names[i],"_evaluate_results.csv",sep=""),sep="\t",header = T))
}

mae=NULL
rmse=NULL
r2=NULL
ev=NULL
for(i in 1:9){
  mae = cbind(mae,eval_list[[class_names[i]]]$MAE)
  rmse = cbind(rmse,eval_list[[class_names[i]]]$RMSE)
  r2 = cbind(r2,eval_list[[class_names[i]]]$R.squared)
  ev = cbind(ev,eval_list[[class_names[i]]]$EV)
}
eval_list$mae=as.data.frame(mae)
eval_list$rmse=as.data.frame(rmse)
eval_list$r2=as.data.frame(r2)
eval_list$ev=as.data.frame(ev)
names(eval_list)

for(i in 10:13){
  colnames(eval_list[[i]])=class_names
}


colors <- c("red", "blue", "green", "orange", "purple", "yellow", "pink", "brown", "cyan")
# R2 score
par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
class=c("H0","HA1","HA2")
plot(density(eval_list[[class_names[1]]][,evals]), col = colors[1],lwd=2,
     xlim = c(-1,max(eval_list[[class_names[1]]][,evals],eval_list[[class_names[2]]][,evals],eval_list[[class_names[3]]][,evals])),
     ylim = c(0,max(density(eval_list[[class_names[1]]][,evals])$y,density(eval_list[[class_names[2]]][,evals])$y,density(eval_list[[class_names[3]]][,evals])$y)),
     main=paste("R2 Score in ",class[1],sep=""),cex.main =1, xlab = "r2 score", ylab = "Density")
lines(density(eval_list[[class_names[2]]][,evals]), col = colors[2],lwd=2)
lines(density(eval_list[[class_names[3]]][,evals]), col = colors[3],lwd=2)
legend("topright", legend = class_names[c(1,2,3)], 
       col = colors[1:3],  bty = "n",lwd=2, cex=0.7)
k=1
for( i in c(4,7)){
  k=k+1
  plot(density(eval_list[[class_names[i]]][,evals]), col = colors[i],lwd=2,
     xlim = c(min(eval_list[[class_names[i]]][,evals],eval_list[[class_names[i+1]]][,evals],eval_list[[class_names[i+2]]][,evals]),
              max(eval_list[[class_names[i]]][,evals],eval_list[[class_names[i+1]]][,evals],eval_list[[class_names[i+2]]][,evals])),
     ylim = c(0,max(density(eval_list[[class_names[i]]][,evals])$y,density(eval_list[[class_names[i+1]]][,evals])$y,density(eval_list[[class_names[i+2]]][,evals])$y)),
     main=paste("R2 Score in ",class[k],sep=""),cex.main =1, xlab = "r2 score", ylab = "Density")
  lines(density(eval_list[[class_names[i+1]]][,evals]), col = colors[i+1],lwd=2)
  lines(density(eval_list[[class_names[i+2]]][,evals]), col = colors[i+2],lwd=2)
  legend("topright", legend = class_names[c(i,i+1,i+2)], 
         col = colors[c(i,i+1,i+2)],  bty = "n",lwd=2, cex=0.7)
}


boxcolors <- c("blue","blue", "blue","red","red","red", "green","green", "green")

# box plot of performance
png("result_scores.png",height = 550,width = 480)
par(mfrow = c(2, 2),mar = c(4, 5, 4, 2))
boxplot(eval_list$r2[1:9], names = class_names[1:9],xlab="Dataset Types", ylab="R2 Scores", main = "R2 Scores",col=boxcolors,cex.axis = 0.7,yaxt = "n")
axis(2, at = round(seq(-1.2, 0.8, by = 0.3),2),cex.axis = 0.7,las=2)
boxplot(eval_list$ev[1:9], names = class_names[1:9],xlab="Dataset Types", ylab="EV Scores", main = "EV Scores",col=boxcolors,cex.axis = 0.7,yaxt = "n")
axis(2, at = round(seq(-1.2, 1, by = 0.3),2),cex.axis = 0.7,las=2)

boxplot(eval_list$mae[1:9], names = class_names[1:9],xlab="Dataset Types", ylab="MAE Scores", main = "MAE Scores",col=boxcolors,cex.axis = 0.7)
boxplot(eval_list$rmse[1:9], names = class_names[1:9],xlab="Dataset Types", ylab="RMSE Scores", main = "RMSE Scores",col=boxcolors,cex.axis = 0.7)

dev.off()

# all ev higher than R2 (only discussion)
for(i in 1:9){
  data_eval = cbind(c(1:1000),eval_list$r2[i],eval_list$ev[i])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T)
  cat(round(su$p.value,5),"\n")
}

# ma, rmse, r2, ev between diff types
#c(10,11,12,13) 
k=13
for(i in c(1,4,7)){
  data_eval = cbind(c(1:1000),eval_list[[k]][i],eval_list[[k]][i+1])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T,paired=F)
  cat(su$p.value,"\n")
  data_eval = cbind(c(1:1000),eval_list[[k]][i],eval_list[[k]][i+2])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T,paired=F)
  cat(su$p.value,"\n")
  data_eval = cbind(c(1:1000),eval_list[[k]][i+1],eval_list[[k]][i+2])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T,paired=F)
  cat(su$p.value,"\n")
}


# average of each big type
for(i in c(1,4,7)){
  data_r2=unlist(c(eval_list$r2[i],eval_list$r2[i+1],eval_list$r2[i+2]))
  cat(round(mean(data_r2),2),round(sd(data_r2),2),"\n")
}
for(i in c(1,4,7)){
data_ev=unlist(c(eval_list$ev[i],eval_list$ev[i+1],eval_list$ev[i+2]))
cat(round(mean(data_ev),2),round(sd(data_ev),2),"\n")
}

# H0 vs HA1, H0 vs HA2
for(i in 1:3){
  data_eval = cbind(c(1:1000),eval_list$ev[i],eval_list$ev[i+3])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T)
  cat("H0 vs HA1",su$p.value,"\n")
  data_eval = cbind(c(1:1000),eval_list$ev[i],eval_list$ev[i+6])
  su = t.test(data_eval[,2],data_eval[,3],alternative = "two.sided",var.equal=T)
  cat("H0 vs HA2",su$p.value,"\n")
}

# HA1 vs HA2
t.test(eval_list$r2[c(4:6)],eval_list$r2[c(7:9)],alternative = "two.sided",var.equal=T,paired=F)
t.test(eval_list$ev[c(4:6)],eval_list$ev[c(7:9)],alternative = "two.sided",var.equal=T,paired=F)
t.test(eval_list[[11]][c(4:6)],eval_list[[11]][c(7:9)],alternative = "two.sided",var.equal=T,paired=F)
t.test(eval_list[[12]][c(4:6)],eval_list[[12]][c(7:9)],alternative = "two.sided",var.equal=T,paired=F)


# all r2 lower than EV in every replicates
all_r2=unlist(eval_list$r2[c(4:9)])
all_ev=unlist(eval_list$ev[c(4:9)])
length(all_r2[all_r2<all_ev])

# no need
boxplot(eval_list$r2[4:6], names = class_names[4:6],xlab="Dataset Types", ylab="R2 Scores", main = "R2 Scores in HA1 Datasets",cex.axis = 0.7)
boxplot(eval_list$r2[7:9], names = class_names[7:9],xlab="Dataset Types", ylab="R2 Scores", main = "R2 Scores in HA2 Datasets",cex.axis = 0.7)
boxplot(eval_list$ev[4:6], names = class_names[4:6],xlab="Dataset Types", ylab="EV Scores", main = "EV Scores in HA1 Datasets",cex.axis = 0.7)
boxplot(eval_list$ev[7:9], names = class_names[7:9],xlab="Dataset Types", ylab="EV Scores", main = "EV Scores in HA2 Datasets",cex.axis = 0.7)

boxplot(eval_list$mae[4:6], names = class_names[4:6],xlab="Dataset Types", ylab="MAE Scores", main = "MAE Scores in HA1 Datasets",cex.axis = 0.7)
boxplot(eval_list$mae[7:9], names = class_names[7:9],xlab="Dataset Types", ylab="MAE Scores", main = "MAE Scores in HA2 Datasets",cex.axis = 0.7)
boxplot(eval_list$rmse[4:6], names = class_names[4:6],xlab="Dataset Types", ylab="RMSE Scores", main = "RMSE Scores in HA1 Datasets",cex.axis = 0.7)
boxplot(eval_list$rmse[7:9], names = class_names[7:9],xlab="Dataset Types", ylab="RMSE Scores", main = "RMSE Scores in HA2 Datasets",cex.axis = 0.7)

sta=NULL
# stats
for(i in 1:9){
  sta=c(sta,paste(round(as.numeric(colMeans(eval_list$r2[i])),3) , "±", round(sd(as.matrix(eval_list$r2[i])),3)))
}
sta
#[1] "-0.231 ± 0.148" "-0.229 ± 0.153" "-0.235 ± 0.15"  "0.471 ± 0.079"  "0.458 ± 0.079" 
#[6] "0.495 ± 0.075"  "0.395 ± 0.089"  "0.404 ± 0.086"  "0.437 ± 0.081" 

eval_sta_file = paste(path,class_names,"_evaluate_stats.csv",sep="")
eval_sta_list=list()
for(i in 1:9){
  eval_sta_list[[class_names[i]]]=as.data.frame(read.csv(eval_sta_file[i],sep="\t",header = T))
}
for(i in 7:9){
  eval_sta_list[[class_names[i]]]=as.data.frame(
    read.csv(paste("res_1Aug/",class_names[i],"_evaluate_stats.csv",sep=""),sep="\t",header = T))
}
means=NULL
sds=NULL
for(i in 1:9){
  means = cbind(means,round(eval_sta_list[[class_names[i]]]$mean,2))
  sds = cbind(sds,round(eval_sta_list[[class_names[i]]]$std,2))
}
colnames(means)=class_names
colnames(sds)=class_names
rownames(means)=eval_sta_list[[class_names[1]]]$name
rownames(sds)=eval_sta_list[[class_names[1]]]$name


res_sta=matrix(paste(as.matrix(means),as.matrix(sds), sep = " ± "), nrow = nrow(as.matrix(means)))
colnames(res_sta)=class_names
rownames(res_sta)=eval_sta_list[[class_names[1]]]$name
write.csv(res_sta, "res_sta.csv", sep = "\t", quote = FALSE, row.names = T, col.names = T)


# within HA1 test
for(i in 10:13){
su=t.test(eval_list[[i]][6],eval_list[[i]][5],alternative = "two.sided",var.equal=T,paired=F)
print(su$p.value)
}
# within HA2 test
for(i in 10:13){
  su=t.test(eval_list[[i]][8],eval_list[[i]][9],alternative = "two.sided",var.equal=T,paired=F)
  print(su$p.value)
}
