file=c("results/HA1W_5VI.txt","results/HA1W_10VI.txt","results/HA1W_100VI.txt","results/HA1W_200VI.txt")
vi5=as.data.frame(read.csv(file[1],sep="\t",header = F))
vi10=as.data.frame(read.csv(file[2],sep="\t",header = F))
vi100=as.data.frame(read.csv(file[3],sep="\t",header = F))
vi200=as.data.frame(read.csv(file[4],sep="\t",header = F))
colnames(vi5)<-c('PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi10)<-c('PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi100)<-c('PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi200)<-c('PVI','MVI','normalised_PVI','normalised_MVI')

# the VI of each bias
plot(density(vi10$normalised_PVI), col = "blue", xlim = c(0,1),
     ylim = c(0, 8),
     main = "Density of PVI scores in HA1W", xlab = "VI score", ylab = "Density")
#lines(density(vi5$normalised_PVI), col = "red")
lines(density(vi100$normalised_PVI), col = "red")
lines(density(vi200$normalised_PVI), col = "green")
legend("topright", legend = c("10_PVI","100_PVI","200_PVI"), col = c("blue", "red", "green"), lty = 1)

plot(density(vi10$normalised_MVI), col = "blue", xlim = c(0,1),
     ylim = c(0, 8),
     main = "Density of MVI scores in HA1W", xlab = "VI score", ylab = "Density")
lines(density(vi10$normalised_MVI),col = "blue")
lines(density(vi100$normalised_MVI), col = "red")
lines(density(vi200$normalised_MVI), col = "green")
legend("topright", legend = c("10_MVI","100_MVI","200_MVI"), col = c("blue", "red", "green"), lty = 1)

#vi5_stat=c(mean(vi5$normalised_MVI) - sd(vi5$normalised_MVI),mean(vi5$normalised_MVI) + sd(vi5$normalised_MVI))
vi10_stat=c(mean(vi10$normalised_MVI) - sd(vi10$normalised_MVI),mean(vi10$normalised_MVI) + sd(vi10$normalised_MVI))
vi100_stat=c(mean(vi100$normalised_MVI) - sd(vi100$normalised_MVI),mean(vi100$normalised_MVI) + sd(vi100$normalised_MVI))
vi200_stat=c(mean(vi200$normalised_MVI) - sd(vi200$normalised_MVI),mean(vi200$normalised_MVI) + sd(vi200$normalised_MVI))

cat(vi5_stat,vi10_stat,vi100_stat,vi200_stat)


# Define the values for each part of the pie chart
parts <- c(mean(vi5$normalised_MVI), mean(vi10$normalised_MVI), mean(vi100$normalised_MVI), mean(vi200$normalised_MVI))


# Define the labels for each part
install.packages("scales")
library(scales)
l1=percent(parts/sum(parts))
l2 <- c("5_VI","10_VI","100_VI","200_VI")
lab=paste(l2,l1,sep=": ")
# Create the pie chart
pie(parts, labels = lab)

result <- t.test(vi10$normalised_PVI, vi100$normalised_PVI, alternative = "two.sided",paired = T,var.equal=T)
result

PVIs=list(vi5$normalised_PVI,vi10$normalised_PVI,vi100$normalised_PVI,vi200$normalised_PVI)
boxplot(PVIs, names = c("5_PVI","10_PVI","100_PVI","200_PVI","300_PVI","400_PVI","500_PVI"), 
        xlab = "SNPs", ylab = "PVI scores", main = "Box Plot of PVI scores in HA1W",col = c("pink",'lightblue',"lightgreen","yellow"))


# additive VI
vi5_10=as.data.frame(read.csv("results/HA1W_5_10VI.txt",sep="\t",header = F))
vi5_100=as.data.frame(read.csv("results/HA1W_5_100VI.txt",sep="\t",header = F))
vi5_200=as.data.frame(read.csv("results/HA1W_5_200VI.txt",sep="\t",header = F))
vi10_100=as.data.frame(read.csv("results/HA1W_10_100VI.txt",sep="\t",header = F))
vi10_200=as.data.frame(read.csv("results/HA1W_10_200VI.txt",sep="\t",header = F))
vi100_200=as.data.frame(read.csv("results/HA1W_100_200VI.txt",sep="\t",header = F))
colnames(vi5_10)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi5_100)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi5_200)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi10_100)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi10_200)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
colnames(vi100_200)<-c("NA",'PVI','MVI','normalised_PVI','normalised_MVI')
head(vi5_10)

add<- vi5$normalised_PVI + vi10$normalised_PVI
t.test(vi5_10$normalised_PVI,add, alternative = "two.sided",paired = F,var.equal=T)
cat(mean(vi5_10$normalised_PVI),mean(add))

pair_PVIs=list(vi5_10$normalised_PVI,vi5_100$normalised_PVI,vi5_200$normalised_PVI,
               vi10_100$normalised_PVI,vi10_200$normalised_PVI,vi100_200$normalised_PVI)
boxplot(pair_PVIs, names = c("5_10VI","5_100VI","5_200VI","10_100VI","10_200VI","100_200VI"),
        xlab = "Paired Variables", ylab = "Paired-PVI Scores", main = "Paired PVI Scores in HA1W",col = c("pink",'lightblue',"lightgreen","yellow"))

# pair VI all less than VI add up


#PCA
data=as.data.frame(read.csv("1.HA2S.csv",sep=",",header = T))
X=data[,c(1:500)]
X
pca <- prcomp(X, scale=T)
summary(pca,r)
pc <- pca_result$rotation

# Perform PCA
pca_result <- prcomp(X, center = TRUE, scale. = TRUE)
# Get the proportion of variance explained by each principal component
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
# Create a data frame for plotting
scree_df <- data.frame(Component = 1:10, Proportion = var_explained[1:10])
# Create the scree plot
#library(ggplot2)
ggplot(scree_df, aes(x = Component, y = Proportion)) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = "Principal Component", y = "Proportion of Variance Explained") +
  ggtitle("Scree Plot") +
  theme_minimal()

# Scores (projected data onto principal components)
loadings_squared <- pca_result$rotation[, 1]^2
# Calculate the proportion of variance explained by each feature
var_explained_features <- loadings_squared / sum(loadings_squared)
# Display the results
var_explained_features
index=c(1:500)
plot(index,var_explained_features)
index[var_explained_features>0.01]

s2d<-plot(pca$x[,1],pca$x[,2],col=colours, pch=19,xlab="PC1", ylab="PC2")
text(pca$x[,1], pca$x[,2], labels = colnames(values),pos
     = 3,offset = 0.5,cex=0.7)



# LD all 1000
#### start
args <- commandArgs(trailingOnly = TRUE)
#nf <- args[1]
nf=1
class<- c("H0N","H0W","H0S")
dataset_names=paste(nf,'.',class,'.csv',sep='')

for(cls in 1:3){
  gm <- as.data.frame(read.csv(dataset_names[cls]))
  gm<-gm[,1:500]
  ld_matrix <- matrix(0, nrow = 500, ncol = 500)
 for(i in 1:500){
  for(j in i:500){
    ld_matrix[i,j] =cor(gm[,i],gm[,j])
  }
}
write.csv(ld_matrix,paste(nf,".",class[cls],"_LD.csv",sep=''),row.names=F)
cat("file",paste(nf,".",class[cls],"_LD.csv",sep=''),"generated.\n")
}
##### end
# average matrix

file_list <- list.files(pattern = ".H0N_LD.csv")
for (file in file_list) {
  as.matrix(read.csv(file))
}
# LD plot
library(corrplot)
xx=read.csv(paste(nf,".",class[cls],"_LD.csv",sep=''))
ld_matrix=as.matrix(xx)
# Create the correlation plot
corrplot(ld_matrix[c(1:20),c(1:20)], method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", main = "Correlation Plot")

corrplot(ld_matrix, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", main = "Correlation Plot")


# no need
min_ld <- min(ld_matrix, na.rm = TRUE)
max_ld <- max(ld_matrix, na.rm = TRUE)
normalized_ld_matrix <- (ld_matrix - min_ld) / (max_ld - min_ld)
hist(xx)
#BiocManager::install("LDheatmap")
library(LDheatmap)
library(genetics)
# Create the LD plot
LDheatmap(xx, title = "LD Heatmap")
heatmap(ld_matrix, symm = TRUE,
        xlab = "SNP Index", ylab = "SNP Index", main = "Average LD Plot")


