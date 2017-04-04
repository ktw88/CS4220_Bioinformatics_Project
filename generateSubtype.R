library(gplots)


brcaExpr<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/data/Normalization.BRCA.protCoding.minAveExpr10.maxExpr50.txt", sep="\t")


zScoreNorm<-function(data){
  meanVal<-mean(data)
  data.norm<-data - meanVal
  dataStdDev<-sd(data.norm)
  zScore<-data.norm / dataStdDev
  
  return(as.numeric(zScore))
}

stanDev<-apply(brcaExpr[,3:1258], 1, sd)
#dataZscore<-t(apply(brcaExpr[,3:1258], 1, zScoreNorm))
dataZscore<-t(apply(brcaExpr[,3:1258], 1, zScoreNorm))
dataZscore.anno<-cbind(brcaExpr[,c(1:2)], dataZscore)
names(dataZscore.anno)<-names(brcaExpr)


stanDev<-apply(dataZscore.anno[,3:1258], 1, sd)



#dataZscore<-t(apply(brcaExpr[1:10,3:1258], 1, zScoreNorm))

a<-apply(brcaExpr[1:10,3:1258], 1, zScoreNorm)







# Plots


# need provide clinical information to differtiate tum and norm

hist(stanDev, breaks=1000)

