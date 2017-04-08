library(dplyr)
library(gplots)

subtypeInfo<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/breastCancerSubtypeInformation.csv", sep="\t")
patientID<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/anno_sample.txt", sep="\t")
brcaExpr<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/data/Normalization.BRCA.protCoding.minAveExpr10.maxExpr50.txt", sep="\t", check.names = F)
names(brcaExpr)[1]<-"GeneID"
PAM50Genes<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/PAM50.geneList.txt", header=F)

# Groups
luminalGroup<-subtypeInfo$ER.Status == "Positive" | subtypeInfo$PR.Status == "Positive"
her2Group<-subtypeInfo$ER.Status == "Negative" & subtypeInfo$PR.Status == "Negative" & subtypeInfo$HER2.Final.Status == "Positive"
tripNegGroup<-subtypeInfo$ER.Status == "Negative" & subtypeInfo$PR.Status == "Negative" & subtypeInfo$HER2.Final.Status == "Negative"
subtypeInfo$group<-rep("NIL", nrow(subtypeInfo))
subtypeInfo$group[luminalGroup]<-"Luminal"
subtypeInfo$group[her2Group]<-"HER2"
subtypeInfo$group[tripNegGroup]<-"TripleNegative"
  




patientID$Complete.TCGA.ID<-substring(patientID$barcode, first = 1, last = 12)
combinedTable<-merge(x=subtypeInfo, y=patientID, all.x=TRUE)
write.table(combinedTable, "/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/clinicalSubtypeInfo.txt", sep="\t", row.names = F, quote = F)





combinedTable.Filter<-combinedTable[!combinedTable$group =="NIL",]
reqrSamples<-names(brcaExpr) %in% combinedTable.Filter$analysis_id


combinedTable.Filter$analysis_id




reqrGenes<-which(brcaExpr$GeneID %in% PAM50Genes$V1)

write.table(brcaExpr[reqrGenes,reqrSamples], "/home/kartong/Code/CS4220_Bioinformatics_Project/data/BRCA.PAM50expr.txt", sep="\t", row.names = F, quote = F)


brcaExprPAM50<-brcaExpr[reqrGenes,reqrSamples]

brcaExprPAM50.log10<-apply(brcaExprPAM50, 1, log10)



zScoreNorm<-function(data){
  meanVal<-mean(data)
  data.norm<-data - meanVal
  dataStdDev<-sd(data.norm)
  zScore<-data.norm / dataStdDev
  
  return(as.numeric(zScore))
}

brcaExprPAM50.Zscore<-t(apply(brcaExprPAM50, 1, zScoreNorm))

brcaExprPAM50.log10.Zscore<-t(apply(brcaExprPAM50.log10, 1, zScoreNorm))

brcaExprPAM50.log10.Zscore.complete<-brcaExprPAM50.log10.Zscore[complete.cases(brcaExprPAM50.log10.Zscore),]
heatmap.2(t(brcaExprPAM50.log10.Zscore.complete), trace = "none", na.rm = T)


kmeanCluster<-kmeans(as.matrix(brcaExprPAM50.log10.Zscore.complete), centers = 3)
#kMeanClusterNames<-names(kmeanCluster$cluster)
kmeanCluster.df<-data.frame(kmeanCluster$cluster)
kmeanCluster.df$analysis_id<-row.names(kmeanCluster.df)
  
kmeanCluster.df


write.table(apple, "/home/kartong/Code/CS4220_Bioinformatics_Project/data/test.txt", sep="\t", row.names = F, quote = F)

