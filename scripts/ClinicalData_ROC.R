library(pROC)
m <- read.table("data/meta.txt",header = T,row.names = 1,sep = "\t",comment.char = "")
m1 <- m[m$group %in% c("FH", "FRI"),]
m1 <- droplevels(m1)
m1$group <- factor(m1$group,levels = c("FH", "FRI"))
for (i in colnames(m1)[6:16]) {
  pdf(paste0("plot/ROC_",i,".pdf"),width=5,height=5)
  rocobj <- plot.roc(m1$group, m1[,i],main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
  ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
  plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
  text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
  ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
  ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
  text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
  dev.off()
}
