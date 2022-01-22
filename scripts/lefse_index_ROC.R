library(pROC)
library(dplyr)
genera <- read.table("data/lefse_genus_index.txt", header = T, row.names = 1, sep = "\t")
genera$group <- factor(genera$group, levels = c("HC", "FH", "FRI"))
genera$group2 <- factor(genera$group2, levels = c("HC", "FH", "No", "Yes"))
#######################################
d <- genera[genera$group %in% c("HC","FRI"),] %>% droplevels()
pdf(paste0("plot/HC_FRI_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- genera[genera$group %in% c("FH","FRI"),] %>% droplevels()
pdf(paste0("plot/FH_FRI_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- genera[genera$group %in% c("HC","FH"),] %>% droplevels()
pdf(paste0("plot/HC_FH_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()


###############################################################
d <- droplevels(genera[genera$group2 %in% c("HC","No"),]) %>% droplevels()
pdf(paste0("plot/HC_No_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- droplevels(genera[genera$group2 %in% c("HC","Yes"),]) %>% droplevels()
pdf(paste0("plot/HC_Yes_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- droplevels(genera[genera$group2 %in% c("FH","No"),]) %>% droplevels()
pdf(paste0("plot/FH_No_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- droplevels(genera[genera$group2 %in% c("FH","Yes"),]) %>% droplevels()
pdf(paste0("plot/FH_Yes_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#######################################
d <- droplevels(genera[genera$group2 %in% c("No","Yes"),]) %>% droplevels()
pdf(paste0("plot/No_Yes_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
