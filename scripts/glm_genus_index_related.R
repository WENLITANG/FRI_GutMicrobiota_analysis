library(dplyr)
library(ggpubr)
genus <- read.table("data/dada2_even9650_sorted_L6.txt", header = T, row.names = 1, sep = "\t", skip = 1, comment.char = "")
m <- read.table("data/meta.txt", header = T, row.names = 1, sep = "\t", comment.char = "")
sid <- intersect(rownames(m), colnames(genus))
genus <- genus[, sid]
m <- m[sid, ]
g <- genus[-grep("Other", rownames(genus)), ]
g <- g[-grep("g__$", rownames(g)), ]
genera <- g[apply(g, 1, function(x) mean(x)>0.001 & length(which(x>0))>nrow(g)*0.2), ]
rownames(genera) <- gsub(".__", "", rownames(genera))
taxa <- lapply(rownames(genera), function(x) strsplit(x, ";")) %>% unlist() %>% matrix(., ncol = 6, byrow = T) %>% as.data.frame()
taxa$shortname <- ifelse(duplicated(taxa$V6), paste(taxa$V5, taxa$V6, sep = "_"), taxa$V6)
taxa$shortname <- gsub("\\[", ".", taxa$shortname)
taxa$shortname <- gsub("\\]", ".", taxa$shortname)
rownames(genera) <- taxa$shortname
genera <- t(genera) %>% as.data.frame()
all(rownames(genera) %in% rownames(m))
dta <- merge(m, genera, by = "row.names")
dta[, colnames(genera)] <- apply(dta[, colnames(genera)], 2, function(x) log2(x+0.000001))
dta$group <- factor(dta$group, levels = c("HC", "FH", "FRI"))

######################################################## differential genus
#--------------------------FRI vs FH vs HC
res <- c()
for (i in colnames(genera)) {
  f <- paste0("group ~ age + gender + ", i)  %>% as.formula()
  tmp <- glm(f, data = dta, family = "binomial") %>% summary()
  res <- rbind(res, tmp$coefficients[4, ])
}
res <- as.data.frame(res)
rownames(res) <- colnames(genera)
res <- res[order(res$`Pr(>|z|)`), ]
res[res$`Pr(>|z|)` < 0.05, ]
#write.table(res, "data/FRI_FH_HC_glm.txt", col.names = NA, sep = "\t", quote = F)
#--------------------------FRI vs HC
dta2 <- subset(dta, group != "FH") %>% droplevels()
levels(dta2$group)
res21 <- c()
for (i in colnames(genera)) {
  f <- paste0("group ~ age + gender + ", i)  %>% as.formula()
  tmp <- glm(f, data = dta2, family = "binomial") %>% summary()
  res21 <- rbind(res21, tmp$coefficients[4, ])
}
res21 <- as.data.frame(res21)
rownames(res21) <- colnames(genera)
res21 <- res21[order(res21$`Pr(>|z|)`), ]
#write.table(res21, "data/FRI_HC_glm.txt", col.names = NA, sep = "\t", quote = F)
res21[res21$`Pr(>|z|)` < 0.05, ]
#--------------------------FRI vs FH
dta2 <- subset(dta, group != "HC") %>% droplevels()
levels(dta2$group)
res23 <- c()
for (i in colnames(genera)) {
  f <- paste0("group ~ age + gender + ", i)  %>% as.formula()
  tmp <- glm(f, data = dta2, family = "binomial") %>% summary()
  res23 <- rbind(res23, tmp$coefficients[4, ])
}
res23 <- as.data.frame(res23)
rownames(res23) <- colnames(genera)
res23 <- res23[order(res23$`Pr(>|z|)`), ]
res23[res23$`Pr(>|z|)` < 0.05, ]
#write.table(res23, "data/FRI_FH_glm.txt", col.names = NA, sep = "\t", quote = F)

####################################################### calculate index
res$genus <- rownames(res)
res21$genus <- rownames(res21)
res23$genus <- rownames(res23)
glm_genus <- rbind(res[res$`Pr(>|z|)` < 0.05, ], #FRI vs FH vs HC
                  res21[res21$`Pr(>|z|)` < 0.05, ], #FRI vs HC
                  res23[res23$`Pr(>|z|)` < 0.05, ]) #FRI vs FH
glm_genus$position <- ifelse(glm_genus$Estimate > 0, 1, -1)
glm_genus_position <- distinct(glm_genus[, c("genus", "position")])
any(duplicated(glm_genus_position$genus)) #FALSE
FRI_HC_genus <- glm_genus_position$genus
ddindex <- genera[, FRI_HC_genus]
for (i in colnames(ddindex)) {
  ddindex[, i] <- ddindex[, i]*glm_genus_position[glm_genus_position$genus %in% i, "position"]
}
ddindex$index <- rowSums(ddindex)
ddindex$group <- dta$group
ddindex$group2 <- m[rownames(ddindex), "Sinus_abscess"]
ddindex$group2 <- ifelse(ddindex$group2 %in% c("Yes","No"), ddindex$group2, as.character(ddindex$group))
ddindex[, FRI_HC_genus] <- abs(ddindex[, FRI_HC_genus])
write.table(ddindex, "data/glm_genus_index.txt", col.names = NA, sep = "\t", quote = F)

######################################################## index, boxplot
ddindex$group <- factor(ddindex$group, levels = c("HC", "FH", "FRI"))
ggboxplot(ddindex,"group", "index", color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = c(0.1, 0.9))+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 1)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  ylab("Index")+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))+
  guides(color=F)
ggsave(paste0("plot/glm/glm_genus_index",".pdf"),width = 3.5,height = 4)
#----------------------------------
ddindex$group2 <- factor(ddindex$group2, levels = c("HC", "FH", "No", "Yes"))
ggboxplot(ddindex,"group2", "index", color = "group2", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = c(0.1, 0.45, 0.4))+
  stat_compare_means(comparisons = list(c("FH","No"), c("FH","Yes"), c("No","Yes")),
                     label = "p.format", label.y = c(0.55, 0.7, 0.85))+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#f22046","#630616"))+
  xlab(NULL)+
  ylab("Index")+
  guides(color=F)
ggsave("plot/glm/Sinus_abscess_index.pdf",width = 3.8,height = 4)

############################################################## pheatmap
library(pheatmap)
annotation_col = data.frame(
  group=ddindex$group,index=ddindex$index
)
rownames(annotation_col) = rownames(ddindex)
ann_colors = list(
  group=c("HC"="#00A087B2","FH"="#3C5488B2","FRI"="#DC0000B2")
)
taxaorder <- c("Escherichia",#"Veillonella","Streptococcus","Actinomyces",
               "Roseburia","Parabacteroides", "Ruminococcus", "Lachnospira",#"Desulfovibrio",
               "Blautia","Coprococcus",#"Prevotella",
               "SMB53","Phascolarctobacterium" #,"Dialister","Anaerostipes"
               )
d_pheatmap <- ddindex[, taxaorder] %>% t() 
pheatmap(d_pheatmap, cluster_cols = F, cluster_rows = F, #scale = "row", 
         color = c(rep("white",1),
                   colorRampPalette(c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A"))(19),
                   colorRampPalette(c("#FC4E2A","#E31A1C","#BD0026","#800026"))(70)),
         #cellwidth = 8, cellheight = 8,
         show_colnames = F,
         annotation_col = annotation_col, gaps_col = c(12, 30), #gaps_row = c(4, 7),
         annotation_colors = ann_colors,
         height = 4, width = 7.2, filename = "plot/glm/pheatmap.pdf")
dev.off()
#----------------------
ddindex_mean <- apply(ddindex[, taxaorder], 2, function(x) unlist(tapply(x, ddindex$group, mean)))
ddindex_mean <- t(ddindex_mean)
pheatmap(ddindex_mean, cluster_cols = F, cluster_rows = F, scale = "row", angle_col = 0,
         height = 4, width = 4, filename = "plot/glm/pheatmap_mean.pdf")
dev.off()
#----------------------
sa <- ddindex[ddindex$group2 %in% c("No","Yes"), ]
sa <- sa[order(sa$group2),]
annotation_col = data.frame(
  group=sa$group2,index=sa$index
)
rownames(annotation_col) = rownames(sa)
ann_colors = list(
  group=c("No"="#f22046","Yes"="#630616")
)
sa_pheatmap <- sa[, taxaorder] %>% t()
pheatmap(sa_pheatmap, cluster_cols = F, cluster_rows = F, #scale = "column", 
         color = c(rep("white",1),
                   colorRampPalette(c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A"))(19),
                   colorRampPalette(c("#FC4E2A","#E31A1C","#BD0026","#800026"))(70)),
         #cellwidth = 8, cellheight = 8,
         show_colnames = F,
         annotation_col = annotation_col, gaps_col = c(4), 
         annotation_colors = ann_colors,
         height = 4, width = 7.2, filename = "plot/glm/pheatmap_Sinus_abscess.pdf")
dev.off()

############################################################### index, ROC
#---------------------------------FRI VS HC
library(pROC)
d <- ddindex[ddindex$group %in% c("HC","FRI"),] %>% droplevels()
pdf(paste0("plot/glm/HC_FRI_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#---------------------------------FRI VS FH
d <- ddindex[ddindex$group %in% c("FH","FRI"),] %>% droplevels()
pdf(paste0("plot/glm/FH_FRI_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
#---------------------------------FH VS HC
d <- ddindex[ddindex$group %in% c("HC","FH"),] %>% droplevels()
pdf(paste0("plot/glm/FH_HC_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()

#---------------------------------Sinus abscess
d <- ddindex[ddindex$group2 %in% c("Yes","No"),] %>% droplevels()
pdf(paste0("plot/glm/Yes_No_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()

d <- ddindex[ddindex$group2 %in% c("Yes","HC"),] %>% droplevels()
pdf(paste0("plot/glm/Yes_HC_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()

d <- ddindex[ddindex$group2 %in% c("No","HC"),] %>% droplevels()
pdf(paste0("plot/glm/No_HC_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()

d <- ddindex[ddindex$group2 %in% c("Yes","FH"),] %>% droplevels()
pdf(paste0("plot/glm/Yes_FH_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()

d <- ddindex[ddindex$group2 %in% c("No","FH"),] %>% droplevels()
pdf(paste0("plot/glm/No_FH_ROC.pdf"),width=5,height=5)
rocobj <- plot.roc(d$group2, d$index,main="", percent=TRUE,ci=TRUE) # print the AUC (will contain the CI)
ciobj <- ci.se(rocobj,specificities=seq(0, 100, 5)) # over a select set of specificities
plot(ciobj, type="shape", col="#1c61b6AA", print.auc=TRUE) # plot as a blue shape
text(50,20,paste("AUC = ",formatC(rocobj$auc,digits=2,format="f"),sep=""),pos=4)
ci.lower<-formatC(rocobj$ci[1],digits=2,format="f")
ci.upper<-formatC(rocobj$ci[3],digits=2,format="f")
text(50,10,paste("95% CI: ",ci.lower,"-",ci.upper,sep=""),pos=4)
dev.off()
