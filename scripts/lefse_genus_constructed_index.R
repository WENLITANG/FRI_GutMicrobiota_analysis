library(ggplot2)
################################################lefse,barplot
lefse_biomarkers <- read.table( "data/lefse_stat.res",sep="\t")
colnames(lefse_biomarkers) <- c("taxonomy","abund","group","LDAscore","pvalue")
lefse_biomarkers <- lefse_biomarkers[apply(lefse_biomarkers,1,function(x) !is.na(x[4])),]
lefse_biomarkers$LDA <- lefse_biomarkers$LDAscore 
lefse_biomarkers$family <- unlist(lapply(lefse_biomarkers$taxonomy,function(x) unlist(strsplit(x,"\\."))[5]))
lefse_biomarkers$genus <- unlist(lapply(lefse_biomarkers$taxonomy,function(x) unlist(strsplit(x,"\\."))[6]))
lefse_biomarkers$taxon <- unlist(lapply(as.character(lefse_biomarkers$taxonomy), function(x) unlist(strsplit(x,"\\."))[length(unlist(strsplit(x,"\\.")))]))
lefse_biomarkers <- lefse_biomarkers[order(lefse_biomarkers$LDA),]
taxonlevel <- c(lefse_biomarkers[lefse_biomarkers$group %in% "Healthy","taxon"],
                lefse_biomarkers[lefse_biomarkers$group %in% "internal.fix","taxon"],
                lefse_biomarkers[lefse_biomarkers$group %in% "osteomyelitis","taxon"])
lefse_biomarkers$taxon <- factor(lefse_biomarkers$taxon,levels = taxonlevel)
ggplot(lefse_biomarkers,aes(x=taxon,y=LDA,fill=group))+
  geom_bar(stat = "identity",color="black",width = 0.8)+
  theme(panel.grid.major.y = element_blank(),
        axis.text.y  = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank())+
  xlab(NULL)+
  ylab("LDA SCORE(log 10)")+
  scale_fill_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"),
                    breaks = c("Healthy", "internal.fix", "osteomyelitis"), 
                    labels  = c("HC", "FH", "FRI"))+
  scale_y_continuous(expand = c(0, 0))+
  coord_flip()
ggsave("plot/3group_lefse.pdf",width = 6,height = 8)
#write.table(lefse_biomarkers,"data/3group_lefse_plot.txt",row.names=F,sep="\t",quote=F)

#----------------------------------index
lefse_table <- read.table("data/group.lefse.txt", header = F, row.names = 1, sep = "\t")
lefse_genus <- lefse_biomarkers[complete.cases(lefse_biomarkers$genus), ]
rownames(lefse_table) <- gsub("\\|",".",rownames(lefse_table))
rownames(lefse_table) <- gsub("\\[","_",rownames(lefse_table))
rownames(lefse_table) <- gsub("\\]","_",rownames(lefse_table))
lefse_genus$taxonomy %in% rownames(lefse_table)
g <- lefse_table[as.character(lefse_genus$taxonomy), ]
rownames(g) <- matrix(unlist(strsplit(rownames(g), "\\.")), ncol = 6, byrow = T)[, 6]
g <- apply(g, 1, function(x) as.numeric(x)/1000000)
g <- data.frame(g)
rownames(g) <- lefse_table["SID", ]
rownames(lefse_genus) <- lefse_genus$genus
lefse_genus[,"nudge_x"] <- ifelse(lefse_genus$group == "osteomyelitis",1,-1)
for (i in colnames(g)) {
  g[, i] <- g[, i] * lefse_genus[i, "nudge_x"]
}
genera <- g[, apply(g[,-ncol(g)], 2, function(x) mean(abs(x))>0.001 & length(which(abs(x)>0))>nrow(g)*0.2)]
genera$index <- rowSums(genera)
genera$group <- as.character(lefse_table["class", ])
genera[, colnames(genera) %in% colnames(g)] <- abs(genera[, colnames(genera) %in% colnames(g)])
genera$group <- ifelse(genera$group == "Healthy", "HC",
                       ifelse(genera$group == "internal.fix", "FH",
                              ifelse(genera$group == "osteomyelitis", "FRI", "other")))

m <- read.table("data/meta.txt", header = T, row.names = 1, sep = "\t", comment.char = "")
genera$group2 <- m[rownames(genera), "Sinus_abscess"]
genera$group2 <- ifelse(genera$group2 %in% c("Yes","No"), genera$group2, genera$group)
write.table(genera, "data/lefse_genus_index.txt", col.names = NA, sep = "\t", quote = F)
