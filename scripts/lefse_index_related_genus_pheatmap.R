library(pheatmap)
genera <- read.table("data/lefse_genus_index.txt", header = T, row.names = 1, sep = "\t")
genera$group <- factor(genera$group, levels = c("HC", "FH", "FRI"))
genera <- genera[order(genera$group),]
generaname <- colnames(genera)[!colnames(genera) %in% c("index", "group", "group2")]
annotation_col = data.frame(
  group=genera$group,index=genera$index
)
rownames(annotation_col) = rownames(genera)
ann_colors = list(
  group=c("HC"="#00A087B2","FH"="#3C5488B2","FRI"="#DC0000B2")
)
taxaorder <- c("Escherichia","Veillonella","Streptococcus","Actinomyces",
               "Roseburia","Parabacteroides","Desulfovibrio",
               "Blautia","Coprococcus","Prevotella","SMB53","Phascolarctobacterium","Dialister","Anaerostipes")
pheatmap(t(genera[,taxaorder]), cluster_cols = F, cluster_rows = F, show_colnames = F,
         color = c(rep("white",1),
                   colorRampPalette(c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A"))(19),
                   colorRampPalette(c("#FC4E2A","#E31A1C","#BD0026","#800026"))(70)),
         #cellwidth = 8, cellheight = 8,
         annotation_col = annotation_col, gaps_col = c(12, 30), gaps_row = c(4, 7),
         annotation_colors = ann_colors,
         height = 4, width = 7.2, filename = "plot/pheatmap.pdf")
#----------------------------------------
genera_mean <- apply(genera[, taxaorder], 2, function(x) unlist(tapply(x, genera$group, mean)))
genera_mean <- as.data.frame(genera_mean)
genera_mean <- genera_mean[c("HC", "FH", "FRI"), ]
pheatmap(t(genera_mean), cluster_cols = F, cluster_rows = F, 
         scale = "row", angle_col = 0,
         height = 4, width = 4, filename = "plot/pheatmap_mean.pdf")

#########################################
sa <- genera[genera$group2 %in% c("No","Yes"),c(generaname, "group2", "index")]
sa <- sa[order(sa$group2),]
annotation_col = data.frame(
  group=sa$group2,index=sa$index
)
rownames(annotation_col) = rownames(sa)
ann_colors = list(
  group=c("No"="#f22046","Yes"="#630616")
)
pheatmap(t(sa[,taxaorder]), cluster_cols = F, cluster_rows = F, 
         color = c(rep("white",1),
                   colorRampPalette(c("#FFFFCC","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A"))(19),
                   colorRampPalette(c("#FC4E2A","#E31A1C","#BD0026","#800026"))(70)),
         #cellwidth = 8, cellheight = 8,
         annotation_col = annotation_col, gaps_col = c(4), gaps_row = c(4, 7),
         annotation_colors = ann_colors,
         height = 4, width = 7.2, filename = "plot/pheatmap_Sinus_abscess.pdf")
