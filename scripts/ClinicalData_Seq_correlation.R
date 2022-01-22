library(dplyr)
library(psych)
library(corrplot)
m <- read.table("data/meta.txt",header = T,row.names = 1,sep = "\t",comment.char = "")
m1 <- m[m$group %in% c("FH", "FRI"),]
m1 <- droplevels(m1)
m1$group <- factor(m1$group,levels = c("FH", "FRI"))
otu <- read.table("data/dada2_even9650.biom.txt",row.names=1, header = T, sep = "\t", skip = 1, comment.char = "")
taxon <- lapply(otu$taxonomy, function(x) {
  tmp <- gsub(".__", "", x, perl = T) %>% gsub(" ", "", .) %>% 
    strsplit(., ";") %>% unlist();
  len <- length(tmp);
  tax <- ifelse(len == 7, 
                paste(tmp[6], tmp[7], sep = "_"),
                tmp[len]) %>% gsub("_$", "", .);
  return(tax)}
) %>% unlist()
taxa <- data.frame(taxonomy = otu$taxonomy,seqid = rownames(otu), 
                   shortname = paste(rownames(otu), taxon, sep = "_"))
rownames(taxa) <- taxa$seqid
sid <- intersect(rownames(m1),colnames(otu))
otu <- t(otu[,sid])
otu <- otu/9650
otu1 <- otu[,colMeans(otu)>0.01]
spearman <- corr.test(otu1,m1[,6:16],method = "spearman",adjust = "none")
spearman_p <- spearman$p
spearman_p <- spearman_p[, apply(spearman$p,2,function(x) any(na.omit(x)<0.05))]
spearman_p <- spearman_p[apply(spearman$p,1,function(x) any(na.omit(x)<0.05)), ]
r <- spearman$r[rownames(spearman_p),colnames(spearman_p)]

taxa_sig <- taxa[rownames(r),]
taxa_sig <- taxa_sig[order(taxa_sig$taxonomy),]
r <- r[rownames(taxa_sig),]
spearman_p <- spearman_p[rownames(taxa_sig),]
rownames(r) <- taxa_sig$shortname
rownames(spearman_p) <- taxa_sig$shortname
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))(200)
pdf("plot/seqMean0.01_clinic_corrplot.pdf",width = 6,height = 6)
corrplot(r, p.mat = spearman_p, insig = "label_sig",sig.level = c(.001,.01,.05),
         tl.col = "black",col = rev(col2),#tl.cex = 0.6,tl.srt=45,
         pch.cex = 2, pch.col = "black",cl.cex = 0.5,
         cl.lim = c(-0.5,0.5),is.corr = F)
dev.off()
