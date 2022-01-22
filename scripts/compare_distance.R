library(ggpubr)
m <- read.table("data/meta.txt",header = T,row.names = 1,sep = "\t",comment.char = "")
groups <- c("HC","FH","FRI")
dm <- read.table("data/unweighted_unifrac_dm.txt",header = T,row.names = 1,sep = "\t")
sid <- intersect(rownames(dm),rownames(m))
dm <- dm[sid,sid]
m <- m[sid,]
con_vs_others <- matrix(NA,ncol = 3,nrow = 2041)
k=1
for (i in groups) {
  cons <- rownames(m[(m$group %in% i),])
  for(j in groups){
    others <- rownames(m[(m$group %in% j),])
    if(i == j){
      dd <- c(as.dist(dm[cons,others]))
    }else{
      dd <- c(as.matrix(dm[cons,others]))
    }
    numb <- length(dd)
    con_vs_others[k:(k+numb-1),3] <- dd
    con_vs_others[k:(k+numb-1),1] <- i
    con_vs_others[k:(k+numb-1),2] <- j
    k <- k+numb
  }
}
con_vs_others <- data.frame(con_vs_others,stringsAsFactors = F)
colnames(con_vs_others) <- c("group1","group2","dist")
con_vs_others$dist <- as.numeric(con_vs_others$dist)
con_vs_others <- con_vs_others[con_vs_others$group1 %in% "HC",]
con_vs_others$group2 <- factor(con_vs_others$group2,levels = groups)
symnum.args <- list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns"))
ggboxplot(con_vs_others,"group2","dist",color = "group2",add = "jitter")+
  stat_compare_means(comparisons = list(c("HC", "FH"),
                                        c("FH", "FRI"),
                                        c("HC", "FRI")),
                     label = "p.signif",
                     symnum.args = symnum.args)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  ylab("Distance (HC vs .)")+
  guides(color=F)
ggsave("plot/dist_unweighted_unifrac.pdf",width = 4,height = 4)
