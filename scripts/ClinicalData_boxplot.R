library(ggpubr)
m <- read.table("data/meta.txt",header = T,row.names = 1,sep = "\t",comment.char = "")
m1 <- m[m$group %in% c("FH", "FRI"),]
m1 <- droplevels(m1)
m1$group <- factor(m1$group,levels = c("FH", "FRI"))
for (index in colnames(m1)[6:16]) {
  plot1 <- ggboxplot(m1,"group",index,color = "group",add = "jitter")+
    stat_compare_means()+
    scale_color_manual(values = c("#3C5488B2","#DC0000B2"))+
    xlab(NULL)+
    guides(color=F)
  ggsave(paste0("plot/boxplot_",index,".pdf"),plot1,width = 3.5,height = 4)
}

index <- "CRP"
ggboxplot(m1,"group",index,color = "group",add = "jitter")+
  stat_compare_means(label.y = 25)+
  scale_color_manual(values = c("#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+
  coord_cartesian(ylim = c(0,25))
ggsave(paste0("plot/boxplot_",index,".pdf"),width = 3.5,height = 4)

index <- "PCT"
ggboxplot(m1,"group",index,color = "group",add = "jitter")+
  stat_compare_means(label.y = 0.1)+
  scale_color_manual(values = c("#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+
  coord_cartesian(ylim = c(0,0.1))
ggsave(paste0("plot/boxplot_",index,".pdf"),width = 3.5,height = 4)
