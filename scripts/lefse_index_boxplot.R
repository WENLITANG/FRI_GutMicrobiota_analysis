library(ggpubr)
genera <- read.table("data/lefse_genus_index.txt", header = T, row.names = 1, sep = "\t")
genera$group <- factor(genera$group, levels = c("HC", "FH", "FRI"))
ggboxplot(genera,"group", "index", color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = c(0, 0.2))+
  stat_compare_means(comparisons = list(c("FH","FRI")), 
                     label = "p.format", label.y = 0.6)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  ylab("Index")+
  guides(color=F)
ggsave(paste0("plot/lefse_genus_index",".pdf"),width = 3.5,height = 4)

genera$group2 <- factor(genera$group2, levels = c("HC", "FH", "No", "Yes"))
ggboxplot(genera,"group2", "index", color = "group2", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = c(0, 0.55, 0.2))+
  stat_compare_means(comparisons = list(c("FH","No"), c("FH","Yes"), c("No","Yes")), 
                     label = "p.format", label.y = c(0.65, 0.8, 1))+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#f22046","#630616"))+
  xlab(NULL)+
  ylab("Index")+
  guides(color=F)
ggsave(paste0("plot/Sinus_abscess_index",".pdf"),width = 3.8,height = 4)
