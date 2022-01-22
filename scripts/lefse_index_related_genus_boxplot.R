library(ggpubr)
genera <- read.table("data/lefse_genus_index.txt", header = T, row.names = 1, sep = "\t")
genera$group <- factor(genera$group, levels = c("HC", "FH", "FRI"))
generaname <- colnames(genera)[!colnames(genera) %in% c("index", "group", "group2")]
for (i in generaname) {
  ggboxplot(genera,"group", i, color = "group", add = "jitter")+
    stat_compare_means(ref.group = "HC", label = "p.format")+
    stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format")+
    scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
    xlab(NULL)+
    guides(color=F)
  ggsave(paste0("plot/", i, ".pdf"),width = 3.5,height = 4)
}

i <- "Veillonella"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.0075)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.008, tip.length = 0.001)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.008))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Actinomyces"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.002)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", 
                     label.y = 0.0023, tip.length = 0.0001)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.0025))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Desulfovibrio"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.0045)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.005, tip.length = 0.003)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.0075))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Dialister"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.021)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.023, tip.length = 0.01)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.025))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Escherichia"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.07)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.075, tip.length = 0.001)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.08))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Parabacteroides"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.016)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.018, tip.length = 0.003)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.02))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Prevotella"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.08)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.09, tip.length = 0.01)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.1))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "SMB53"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.02)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.023, tip.length = 0.01)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.025))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)

i <- "Streptococcus"
ggboxplot(genera,"group", i, color = "group", add = "jitter")+
  stat_compare_means(ref.group = "HC", label = "p.format", label.y = 0.018)+
  stat_compare_means(comparisons = list(c("FH","FRI")), label = "p.format", label.y = 0.02, tip.length = 0.01)+
  scale_color_manual(values = c("#00A087B2","#3C5488B2","#DC0000B2"))+
  xlab(NULL)+
  guides(color=F)+coord_cartesian(ylim = c(0, 0.02))
ggsave(paste0("plot/", i, "_2.pdf"),width = 3.5,height = 4)
