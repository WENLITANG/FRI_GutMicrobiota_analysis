library(epiDisplay)
library(forestplot)
m <- read.table("data/meta.txt",header = T,row.names = 1,sep = "\t",comment.char = "")
m1 <- m[m$group %in% c("FH", "FRI"),]
m1 <- droplevels(m1)
m1$group <- factor(m1$group,levels = c("FH", "FRI"))
res <- c()
for(i in colnames(m1)[6:16]){
  glm1 <- glm(as.formula(paste0("group~",i)),data = m1,family = "binomial") %>%
    logistic.display(.) %>% .$table
  res <- rbind(res, glm1[1, ])
}
res <- as.data.frame(res)
rownames(res) <- colnames(m1)[6:16]
clinic_summ <- apply(m1[,6:16],2,function(x)
  unlist(tapply(x, m1$group, function(d) c(number=length(na.omit(d)),mean=mean(d,na.rm = T),sd=sd(d,na.rm = T),
                                           quantile(d,na.rm = T)[2:4]))))
p <- apply(m1[,6:16],2,function(x) wilcox.test(x~m1$group,m1)$p.value)
clinic_summ <- rbind(clinic_summ,wilcox_p = p) %>% t() %>% data.frame()
clinic_summ$wilcox_p_fdr <- p.adjust(clinic_summ$wilcox_p,method = "fdr")
clinic_summ$FH.mean_sd <- paste0(round(clinic_summ$FH.mean,digits = 2)," ± ",round(clinic_summ$FH.sd,digits = 2))
clinic_summ$FH.50._ci <- paste0(round(clinic_summ$FH.50.,digits = 2),"(",round(clinic_summ$FH.25.,digits=2),"-",round(clinic_summ$FH.75.,digits = 2),")")
clinic_summ$FRI.mean_sd <- paste0(round(clinic_summ$FRI.mean,digits = 2)," ± ",round(clinic_summ$FRI.sd,digits = 2))
clinic_summ$FRI.50._ci <- paste0(round(clinic_summ$FRI.50.,digits = 2),"(",round(clinic_summ$FRI.25.,digits=2),"-",round(clinic_summ$FRI.75.,digits = 2),")")
res2 <- merge(clinic_summ, res, by = "row.names", sort = F)
rownames(res2) <- res2$Row.names
res2$Row.names <- NULL
write.table(res2,"data/clinic_GLM.txt",sep = "\t",quote = F, col.names = NA)

l <- read.table("data/clinic_GLM_modified.txt",header = T,sep = "\t")
l$lower <- round(l$lower,digits = 2)
l$upper <- round(l$upper,digits = 2)
l$OR <- round(l$OR,digits = 2)
l$OR_95CI <- paste0(l$OR,"(",l$lower,",",l$upper,")")
l$OR_95CI[c(1,13)] <- NA
l$OR_95CI[2] <- "OR(95%CI)"

#16*10
pdf("plot/clinic_forestplot.pdf", width = 16, height = 10)
forestplot(x = l[,c(1:7,16)], labeltext=l[,c(1:7,16)],
           graph.pos=8,
           mean=l$OR,
           lower=l$lower, upper=l$upper,
           clip = c(0.5,2),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="#1c61b6", lines="#1c61b6", zero = "gray50"),
           zero=1, hrzl_lines = T, lwd.zero = 1.5,
           cex=0.9, lineheight = "auto",
           colgap=unit(8,"mm"),graphwidth = unit(90,"mm"),
           lwd.ci=1.5, boxsize=0.2,
           ci.vertices=TRUE, ci.vertices.height = 0.1)
dev.off()
