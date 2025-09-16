
## install.packages("ggpubr")
library(ggplot2)
myboxplot <- function(x, data, col = NULL, xlab, pvalue="auto") { 
  boxplot(x, data, axes = FALSE, col = col,outline=FALSE) 
  axis(1, at = 1:2, labels =FALSE) 
  text(1:2, y=par()$usr[3]-0.08*(par()$usr[4]-par()$usr[3]), 
       srt=60, xpd=T, adj=1, labels = xlab) 
  if (pvalue == "auto") { 
    pvalue <- round(t.test(x, data=data)$p.value, 3) 
  } 
  if (!is.null(pvalue)) { 
    plab <- paste("p =", pvalue) 
    text(1.5, y = par()$usr[4]*1.05, xpd=T, label=plab, col=col) 
    }}
layout(t(1:4))
par(oma=c(2,4,4,0),mar=c(5,2,1,1),cex=1)
Stage=read.table("Stage.txt",sep="\t",header=T,check.names=F)
myboxplot(TRMT6 ~ Stage,data=Stage,col="#00AFBB")
axis(2,las=1)
T=read.table("T.txt",sep="\t",header=T,check.names=F)
myboxplot(TRMT6 ~ T,data=T,col="hotpink")
N=read.table("N.txt",sep="\t",header=T,check.names=F)
myboxplot(TRMT6 ~ N,data=N,col="steelblue2")
M=read.table("M.txt",sep="\t",header=T,check.names=F)
myboxplot(TRMT6 ~ M,data=M,col="#FC4E07")

Stage_kruskalTest<-kruskal.test(TRMT6 ~ Stage, data = Stage) 
Stage_pValue=Stage_kruskalTest$p.value

T_kruskalTest<-kruskal.test(TRMT6 ~ T, data = T) 
T_pValue=T_kruskalTest$p.value

N_wilcoxTest<-wilcox.test(TRMT6 ~ N, data = N) 
N_pValue=N_wilcoxTest$p.value

M_wilcoxTest<-wilcox.test(TRMT6 ~ M, data = M) 
M_pValue=M_wilcoxTest$p.value


######΢??: 18520221056
