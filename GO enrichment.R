library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")
library("org.Hs.eg.db")         
library(export)

rt=read.table("symbol.txt",sep="\t",check.names=F,header=T)   
genes=as.vector(rt[,1])
entrezIDs <- mget(genes, org.Hs.egSYMBOL2EG, ifnotfound=NA)   
entrezIDs <- as.character(entrezIDs)
out=cbind(rt,entrezID=entrezIDs)
write.table(out,file="id.txt",sep="\t",quote=F,row.names=F)

rt=read.table("id.txt",sep="\t",header=T,check.names=F)       
rt=rt[is.na(rt[,"entrezID"])==F,]                                
gene=rt$entrezID

GO<-enrichGO(gene=gene,OrgDb = "org.Hs.eg.db",ont="ALL",pvalueCutoff = 0.5,qvalueCutoff = 1,readable = T) 
go<-as.data.frame(GO)
View(go)
table(go[,1]) #Count number of BP,CC,MF


go_MF<-go[go$ONTOLOGY=="MF",][1:10,]
go_CC<-go[go$ONTOLOGY=="CC",][1:10,]
go_BP<-go[go$ONTOLOGY=="BP",][1:50,]
go_enrich_df<-data.frame(ID=c(go_BP$ID, go_CC$ID, go_MF$ID),
                         Description=c(go_BP$Description, go_CC$Description, go_MF$Description),
                         GeneNumber=c(go_BP$Count, go_CC$Count, go_MF$Count),
                         type=factor(c(rep("biological process", 50), rep("cellular component", 10),rep("molecular function",10)),levels=c("molecular function", "cellular component", "biological process")))

## numbers as data on x axis
go_enrich_df$number <- factor(rev(1:nrow(go_enrich_df)))
## shorten the names of GO terms
shorten_names <- function(x, n_word=4, n_char=40){
  if (length(strsplit(x, " ")[[1]]) > n_word || (nchar(x) > 40))
  {
    if (nchar(x) > 40) x <- substr(x, 1, 40)
    x <- paste(paste(strsplit(x, " ")[[1]][1:min(length(strsplit(x," ")[[1]]), n_word)],
                     collapse=" "), "...", sep="")
    return(x)
  } 
  else
  {
    return(x)
  }
}


## colors for bar // green, blue, orange
CPCOLS <- c("#8DA1CB", "#FD8D62", "#66C3A5")
library(ggplot2)
p <- ggplot(data=go_enrich_df, aes(x=number, y=GeneNumber, fill=type)) +
  geom_bar(stat="identity", width=0.8) + coord_flip() + 
  scale_fill_manual(values = CPCOLS) + theme_test() + 
  scale_x_discrete(labels=rev(go_enrich_df$Description)) +
  xlab("GO term") + 
  theme(axis.text=element_text(face = "bold", color="gray50")) +
  labs(title = "The Most Enriched GO Terms")
p
graph2ppt(file="effect plot.pptx", width=16, height=16)





