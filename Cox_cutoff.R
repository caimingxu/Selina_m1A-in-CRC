#install.packages("OptimalCutpoints")

library(OptimalCutpoints)

rt = read.table(file = "input.txt",sep = "\t",row.names = 1,header = T)

optimal.cutpoints.Youden <- optimal.cutpoints(X = "TRMT6", status = "fustat", tag.healthy = 0,methods = "Youden", data = rt, pop.prev = NULL,control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
cutpointsYouden = summary(optimal.cutpoints.Youden)
#plot(optimal.cutpoints.Youden)
cutpoint=cutpointsYouden$Youden$Global$optimal.cutoff$cutoff
TRMT6=rt$TRMT6
risk=as.vector(ifelse(TRMT6>cutpoint,"high","low"))
Gene="TRMT6"
outTab=c("futime","fustat",Gene)
write.table(cbind(id=rownames(cbind(rt[,outTab],risk)),cbind(rt[,outTab],risk)),
            file = "TRMT6_Output.txt",sep="\t",quote = F,
            row.names = F)
