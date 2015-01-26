pdf("auc_out_8_methods_and_ensemble.V3.pdf")
for (tissue in c("forebrain","heart","limb","midbrain","hindbrain","neuraltube"))
{
        cat(tissue,"\n")
        x <- read.table(paste("/home/si14w/my/",tissue,".V3.allages",sep=""), sep="\t", header=TRUE)
	boxplot(x,las=2, cex.names=0.6, pch=".", main=tissue, cex.main=0.85, ylab="auc", col="lightgreen", ylim=c(0.5,1), BTY="N")
}
dev.off()
