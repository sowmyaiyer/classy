pdf("auc_out_8_methods_and_ensemble.V4.combinedreps.newmethod.pdf")
par(mar=c(6,5,2,5)+0.1)
tissues <- c("forebrain","heart","limb","midbrain","hindbrain","neuraltube")
m <- matrix(nrow=30, ncol=length(tissues))
best_method <- list()
colnames(m) <- tissues
for (tissue in tissues)
{
        x <- read.table(paste("/home/si14w/TR/classy/",tissue,".V4.new.allages",sep=""), sep="\t", header=TRUE)
	m[,as.character(tissue)] <- x[,which.max(apply(x,2,median))]	
	cat(tissue,":",colnames(x)[which.max(apply(x,2,median))],"\n")
	boxplot(x,las=2, cex.names=0.5, pch=".", main=tissue, cex.main=0.85, ylab="auc", col="lightgreen", ylim=c(0,1), BTY="N",yaxt="n")
	axis(2,at=seq(0,1,0.05), seq(0,1,0.05), cex=0.85, cex.axis=0.85, las=1)
	axis(4,at=seq(0,1,0.05), seq(0,1,0.05), cex=0.85, cex.axis=0.85, las=1)
	abline(h=seq(0,1,0.05), lty="dotted", col="gray")
}
boxplot(m, col=c("lightgreen","lightblue","pink","brown4","orange","yellow"), las=2, cex.names=0.6, pch=".", main="AUC distribution of best scoring method in each tissue", cex.main=0.85, ylab="auc", ylim=c(0,1), BTY="N", yaxt="n")
axis(2,at=seq(0,1,0.05), seq(0,1,0.05), cex=0.85, cex.axis=0.85, las=1)
axis(4,at=seq(0,1,0.05), seq(0,1,0.05), cex=0.85, cex.axis=0.85, las=1)
abline(h=seq(0,1,0.05), lty="dotted", col="gray")
dev.off()
