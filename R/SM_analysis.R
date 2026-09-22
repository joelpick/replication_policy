library(beeswarm)
library(viridis)
library(scales)

# setwd("/Users/joelpick/github/replication_policy")

dd <- read.csv("./Data/replication_data.csv")
pub <- read.csv("./Data/publishers.csv")

dd$Publisher2<-pub$Publisher2[match(dd$Journal,pub$Journal)]

## remove out of scope papers
dd_scope <- subset(dd,info_found!="Out of scope for this journal")

## add on journal Impact Factors (JIF)
dd_scope$JIF <- as.numeric(dd_scope$JIF)
dd_scope$JIF_log <- log(dd_scope$JIF)


############
#### Sensitivity analysis - publisher
############

# full model
model_full<-lme4::lmer(JIF_log~novelty + info_found + (1|Publisher2),dd_scope )

# reduced models for LRT
model_noN<-lme4::lmer(JIF_log~info_found + (1|Publisher2),dd_scope )
model_noR<-lme4::lmer(JIF_log~novelty + (1|Publisher2),dd_scope )

# LRTs
novelty_test<-anova(model_full,model_noN)
rep_info_test<-anova(model_full,model_noR)

#results table
rbind(
cbind(summary(model_full)$coef[,1:2],
	rbind(NA,novelty_test[2,c("Chisq","Pr(>Chisq)")],rep_info_test[2,c("Chisq","Pr(>Chisq)")])
),
publisher=c(as.numeric(summary(model_full)$varcor),NA,NA,NA),
residual=c(summary(model_full)$sigma^2,NA,NA,NA)
)

# original model and results table for comparison
model_noRE<-lm(JIF_log~novelty + info_found,dd_scope )
rbind(summary(model_noRE)$coef,
	residual=c(summary(model_noRE)$sigma^2,NA,NA,NA))


############
#### Visualising impact factor by publisher
#### Figure S1
############

pub_tab<-table(dd_scope$Publisher2)
mult_pub<-names(pub_tab[pub_tab>1])
mult_pub2<-names(pub_tab[pub_tab>5])
dd_scope$mult_pub<-as.factor(dd_scope$Publisher2 %in% mult_pub + dd_scope$Publisher2 %in% mult_pub2)
new_order <- with(dd_scope, reorder(Publisher2, JIF_log , median , na.rm=T))


setEPS()
pdf("./Figures/FigureS1_publisher_JIF.pdf", height=6, width=9)
# png("./Figures/FigureS1_publisher_JIF.png", height=430, width=645)
{
par(mar=c(4,12,1,1))

boxplot(JIF~new_order,dd_scope, horizontal=TRUE, ylab="",  border=c(2,"purple",4)[as.factor(((pub_tab>1 )+( pub_tab>5))[levels(new_order)])], yaxt="n", log="x", range=0, xlab="Journal Impact Factor")
Map(axis, side=2, at=1:length(levels(new_order)), col.axis=c(2,"purple",4)[as.factor(((pub_tab>1 )+( pub_tab>5))[levels(new_order)])], labels=levels(new_order), cex.axis=0.3, las=1)
legend("bottomright",c("1","2-5",">5"),col=c(2,"purple",4),pch=19, title="Number of Journals", bty="n")


}
dev.off()



############
#### Replication policy by publisher group
#### Figure S2
############

## whether info is found or not 
pub_tab2<-table(dd_scope$Publisher2,dd_scope$info_found)


table(dd_scope$mult_pub,dd_scope$info_found)

setEPS()
pdf("./Figures/FigureS2_replication_policy_publisher.pdf", height=4, width=5)
# png("./Figures/FigureS2_replication_policy_publisher.png", height=430, width=645)
{
	par(mar=c(5,5,1,1))

barplot(t(table(dd_scope$mult_pub,dd_scope$info_found))[2:1,], beside=TRUE, names.arg=c("1 journal", "2-5 journals", ">5 journals"), xlab="Publisher Group", ylab="Number of Journals")
legend("topleft", c("Yes","No"), pch=15,col=grey.colors(2), title="Information on Replications", bty="n")
}
dev.off()



############
#### Impact Factor by publisher group and replication policy/novelty language
#### Figure S3
############
se<-function(x)sd(x)/sqrt(length(x))

info_mean<-aggregate(JIF~info_found+mult_pub,dd_scope,mean)
info_se<-aggregate(JIF~info_found+mult_pub,dd_scope,se)

novelty_mean<-aggregate(JIF~novelty+mult_pub,dd_scope,mean)
novelty_se<-aggregate(JIF~novelty+mult_pub,dd_scope,se)



setEPS()
pdf("./Figures/FigureS3_JIF_publisher_novelty.pdf", height=5, width=9)
# png("./Figures/FigureS2_replication_policy_publisher.png", height=430, width=645)
{

par(mfrow=c(1,2), mar=c(5,5,1,1))
beeswarm(JIF~info_found+mult_pub,dd_scope, subset=info_found!="Out of scope for this journal",pch=19, cex=0.75, col=scales::alpha(c(2,2,"purple","purple",4,4),0.3),method = "compactswarm",corral="wrap", xlab="Information about replication", log=TRUE, at=6:1, ylab="Journal impact factor",labels=c("No","Yes"))
points(info_mean[,3]~c(6,4:1), pch=19, col=1)
arrows(c(6,4:1),info_mean[,3]+info_se[,3],c(6,4:1),info_mean[,3]-info_se[,3], code=3, angle=90, length=0.1, col=1)
mtext("A)",side=3, adj=0.02, las=1,outer=TRUE, line=-2, cex=1.5)
legend("topright",c("1","2-5",">5"),col=c(2,"purple",4),pch=19, title="Number of Journals", bty="n")

beeswarm(JIF~novelty+mult_pub,dd_scope, subset=novelty!="Out of scope for this journal",pch=19, cex=0.75, col=scales::alpha(c(2,2,"purple","purple",4,4),0.3),method = "compactswarm",corral="wrap", xlab="Used novelty language", log=TRUE, at=6:1, ylab="Journal impact factor",labels=c("No","Yes"))
points(novelty_mean[,3]~c(6:1), pch=19, col=1)
arrows(c(6:1),novelty_mean[,3]+novelty_se[,3],c(6:1),novelty_mean[,3]-novelty_se[,3], code=3, angle=90, length=0.1, col=1)
mtext("B)",side=3, adj=0.53, las=1,outer=TRUE, line=-2, cex=1.5)

}
dev.off()
