library(beeswarm)
library(viridis)

# setwd("/Users/joelpick/github/replication_policy")

dd <- read.csv("./Data/replication_data.csv")
dd_J <- read.csv("./Data/journal_data.csv")


## remove out of scope papers
dd_scope <- subset(dd,info_found!="Out of scope for this journal")

## number out of scope
nrow(dd) - nrow(dd_scope)

## add on journal Impact Factors (JIF)
dd_scope$JIF <- dd_J[match(dd_scope$Journal,dd_J$Journal),"X2023.JIF"]
dd_scope$JIF <- ifelse(dd_scope$JIF=="<0.1",0.1,as.numeric(dd_scope$JIF))

# group replication policy and accepting categories into single variables
dd_scope$replication_policy <- ifelse(dd_scope$info_found=="Yes",
	ifelse(dd_scope$replications_mention=="Yes",dd_scope$replication_policy_mentions,dd_scope$replication_policy_not
		),NA)
dd_scope$accept_category <- ifelse(dd_scope$info_found=="Yes", 
	ifelse(dd_scope$replications_mention=="Yes",dd_scope$accept_category_mentions,dd_scope$accept_category_not
		),NA)


############
#### results
############

## was there info on replications?
table(dd_scope$info_found)
round(table(dd_scope$info_found)/nrow(dd_scope),3)*100

# replications mentioned
tab1<-table(dd_scope$replications_mention,dd_scope$info_found,useNA="ifany")
tab1/sum(tab1)

# aggregate(dd_scope,length)
# table(dd_scope$policy_location)

# subset(dd_scope,info_found=="Yes")

# dd_scope["128",]

table(dd_scope$novelty)
table(dd_scope$novelty)/nrow(dd_scope)

novel_rep <- table(dd_scope$replication_policy,dd_scope$novelty)[c(4,2,3,1),]
novel_rep

table(dd_scope$accept_category,dd_scope$replications_mention)

tab2 <- table(dd_scope$replication_policy,dd_scope$replications_mention)[c(4,2,3,1),]
tab2/sum(tab2)

setEPS()
pdf("./Figures/Figure_replication_policy.pdf", height=6, width=9)
{
par(mfrow=c(1,2), mar=c(5,4,1,2))
bp1<-barplot(tab1[,2:1], col=palette.colors()[c(2,3,8)], xlab="Information about replication", ylab="Number of Journals", horiz=FALSE, ylim= c(0,230))
legend("topleft",c("Directly ","Indirectly ","No Info"), pch=15,col=palette.colors()[c(2,3,8)],bty="n")
mtext("A)",side=3, adj=0, las=1,outer=TRUE, line=-2, cex=1.5)


x1<-rep(NA,nrow(tab1))
x2<-rep(NA,nrow(tab1))
for(i in 1:nrow(tab1)) x1[i]<-sum(tab1[0:(i-1),1]) + tab1[i,1]/2
for(i in 1:nrow(tab1)) x2[i]<-sum(tab1[0:(i-1),2]) + tab1[i,2]/2

text(rep(bp1[1],2),x2[1:2],tab1[1:2,2])
text(bp1[2],x1[3],tab1[3,1], col="white")




par(mar=c(5,5,1,1))
bp2 <- barplot(tab2[,2:1], col=viridis::viridis(nrow(tab2)), xlab="Directly mentions replication", ylab="Number of Journals", horiz=FALSE, ylim=c(0,27),yaxt="n")
axis(2,c(0,5,10,15,20),c(0,5,10,15,20))
legend("topleft",rownames(tab2), pch=15,col=viridis::viridis(nrow(tab2)),box.col=0)#,box.col="white", bg="white"
mtext("B)",side=3, adj=0.43, las=1,outer=TRUE, line=-2, cex=1.5)

x1<-rep(NA,nrow(tab2))
x2<-rep(NA,nrow(tab2))
for(i in 1:nrow(tab2)) {
	x1[i]<-sum(tab2[0:(i-1),1]) + tab2[i,1]/2
  x2[i]<-sum(tab2[0:(i-1),2]) + tab2[i,2]/2
}
text(rep(bp2[2],4),x1,tab2[,1], col=c("white","white",1,1))
text(rep(bp2[1],3),x2[c(1,3,4)],tab2[c(1,3,4),2], col=c("white",1,1))
}
dev.off()


setEPS()
pdf("./Figures/Figure_novel_policy.pdf", height=6, width=4.5)
{
novel_rep <- table(dd_scope$replication_policy,dd_scope$novelty)[c(4,2,3,1),2:1]
novel_rep

par(mfrow=c(1,1), mar=c(5,5,1,1))
bp3 <- barplot(novel_rep, beside=FALSE,col=viridis::viridis(4), ylim=c(0,22), ylab="Number of Journals", xlab="Used novelty language", yaxt="n")
axis(2,c(0,5,10,15),c(0,5,10,15))
legend("topleft",rownames(novel_rep), pch=15,col=viridis::viridis(nrow(novel_rep)),box.col=0)

x1<-rep(NA,nrow(novel_rep))
x2<-rep(NA,nrow(novel_rep))
for(i in 1:nrow(novel_rep)) {
	x1[i]<-sum(novel_rep[0:(i-1),1]) + novel_rep[i,1]/2
  x2[i]<-sum(novel_rep[0:(i-1),2]) + novel_rep[i,2]/2
}
text(rep(bp3[1],4),x1,novel_rep[,1], col=c("white","white",1,1))
text(rep(bp3[2],4),x2,novel_rep[,2], col=c("white","white",1,1))
}
dev.off()

##### impact factor

### Impact factor factor figure

se<-function(x)sd(x)/sqrt(length(x))

info_mean<-aggregate(JIF~info_found,dd_scope,mean)
info_se<-aggregate(JIF~info_found,dd_scope,se)

novelty_mean<-aggregate(JIF~novelty,dd_scope,mean)
novelty_se<-aggregate(JIF~novelty,dd_scope,se)

setEPS()
pdf("./Figures/Figure_JIF.pdf", height=5, width=9)
{
par(mfrow=c(1,2), mar=c(5,5,1,1))
beeswarm(JIF~info_found,dd_scope, subset=info_found!="Out of scope for this journal",pch=19, cex=0.75, col=scales::alpha(1,0.3),method = "compactswarm",corral="wrap", xlab="Information about replication", log=TRUE, at=2:1)
points(info_mean[,2]~c(1,2), pch=19, col="red")
arrows(c(1,2),info_mean[,2]+info_se[,2],c(1,2),info_mean[,2]-info_se[,2], code=3, angle=90, length=0.1, col="red")
mtext("A)",side=3, adj=0.02, las=1,outer=TRUE, line=-2, cex=1.5)

beeswarm(JIF~novelty,dd_scope, subset=novelty!="Out of scope for this journal",pch=19, cex=0.75, col=scales::alpha(1,0.3),method = "compactswarm",corral="wrap", xlab="Used novelty language", log=TRUE, at=2:1)
points(novelty_mean[,2]~c(1,2), pch=19, col="red")
arrows(c(1,2),novelty_mean[,2]+novelty_se[,2],c(1,2),novelty_mean[,2]-novelty_se[,2], code=3, angle=90, length=0.1, col="red")
mtext("B)",side=3, adj=0.53, las=1,outer=TRUE, line=-2, cex=1.5)

}
dev.off()

### Impact factor model
summary(lm(log(JIF)~novelty + info_found,dd_scope))

