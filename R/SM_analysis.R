library(beeswarm)
library(viridis)
library(scales)

# setwd("/Users/joelpick/github/replication_policy")

#load data
dd <- read.csv("./Data/replication_data.csv")
pub <- read.csv("./Data/publishers.csv")
dd$JIF <- ifelse(dd$JIF=="<0.1",0.1,as.numeric(dd$JIF))

dd$Publisher2<-pub$Publisher2[match(dd$Journal,pub$Journal)]



## remove out of scope papers
dd_scope <- subset(dd,info_found!="Out of scope for this journal")

## number out of scope
nrow(dd) - nrow(dd_scope)

## add on journal Impact Factors (JIF)
dd_scope$JIF <- ifelse(dd_scope$JIF=="<0.1",0.1,as.numeric(dd_scope$JIF))

# group replication policy and accepting categories into single variables
dd_scope$replication_policy <- ifelse(dd_scope$info_found=="Yes", 
	ifelse(dd_scope$replications_mention=="Yes",dd_scope$replication_policy_mentions,dd_scope$replication_policy_not
		),NA)
dd_scope$accept_category <- ifelse(dd_scope$info_found=="Yes", 
	ifelse(dd_scope$replications_mention=="Yes",dd_scope$accept_category_mentions,dd_scope$accept_category_not
		),NA)


pub_tab<-table(dd_scope$Publisher2,dd_scope$replication_policy)
pub_tab[rowSums(pub_tab)>0,c(1,3,2,4)]

## whether info is found or not 
pub_tab2<-table(dd_scope$Publisher2,dd_scope$info_found)

pub_tab2[pub_tab2[,2]>0,]
pub_tab2[rowSums(pub_tab2)==1,]

mult_pub<-rownames(pub_tab2[rowSums(pub_tab2)>1,])

dd_scope$mult_pub<-dd_scope$Publisher2 %in% mult_pub
boxplot(JIF_log~mult_pub,dd_scope, horizontal=TRUE, las=1)


hist(pub_tab2[,2]/rowSums(pub_tab2))
var(pub_tab2[,2]/rowSums(pub_tab2))
mean(pub_tab2[,2]/rowSums(pub_tab2))
olre<-1:nrow(pub_tab2)
summary(glm(as.matrix(pub_tab2)~1,family="binomial"))
summary(lme4::glmer(as.matrix(pub_tab2)~1 + (1|olre),family="binomial"))

par(mar=c(4,10,1,1))
boxplot(JIF_log~Publisher2,dd_scope, horizontal=TRUE, las=1)

plot(pub_tab2[,2]~rowSums(pub_tab2))


############
#### Sensitivity analysis - publisher
############

dd_scope$JIF_log <- log(dd_scope$JIF)

model_full<-lme4::lmer(JIF_log~novelty + info_found + (1|Publisher2),dd_scope )
model_noN<-lme4::lmer(JIF_log~info_found + (1|Publisher2),dd_scope )
model_noR<-lme4::lmer(JIF_log~novelty + (1|Publisher2),dd_scope )
anova(model_full,model_noN)
anova(model_full,model_noR)
summary(model_full)

model_noRE<-lm(JIF_log~novelty + info_found,dd_scope )
summary(model_noRE)

var(log(dd_scope$JIF))

model_noRE2<-lm(JIF_log~novelty + info_found,dd_scope, subset=Publisher2 %in% mult_pub )
summary(model_noRE2)

model_full2<-lme4::lmer(JIF_log~novelty + info_found + (1|Publisher2),dd_scope, subset=Publisher2 %in% mult_pub  )
summary(model_full2)
model_noN2<-lme4::lmer(JIF_log~info_found + (1|Publisher2),dd_scope , subset=Publisher2 %in% mult_pub  )
model_noR2<-lme4::lmer(JIF_log~novelty + (1|Publisher2),dd_scope , subset=Publisher2 %in% mult_pub  )
anova(model_full2,model_noN2)
anova(model_full2,model_noR2)

mult_pub






