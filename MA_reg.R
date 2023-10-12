library(dplyr)
library(effects)
library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggeffects)
library(sjPlot)
library(sjmisc)
library(emmeans)
devtools::install_github("sammo3182/interplot")
library(interplot)
library(jtools)
library(interactions)

9#############################################################################
########################### BUILD FINAL DATASET #############################
saveRDS(MA.data, "MA.data.rds")
#note: MA.data is in this folder because I fixed it and saved it here

#note:
#IL and MA has handles for some reason but no handlecount of legis
#NM and NV have regular old handle_count

#build
final.data <- bind_rows(AL.data, DE.data, ID.data, KS.data, KY.data, LA.data)
final.data <- subset (final.data, select = -handle_count.x)
names(final.data)[names(final.data) == 'handle_count.y'] <- 'handle_count'

final.data <- bind_rows(final.data, NM.data, NV.data)
IL.data <- subset (IL.data, select = -handle)
MA.data <- subset (MA.data, select = -handle)

final.data <- bind_rows(final.data, IL.data, MA.data)

final.data$race[final.data$race == "White?"] <- "White"
final.data[final.data$name==433, "race"] <- "Latino"

saveRDS(final.data, "final.data.rds")

######## final data 2
#remove the Indep. party variables 
final.data2 <- subset(final.data, !(party == "(I)"))
test <- subset(final.data, (party == "(I)"))

final.data2$gen <- as.factor(final.data2$gender)
final.data2$rac <- as.factor(final.data2$race)
final.data2$rac2 <- relevel(final.data2$rac, ref = 6)
final.data2$part <- as.factor(final.data2$party)

#replace NA's with 0's
final.data2[is.na(final.data2)] <- 0
final.data2$gen[final.data2$gen == "femlae"] <- "female"

saveRDS(final.data2, "final.data2.rds")

######## final data 3
#add the fixed MA state in it
final.data3 <- subset(final.data2, state!="MA")
final.data3 <- final.data3[,1:18]
final.data3 <- bind_rows(final.data3, MA.data2)

#check that the number of legislators matches number of observations
length(unique(final.data3$name))

#check that the party/gender/race varaiables for issues
print(unique(final.data3$gender))
final.data3$gender[final.data3$gender == "femlae"] <- "female"

print(unique(final.data3$party)) #susannah whipps is still in there
x3<- which(final.data3$party == "0") 
final.data3 <- subset(final.data3, name!="Susannah Whipps")

print(unique(final.data3$race))

final.data3$gen <- as.factor(final.data3$gender)
final.data3$rac <- as.factor(final.data3$race)
final.data3$rac2 <- relevel(final.data3$rac, ref = 6)
final.data3$part <- as.factor(final.data3$party)
saveRDS(final.data3, "final.data3.rds")


#############################################################################
################################# ANALYSIS ##################################

############################ DESCRIPTIVE STATS ##############################
##hist of Sev and tox averages
#tox
ggplot(final.data4, aes(x=TOX.avg)) + 
  geom_histogram(color="lightblue", fill="lightblue") +
  labs(title="Average Toxicity Score Histogram",x="Average Toxicity Score", y = "Count")
#sev tox
ggplot(final.data4, aes(x=SEV.TOX.avg)) + 
  geom_histogram(color="darkblue", fill="darkblue") +
  labs(title="Average Severe Toxicity Score Histogram",x="Average Severe Toxicity Score", y = "Count")

##find Toxic tweets
tox <- subset(scored.KY.ma, TOXICITY < 0.1)
sevtox <- subset(scored.IL.ma, SEVERE_TOXICITY < 0.1)

##9 legislators individual histograms
#NV
NV <- subset(scored.NV.ma, name == "Ben Kieckhefer")
NVt <- ggplot(NV, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Nevada", x="Toxicity Score", y = "Count")
NVst <- ggplot(NV, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Ben Kieckhefer",x="Severe Toxicity Score", y = "Count")

#MA
MA <- subset(scored.MA.ma, name == "Adam G. Hinds")
MAt <- ggplot(MA, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title = "Massachusetts", x="Toxicity Score", y = "Count")
MAst <- ggplot(MA, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title = "Adam G. Hinds", x="Severe Toxicity Score", y = "Count")

#LA
LA <- subset(scored.LA.ma, name == "Malinda Brumfield White")
LAt <- ggplot(LA, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Louisiana", x="Toxicity Score", y = "Count")
LAst <- ggplot(LA, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Malinda Brumfield White", x="Severe Toxicity Score", y = "Count")

#KY
KY <- subset(scored.KY.ma, name == "Kimberly Poore Moser")
KYt <- ggplot(KY, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title="Kentucky", x="Toxicity Score", y = "Count")
KYst <- ggplot(KY, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title="Kimberly Poore Moser", x="Severe Toxicity Score", y = "Count")

#IL
IL <- subset(scored.IL.ma, name == "Tim Butler")
ILt <- ggplot(IL, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title="Illinois", x="Toxicity Score", y = "Count")
ILst <- ggplot(IL, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title="Tim Butler", x="Severe Toxicity Score", y = "Count")

#ID
ID <- subset(scored.ID.ma, name == "Alison Rabe")
IDt <- ggplot(ID, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title = "Idaho",x="Toxicity Score", y = "Count")
IDst <- ggplot(ID, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title = "Alison Rabe",x="Severe Toxicity Score", y = "Count")

#DE
DE <- subset(scored.DE.ma, name == "Kyle Gay")
DEt <- ggplot(DE, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Delaware", x="Toxicity Score", y = "Count")
DEst <- ggplot(DE, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title = "Kyle Gay", x="Severe Toxicity Score", y = "Count")

#AL
AL <- subset(scored.AL.ma, name == "Jack W. Williams")
ALt <- ggplot(AL, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title= "Alabama", x="Toxicity Score", y = "Count")
ALst <- ggplot(AL, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title= "Jack W. Williams", x="Severe Toxicity Score", y = "Count")

#NM
NM <- subset(scored.NM.ma, name == "Roger Montoya")
NMt <- ggplot(NM, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title="New Mexico", x="Toxicity Score", y = "Count")
NMst <- ggplot(NM, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue3", fill="slateblue3")+
  labs(title="Roger Montoya", x="Severe Toxicity Score", y = "Count")

#KS
KS <- subset(scored.KS.ma, name == "Stephanie Clayton")
KSt <- ggplot(KS, aes(x=TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title="Kansas", x="Toxicity Score", y = "Count")
KSst <- ggplot(KS, aes(x=SEVERE_TOXICITY)) + 
  geom_histogram(color="slateblue1", fill="slateblue1")+
  labs(title="Stephanie Clayton", x="Severe Toxicity Score", y = "Count")

grid.arrange(NVt, NVst, MAt, MAst, LAt, LAst, KYt, KYst,ILt, ILst, ncol=2)
             
grid.arrange(IDt, IDst, DEt, DEst, ALt, ALst, NMt, NMst, KSt, KSst, ncol=2)

##mentions distribution
ggplot(data=final.data, aes(x=name, y=mentions)) +
  geom_bar(stat="identity", fill="steelblue")

##how many... race
race_breakdown <- subset(final.data4, race == "Multiracial")
  #latino = 50, white = 661, black = 97, asian american = 15, multiracial = 9, Native A: 6 
#men/women
gender_breakdown <- subset(final.data4, gender == "male")
#D/R,
party_breakdown <- subset(final.data4, party == "(R)")


############################ REGRESSION  ##############################
unique(final.data$race)

final.data$gen <- as.factor(final.data$gender)
final.data$rac <- as.factor(final.data$race)
final.data$rac2 <- relevel(final.data$rac, ref = 6)
final.data$part <- as.factor(final.data$party)

final.data$gen2<- (as.numeric(final.data$gen)-1)/2 #converts to binary numeric

###### TOXIC SCORES
#regular model - no interactions
m1.tox <- lm(TOX.avg ~ gen + rac2 + part + 
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LTOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m1.tox)

#gender x race
m2.tox <- lm(TOX.avg ~ gen + rac2 + part + gen*rac2 +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LTOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m2.tox)

#expected values
emm2.tox <- emmeans(m2.tox, ~ gen*rac2)
emmip2.tox <- emmip(m2.tox, gen ~ rac2, CIs=TRUE, plotit=FALSE)
p <- ggplot(data= emmip2.tox, aes(x=rac2, y = yvar,fill = gen)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9))+
  labs(title="Gender x Race Interaction ", x="Race", y = "Predicted Means")
p <- p + guides(fill=guide_legend(title="Gender"))
p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggsave(file ="m2.tox.png",  width = 10, height = 6)


#race x part
m3.tox <- lm(TOX.avg ~ gen + rac2 + part + rac2*part +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LTOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m3.tox)

emm3.tox <- emmeans(m3.tox, ~ rac2*part)

emmip3.tox <- emmip(m3.tox, rac2 ~ part, CIs=TRUE, plotit=FALSE)

m3 <- ggplot(data= emmip3.tox, aes(x=part, y = yvar,fill = rac2)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9)) +
  labs(title="Race x Party Interaction ", x="Party", y = "Predicted Means")
m3 <- m3 + guides(fill=guide_legend(title="Race"))


#gender x part
m4.tox <- lm(TOX.avg ~ gen + rac2 + part + gen*part +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LTOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m4.tox)

emm4.tox <- emmeans(m4.tox, ~ gen*part)

emmip4.tox <- emmip(m4.tox, gen ~ part, CIs=TRUE, plotit=FALSE)

m4 <- ggplot(data= emmip4.tox, aes(x=part, y = yvar,fill = gen)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9)) +
  labs(title="Gender x Party Interaction ", x="Gender", y = "Predicted Means")
m4 <- m4 + guides(fill=guide_legend(title="Gender"))

library(stargazer)
stargazer(m1.tox, m2.tox, m3.tox, m4.tox, type = "html", out = "Final_tox4.doc")


###### SEVERE TOXIC SCORES
#regular model - no interactions
m1.sevtox <- lm(SEV.TOX.avg ~ gen + rac2 + part + 
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
                 LSEV.TOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m1.sevtox)

#gender x race
m2.sevtox <- lm(SEV.TOX.avg ~ gen + rac2 + part + gen*rac2 +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LSEV.TOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m2.sevtox)

emm2.sevtox <- emmeans(m2.sevtox, ~ gen*rac2)

emmip2.tox <- emmip(m2.sevtox, gen ~ rac2, CIs=TRUE, plotit=FALSE)

m2s <- ggplot(data= emmip2.tox, aes(x=rac2, y = yvar,fill = gen)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9))+
  labs(title="Gender x Race Interaction ", x="Race", y = "Predicted Means")
m2s <- m2s + guides(fill=guide_legend(title="Gender"))
m2s <- m2s + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#race x part
m3.sevtox <- lm(SEV.TOX.avg ~ gen + rac2 + part + rac2*part +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LSEV.TOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m3.sevtox)

emm3.sevtox <- emmeans(m3.sevtox, ~ rac2*part)

emmip3.sevtox <- emmip(m3.sevtox, rac2 ~ part, CIs=TRUE, plotit=FALSE)

m3s <- ggplot(data= emmip3.sevtox, aes(x=part, y = yvar,fill = rac2)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9)) +
  labs(title="Race x Party Interaction ", x="Party", y = "Predicted Means")
m3s <- m3s + guides(fill=guide_legend(title="Race"))


#gender x part
m4.sevtox <- lm(SEV.TOX.avg ~ gen + rac2 + part + gen*part +
               handle_count + mentions + state +
               like.avg + retweet.avg + reply.avg +
               LSEV.TOX.avg + Llike.avg + Lretweet.avg + Lreply.avg, data = final.data3)
summary(m4.sevtox)

emm4.sevtox <- emmeans(m4.sevtox, ~ gen*part)

emmip4.sevtox <- emmip(m4.sevtox, gen ~ part, CIs=TRUE, plotit=FALSE)

m4s <- ggplot(data= emmip4.sevtox, aes(x=part, y = yvar,fill = gen)) +
  geom_point(size=3 ,shape=21,position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), position=position_dodge(0.9)) +
  labs(title="Gender x Party Interaction ", x="Party", y = "Predicted Means")
m4s <- m4s + guides(fill=guide_legend(title="Gender"))

stargazer(m1.sevtox, m2.sevtox, m3.sevtox, m4.sevtox, type = "html", out = "Final_sevtox4.doc")

############################# SCRAPZ ###############################
plot(effect("gen:part", m4.tox), multiline = TRUE, colors = c("pink", "blue"),
     xlab = "Party", main = "Model 4: Gender and Party Interaction Plot")


sjPlot::plot_model(m3.tox, type = "pred", terms = c("rac2", "part"))

#hypothetical latino republican = -0.004(1) + 0.014(1) + 0.021(1) + 0.071(1)(1) + 
#    average handle count in data(coeff for handle count) + avg men (ceoff for men) + coeff MA(1)
#    avg # of avg. likes (coeff of avg. likes) + same retweets + same replies etc etc.
# once I run this I get the expected toxicity for this hypothetical latino republican person
# latino democrat, white republican is the comparisons- these are also significant in the model 
# on average - this many unit more toxicity (take difference in expected values) etc. 

#marginal effects
interplot(m = m2.tox, var1 = "gen2", var2 = "rac2", ci = .9, point = T)

#jtools
jtools::cat_plot(m2.tox, pred = "gen", modx = "rac2")

cat_plot(m2.tox, pred = "rac2", modx = "gen")

