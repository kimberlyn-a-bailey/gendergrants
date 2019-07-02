############################################################################Data prep

library(ggplot2)
library(ggthemes) # Load
library(ggpubr)

############################################################################NIH Data

nihData = read.csv("/Users/user/Desktop/Gender Boldness/NIH_2006to2017.csv")

nihDataFemale = nihData[nihData$Gender=="female",]
nihDataMale = nihData[nihData$Gender=="male",]

t.test(log(nihDataFemale$Award),log(nihDataMale$Award))
ggdensity(log(nihDataFemale$Award))
ggdensity(log(nihDataMale$Award))
t.test(log(nihDataFemale$certain+0.01),log(nihDataMale$certain+0.01))
ggdensity(log(nihDataFemale$certain+0.01))
ggdensity(log(nihDataMale$certain+0.01))
t.test(log(nihDataFemale$tentat+0.01),log(nihDataMale$tentat+0.01))
ggdensity(log(nihDataFemale$tentat+0.01))
ggdensity(log(nihDataMale$tentat+0.01))
t.test(log(nihDataFemale$insight+0.01),log(nihDataMale$insight+0.01))
ggdensity(log(nihDataFemale$insight+0.01))
ggdensity(log(nihDataMale$insight+0.01))
t.test(log(nihDataFemale$cause+0.01),log(nihDataMale$cause+0.01))
ggdensity(log(nihDataFemale$cause+0.01))
ggdensity(log(nihDataMale$cause+0.01))

ggplot(nihData, aes(x=Gender, y=log(Award))) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nihData, aes(x=Gender, y=certain)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nihData, aes(x=Gender, y=tentat)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nihData, aes(x=Gender, y=insight)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nihData, aes(x=Gender, y=cause)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

mod <- lm(log(nihData$Award) ~ nihData$Gender + log(nihData$certain+0.01)+log(nihData$insight+0.01)+log(nihData$cause+0.01)+log(nihData$tentat+0.01))
summary(mod)

############################################################################NSF Data

#Read in data
nsfData <- read.csv("/Users/user/Desktop/Gender Boldness/NSF_2006to2017.csv")

nsfDataFemale = nsfData[nsfData$Gender=="female",]
nsfDataMale = nsfData[nsfData$Gender=="male",]

t.test(log(nsfDataFemale$Award),log(nsfDataMale$Award))
ggdensity(log(nsfDataFemale$Award))
ggdensity(log(nsfDataMale$Award))
t.test(log(nsfDataFemale$certain+0.01),log(nsfDataMale$certain+0.01))
ggdensity(log(nsfDataFemale$certain+0.01))
ggdensity(log(nsfDataMale$certain+0.01))
t.test(log(nsfDataFemale$tentat+0.01),log(nsfDataMale$tentat+0.01))
ggdensity(log(nsfDataFemale$tentat+0.01))
ggdensity(log(nsfDataMale$tentat+0.01))
t.test(log(nsfDataFemale$insight+0.01),log(nsfDataMale$insight+0.01))
ggdensity(log(nsfDataFemale$insight+0.01))
ggdensity(log(nsfDataMale$insight+0.01))
t.test(log(nsfDataFemale$cause+0.01),log(nsfDataMale$cause+0.01))
ggdensity(log(nsfDataFemale$cause+0.01))
ggdensity(log(nsfDataMale$cause+0.01))

ggplot(nsfData, aes(x=Gender, y=log(Award))) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nsfData, aes(x=Gender, y=certain)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nsfData, aes(x=Gender, y=tentat)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nsfData, aes(x=Gender, y=insight)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(nsfData, aes(x=Gender, y=cause)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

mod <- lm(log(nsfData$Award) ~ log(nsfData$certain+0.01)+log(nsfData$insight+0.01)+log(nsfData$cause+0.01)+log(nsfData$tentat+0.01))
mod <- lm(log(nsfData$Award) ~ log(nsfData$cause+0.01))

summary(mod)

############################################################################Markowitz Data

#Read in data
markData <- read.csv("/Users/user/Desktop/Gender Boldness/MarkowitzNSFData.csv")

print(mean(markDataFemale$AwardedAmountToDate))
print(mean(markDataMale$AwardedAmountToDate))

markDataFemale = markData[markData$Gender=="female",]
markDataMale = markData[markData$Gender=="male",]

t.test(log(markDataFemale$AwardedAmountToDate),log(markDataMale$AwardedAmountToDate))
ggdensity(log(markDataFemale$AwardedAmountToDate))
ggdensity(log(markDataMale$AwardedAmountToDate))
t.test(log(markDataFemale$certain+0.01),log(markDataMale$certain+0.01))
ggdensity(log(markDataFemale$certain+0.01))
ggdensity(log(markDataMale$certain+0.01))
t.test(log(markDataFemale$tentat+0.01),log(markDataMale$tentat+0.01))
ggdensity(log(markDataFemale$tentat+0.01))
ggdensity(log(markDataMale$tentat+0.01))
t.test(log(markDataFemale$insight+0.01),log(markDataMale$insight+0.01))
ggdensity(log(markDataFemale$insight+0.01))
ggdensity(log(markDataMale$insight+0.01))
t.test(log(markDataFemale$cause+0.01),log(markDataMale$cause+0.01))
ggdensity(log(markDataFemale$cause+0.01))
ggdensity(log(markDataMale$cause+0.01))

print(length(markData$Gender))
print(length(markData$AwardedAmountToDate))

ggplot(markData, aes(x=markData$Gender, y=log(markData$AwardedAmountToDate))) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(markData, aes(x=Gender, y=certain)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(markData, aes(x=Gender, y=tentat)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(markData, aes(x=Gender, y=insight)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

ggplot(markData, aes(x=Gender, y=cause)) + theme_hc() + 
  geom_boxplot(outlier.shape=NA, fill=(c("pink","lightblue")))  + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.3, height=0),col="gray",alpha=0.3)

mod <- lm(log(markData$AwardedAmountToDate) ~ log(markData$certain+0.01)+log(markData$insight+0.01)+log(markData$cause+0.01)+log(markData$tentat+0.01))
mod <- lm(log(markData$AwardedAmountToDate) ~  markData$Gender +markData$Dic)
mod <- lm(log(markData$AwardedAmountToDate) ~  markData$Gender + markData$certain + markData$cause + markData$WC + markData$tentat +markData$Dic)
summary(mod)


