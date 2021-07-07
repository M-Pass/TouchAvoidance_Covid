rm(list=ls())

# scoring of pre-pandemic results
load("R Data/TouchAvoid.RData")
Pre <- NULL

# old script... a bit of a mess, but it works
data = rbind(Source1$MSP, Source2$MSP, Source3$MSP[,-52])
Demo = rbind(Source1$Demogr, Source2$Demogr, Source3$Demogr)
Demo$Gender <- substr(tolower(Demo$sesso), 1, 1) == "m"
Source2$TAQ = Source2$TAQ[ , !names(Source2$TAQ)%in%c("Col40", "Col41", "Col42")]
Source3$TAQ = Source3$TAQ[ , !names(Source3$TAQ)%in%c("Col40", "Col41", "Col42")]

data[,c(24, 26, 45, 51)] <- 6-data[,c(24, 26, 45, 51)]

Pre$MSP_PRE <- apply(data[,3:51], 1, mean, na.rm=T)

data = rbind(Source1$STAI, Source2$STAI, Source3$STAI)
data[,c(3, 5, 8, 9, 12, 15, 16, 18, 21)] <- 6-data[,c(3, 5, 8, 9, 12, 15, 16, 18, 21)]
Pre$STAI_PRE <- apply(data[,3:22], 1, mean, na.rm=T)

TAQ = rbind(Source1$TAQ, Source2$TAQ, Source3$TAQ)
###

TAQ$TAQ_9 <- as.numeric(TAQ$TAQ_9)
TAQ$TAQ_19 <- as.numeric(TAQ$TAQ_19)

TAQ.Samesex.items = paste("TAQ_", c(22,24,27), sep="")
TAQ.Samesex.invitems = paste("TAQ_", c(23, 25, 26), sep="")
TAQ.Oppsex.items = paste("TAQ_", c(28,30,33), sep="")
TAQ.Oppsex.invitems = paste("TAQ_", c(29, 31, 32), sep="")
TAQ.Family.items = paste("TAQ_", c(20, 21), sep="")
TAQ.Family.invitems = paste("TAQ_", c(16, 17, 18, 19), sep="")
TAQ.Partner.items = paste("TAQ_", c(2, 3, 4, 5, 9, 11, 14), sep="")
TAQ.Partner.invitems = paste("TAQ_", c(1, 7, 8), sep="")
TAQ.Stranger.items = paste("TAQ_", c(34, 35, 37), sep="")

TAQ[, TAQ.Samesex.invitems] = 6 - TAQ[, TAQ.Samesex.invitems]
TAQ[, TAQ.Oppsex.invitems] = 6 - TAQ[, TAQ.Oppsex.invitems]
TAQ[, TAQ.Family.invitems] = 6 - TAQ[, TAQ.Family.invitems]
TAQ[, TAQ.Partner.invitems] = 6 - TAQ[, TAQ.Partner.invitems]

TAQ$TAQ.samesex.tot = apply(TAQ[, c(TAQ.Samesex.items, TAQ.Samesex.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.oppsex.tot = apply(TAQ[, c(TAQ.Oppsex.items, TAQ.Oppsex.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.family.tot = apply(TAQ[, c(TAQ.Family.items, TAQ.Family.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.partner.tot = apply(TAQ[, c(TAQ.Partner.items, TAQ.Partner.invitems)], 1, mean, na.rm=T)
TAQ$TAQ.stranger.tot = apply(TAQ[, c(TAQ.Stranger.items)], 1, mean, na.rm=T)


Pre$TAQ_PARTNER_PRE <- TAQ$TAQ.partner.tot
Pre$TAQ_FAMILY_PRE <- TAQ$TAQ.oppsex.tot
Pre$TAQ_SAMESEX_PRE <- TAQ$TAQ.family.tot
Pre$TAQ_OPPSEX_PRE <- TAQ$TAQ.oppsex.tot
Pre$TAQ_STRANGER_PRE <- TAQ$TAQ.stranger.tot

# scoring of post-pandemic results

data <- readxl::read_excel("Post1005.xlsx")
data$Età <- as.numeric(data$Età)
colnames(data)[18] <- "ConosceMorti"
data$Sesso[data$Sesso == "Preferisco non rispondere"] <- NA

# fixing age for participants who provided year of birth instead of age
data$Età[1:40] <- 2021-as.numeric(data$Età[1:40])
data$Età[2] <- NA
data$Età[40] <- NA
data$Età[data$Età == "1974"] <- 2021-1974
data$Age60 <- data$Età > 60

# removing 2 test administrations
data <- data[-c(1,2),]

# removing data for participants who did not provide consent
data <- data[data$`Consenso informato per la partecipazione allo studio` != "Non do il mio consenso",]

# reverse items
data[,c(38, 39, 40, 41, 45, 47, 50, 56, 57, 58, 60, 63, 64, 66, 69, 70, 71, 73)] <- 6-data[,c(38, 39, 40, 41, 45, 47, 50, 56, 57, 58, 60, 63, 64, 66, 69, 70, 71, 73)]
data[,c(115, 117, 136, 142)] <- 6-data[,c(115, 117, 136, 142)]
data[,c(74, 76, 79, 80, 83, 86, 87, 89, 92)] <- 6-data[,c(74, 76, 79, 80, 83, 86, 87, 89, 92)]

Post <- NULL
Post$TAQ_PARTNER_POST <- apply(data[,c(37,38,39,40,41,43,44,45,47,50)], 1, mean, na.rm=T)
Post$TAQ_FAMILY_POST <- apply(data[,c(52,53,54,55,56,57)], 1, mean, na.rm=T)
Post$TAQ_SAMESEX_POST <- apply(data[,c(58,59,60,61,62,63)], 1, mean, na.rm=T)
Post$TAQ_OPPSEX_POST <- apply(data[,c(64,65,66,67,68,69)], 1, mean, na.rm=T)
Post$TAQ_STRANGER_POST <- apply(data[,c(70,71,73)], 1, mean, na.rm=T)
Post$MSP_POST <- apply(data[,94:142], 1, mean, na.rm=T)
Post$STAI_POST <- apply(data[,74:93], 1, mean, na.rm=T)
Post$COVID1 <- apply(data[,148:149], 1, mean, na.rm=T)
Post$COVID2 <- apply(data[,150:151], 1, mean, na.rm=T)
Post$COVID3 <- apply(data[,152:153], 1, mean, na.rm=T)
Post$COVID4 <- apply(data[,154:155], 1, mean, na.rm=T)
Post$Età <- data$Età
Post$Sesso <- data$Sesso
Post$Age60 <- Post$Età > 60
Post$Vaccine <- data$`E' stato stato vaccinato per il COVID?` == "Sì"
Post$ConosceMorti <- data$ConosceMorti == "Sì"

# RQ1

t.test(Pre$TAQ_PARTNER_PRE, Post$TAQ_PARTNER_POST)
t.test(Pre$TAQ_FAMILY_PRE, Post$TAQ_FAMILY_POST)
t.test(Pre$TAQ_SAMESEX_PRE, Post$TAQ_SAMESEX_POST)
t.test(Pre$TAQ_OPPSEX_PRE, Post$TAQ_OPPSEX_POST)
t.test(Pre$TAQ_STRANGER_PRE, Post$TAQ_STRANGER_POST)
t.test(Pre$MSP_PRE, Post$MSP_POST)
t.test(Pre$STAI_PRE, Post$STAI_POST)

unadj.p.values <- c(t.test(Pre$TAQ_PARTNER_PRE, Post$TAQ_PARTNER_POST)$p.value,
                    t.test(Pre$TAQ_FAMILY_PRE, Post$TAQ_FAMILY_POST)$p.value,
                    t.test(Pre$TAQ_SAMESEX_PRE, Post$TAQ_SAMESEX_POST)$p.value,
                    t.test(Pre$TAQ_OPPSEX_PRE, Post$TAQ_OPPSEX_POST)$p.value,
                    t.test(Pre$TAQ_STRANGER_PRE, Post$TAQ_STRANGER_POST)$p.value,
                    t.test(Pre$MSP_PRE, Post$MSP_POST)$p.value,
                    t.test(Pre$STAI_PRE, Post$STAI_POST)$p.value)

p.adjust(unadj.p.values, method="BH")

effsize::cohen.d(Pre$TAQ_PARTNER_PRE, Post$TAQ_PARTNER_POST, na.rm=T)
effsize::cohen.d(Pre$TAQ_FAMILY_PRE, Post$TAQ_FAMILY_POST, na.rm=T)
effsize::cohen.d(Pre$TAQ_SAMESEX_PRE, Post$TAQ_SAMESEX_POST, na.rm=T)
effsize::cohen.d(Pre$TAQ_OPPSEX_PRE, Post$TAQ_OPPSEX_POST, na.rm=T)
effsize::cohen.d(Pre$TAQ_STRANGER_PRE, Post$TAQ_STRANGER_POST, na.rm=T)
effsize::cohen.d(Pre$MSP_PRE, Post$MSP_POST, na.rm=T)
effsize::cohen.d(Pre$STAI_PRE, Post$STAI_POST, na.rm=T)

# RQ2

summary(lm(MSP_POST ~ Età + Sesso + TAQ_PARTNER_POST + TAQ_FAMILY_POST + TAQ_SAMESEX_POST + TAQ_OPPSEX_POST + TAQ_STRANGER_POST + COVID1 + COVID2 + COVID3 + COVID4, data=Post))
summary(lm(STAI_POST ~ Età + Sesso + TAQ_PARTNER_POST + TAQ_FAMILY_POST + TAQ_SAMESEX_POST + TAQ_OPPSEX_POST + TAQ_STRANGER_POST + COVID1 + COVID2 + COVID3 + COVID4, data=Post))

# RQ3
summary(lm(TAQ_PARTNER_POST ~ Sesso + Age60 + Vaccine + ConosceMorti + COVID1 + COVID2 + COVID3 + COVID4, data=Post))
summary(lm(TAQ_FAMILY_POST ~ Sesso + Age60 + Vaccine + ConosceMorti + COVID1 + COVID2 + COVID3 + COVID4, data=Post))
summary(lm(TAQ_SAMESEX_POST ~ Sesso + Age60 + Vaccine + ConosceMorti + COVID1 + COVID2 + COVID3 + COVID4, data=Post))
summary(lm(TAQ_OPPSEX_POST ~ Sesso + Age60 + Vaccine + ConosceMorti + COVID1 + COVID2 + COVID3 + COVID4, data=Post))
summary(lm(TAQ_STRANGER_POST ~ Sesso + Age60 + Vaccine + ConosceMorti + COVID1 + COVID2 + COVID3 + COVID4, data=Post))

