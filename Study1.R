rm(list=ls())
data <- readxl::read_excel("TouchTRT.xlsx")
data$Age <-2021-data$`Anno di nascita`

## pre-test
# reverse items
data[,c(156, 161, 162, 163, 165, 167, 168, 170, 171, 172, 173, 174, 178, 180, 181, 184, 186, 187, 189)] <- 6-data[,c(156, 161, 162, 163, 165, 167, 168, 170, 171, 172, 173, 174, 178, 180, 181, 184, 186, 187, 189)]
data[,c(193, 195, 198, 199, 202, 205, 206, 208, 211)] <- 6-data[,c(193, 195, 198, 199, 202, 205, 206, 208, 211)]
data[,c(234,236,255,261)] <- 6-data[,c(234,236,255,261)]


data$TAQ_PARTNER_PRE <- apply(data[,c(156, 157, 158, 159, 160, 162, 163, 164, 166, 169)], 1, mean, na.rm=T)
data$TAQ_FAMILY_PRE <- apply(data[,c(171, 172, 173, 174, 175, 176)], 1, mean, na.rm=T)
data$TAQ_SAMESEX_PRE <- apply(data[,c(177, 178, 179, 180, 181, 182)], 1, mean, na.rm=T)
data$TAQ_OPPSEX_PRE <- apply(data[,c(183, 184, 185, 186, 187, 188)], 1, mean, na.rm=T)
data$TAQ_STRANGER_PRE <- apply(data[,c(189, 190, 192)], 1, mean, na.rm=T)
data$MSP_PRE <- apply(data[,213:261], 1, mean, na.rm=T)
data$STAI_PRE <- apply(data[,193:212], 1, mean, na.rm=T)



## post-test
# reverse items
data[,c(38, 39, 40, 41, 45, 47, 50, 56, 57, 58, 60, 63, 64, 66, 69, 70, 71, 73)] <- 6-data[,c(38, 39, 40, 41, 45, 47, 50, 56, 57, 58, 60, 63, 64, 66, 69, 70, 71, 73)]
data[,c(74, 76, 79, 80, 83, 86, 87, 89, 92)] <- 6-data[,c(74, 76, 79, 80, 83, 86, 87, 89, 92)]
data[,c(115, 117, 136, 142)] <- 6-data[,c(115, 117, 136, 142)]


data$TAQ_PARTNER_POST <- apply(data[,c(37,38,39,40,41,43,44,45,47,50)], 1, mean, na.rm=T)
data$TAQ_FAMILY_POST <- apply(data[,c(52,53,54,55,56,57)], 1, mean, na.rm=T)
data$TAQ_SAMESEX_POST <- apply(data[,c(58,59,60,61,62,63)], 1, mean, na.rm=T)
data$TAQ_OPPSEX_POST <- apply(data[,c(64,65,66,67,68,69)], 1, mean, na.rm=T)
data$TAQ_STRANGER_POST <- apply(data[,c(70,71,73)], 1, mean, na.rm=T)
data$MSP_POST <- apply(data[,94:142], 1, mean, na.rm=T)
data$STAI_POST <- apply(data[,74:93], 1, mean, na.rm=T)

# RQ1
t.test(data$TAQ_PARTNER_PRE, data$TAQ_PARTNER_POST, paired=T)
t.test(data$TAQ_FAMILY_PRE, data$TAQ_FAMILY_POST, paired=T)
t.test(data$TAQ_SAMESEX_PRE, data$TAQ_SAMESEX_POST, paired=T)
t.test(data$TAQ_OPPSEX_PRE, data$TAQ_OPPSEX_POST, paired=T)
t.test(data$TAQ_STRANGER_PRE, data$TAQ_STRANGER_POST, paired=T)
t.test(data$MSP_PRE, data$MSP_POST, paired=T)
t.test(data$STAI_PRE, data$STAI_POST, paired=T)

effsize::cohen.d(d=data$TAQ_PARTNER_POST, f=data$TAQ_PARTNER_PRE, paired=T, data=data)
effsize::cohen.d(d=data$TAQ_FAMILY_POST, f=data$TAQ_FAMILY_PRE, paired=T, data=data)
effsize::cohen.d(d=data$TAQ_SAMESEX_POST, f=data$TAQ_SAMESEX_PRE, paired=T, data=data)
effsize::cohen.d(d=data$TAQ_OPPSEX_POST, f=data$TAQ_OPPSEX_PRE, paired=T, data=data)
effsize::cohen.d(d=data$TAQ_STRANGER_POST, f=data$TAQ_STRANGER_PRE, paired=T, data=data)
effsize::cohen.d(d=data$MSP_PRE, f=data$MSP_POST, paired=T, data=data)
effsize::cohen.d(d=data$STAI_PRE, f=data$STAI_POST, paired=T, data=data)

# RQ2

data$TAQ_STRANGER_DIFF <- data$TAQ_STRANGER_POST-data$TAQ_STRANGER_PRE
data$TAQ_FAMILY_DIFF <- data$TAQ_FAMILY_POST-data$TAQ_FAMILY_PRE
data$TAQ_SAMESEX_DIFF <- data$TAQ_SAMESEX_POST-data$TAQ_SAMESEX_PRE
data$TAQ_OPPSEX_DIFF <- data$TAQ_OPPSEX_POST-data$TAQ_OPPSEX_PRE
data$TAQ_PARTNER_DIFF <- data$TAQ_PARTNER_POST-data$TAQ_PARTNER_PRE

summary(lm(MSP_POST ~ TAQ_PARTNER_DIFF + TAQ_FAMILY_DIFF + TAQ_SAMESEX_DIFF + TAQ_OPPSEX_DIFF + TAQ_STRANGER_DIFF + Age + Sesso, data=data))
summary(lm(STAI_POST ~ TAQ_PARTNER_DIFF + TAQ_FAMILY_DIFF + TAQ_SAMESEX_DIFF + TAQ_OPPSEX_DIFF + TAQ_STRANGER_DIFF + Age + Sesso, data=data))
