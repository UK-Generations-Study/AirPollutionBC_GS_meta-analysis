######PM10######
#####Install and load required packages#####
install.packages("metafor")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("forestplot")

library("metafor")
library("dplyr")
library("tidyr")
library("ggplot2")
library("forestplot")

###Set the working path
setwd("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Figures")

###Initial meta-analysis with all studies (16 studies)
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis.xlsx", sheet = "PM10-All")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$weights <- weights(PM10_res)  #Extract weights
data_sorted <- data[order(-data$weights),]  #Sort data by descending weight
PM10_res_sorted <- rma(yi = logHR_10, sei = SE_10, data = data_sorted, method = "DL", slab = data_sorted$Study)  #Re-run meta-analysis with sorted data
PM10_res <- PM10_res_sorted

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
ppi <- 600
png(filename = 'PM10Initial.png',width=6*ppi, height=8*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study","HR [95%CI]"), showweights = TRUE,
       xlab = "Hazard Ratio", mlab = paste0("Random effect model\nHeterogeneity: I² = ", round(PM10_res$I2, 2),"%, ",format_p(PM10_res$QEp)),
       xlim = c(-3,2.5),ylim = c(-3,length(PM10_res$yi)+3),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = length(PM10_res$yi) + 2,labels = "Weight",font = 2, cex = 0.8)
dev.off()
###based on the figure, it looks like there are no influential studies
loo_df <- as.data.frame(leave1out(PM10_res))
loo_df$HR <- round(exp(loo_df$estimate), 2)
loo_df$CI <- paste0("(", round(exp(loo_df$ci.lb), 2), ", ", round(exp(loo_df$ci.ub), 2), ")")
loo_df$HR_95CI <- paste0(loo_df$HR,loo_df$CI)
loo_df$I2_new <- paste0(round(loo_df$I2,2),"%")
loo_df$P_value <- ifelse(loo_df$pval<0.001,"<0.001",loo_df$pval)
loo_df$Heterogeneity_Pvalue <- ifelse(loo_df$Qp<0.001,"<0.001",loo_df$Qp)
write.csv(loo_df,file = "C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Table/PM10InitialLOO.csv")
###but based on the leave-one-out analysis, Smotherman's study (2023) is an influential study (I2 changing from 86.87% to 28.21%)

###Primary meta-analysis with all studies (15 studies)
###***(1)Overall Forest plot***###
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis.xlsx", sheet = "PM10-Main")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$weights <- weights(PM10_res)  #Extract weights
data_sorted <- data[order(-data$weights),]  #Sort data by descending weight
PM10_res_sorted <- rma(yi = logHR_10, sei = SE_10, data = data_sorted, method = "DL", slab = data_sorted$Study)  #Re-run meta-analysis with sorted data
PM10_res <- PM10_res_sorted

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
ppi <- 600
png(filename = 'PM10Main.png',width=6*ppi, height=8*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study","HR [95%CI]"), showweights = TRUE,
       xlab = "Hazard Ratio", mlab = paste0("Random effect model\nHeterogeneity: I² = ", round(PM10_res$I2, 2),"%, ",format_p(PM10_res$QEp)),
       xlim = c(-3,2.5),ylim = c(-3,length(PM10_res$yi)+3),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = length(PM10_res$yi) + 2,labels = "Weight",font = 2, cex = 0.8)
dev.off()

###***(2)Funnel plot and Egger's test to check publication bias***###
egger_test <- regtest(PM10_res, model = "rma", predictor = "sei")
print(egger_test)
ppi <- 600
png(filename = 'PM10Funnelplot.png',width=6*ppi, height=8*ppi,res = ppi)
funnel(PM10_res,atransf = exp, at = log(c(0.5, 1, 1.5, 2)),ylim = c(0,0.35),yaxt = "n",xlab = "Hazard Ratio", ylab = "Standard Error")
axis(2, at = seq(0, 0.35, by = 0.05), las = 1)
text(x = log(0.45), y = 0.35, pos = 4,labels = paste0("Egger's test: z = ", round(egger_test$zval, 2),", p = ", signif(egger_test$pval, 3)),cex = 0.9)
dev.off()

###***(3)Leave-one-out analysis***###
loo_df <- as.data.frame(leave1out(PM10_res))
loo_df$HR <- round(exp(loo_df$estimate), 2)
loo_df$CI <- paste0("(", round(exp(loo_df$ci.lb), 2), ", ", round(exp(loo_df$ci.ub), 2), ")")
loo_df$HR_95CI <- paste0(loo_df$HR,loo_df$CI)
loo_df$I2_new <- paste0(round(loo_df$I2,2),"%")
loo_df$P_value <- ifelse(loo_df$pval<0.001,"<0.001",loo_df$pval)
loo_df$Heterogeneity_Pvalue <- ifelse(loo_df$Qp<0.001,"<0.001",loo_df$Qp)
write.csv(loo_df,file = "C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Table/PM10MainLOO.csv")

###***(4)Subgroup analysis***###
#***1.Study design (Cohort, case-control,ecological)***#
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis.xlsx", sheet = "PM10-Study_design")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

#Preparations for subgroup forest plots
data$Study_design <- factor(data$Study_design, levels = c("Cohort study","Case-control study","Ecological study"))
Numch <- nrow(subset(data, data$Study_design =="Cohort study"))
Numcc <- nrow(subset(data, data$Study_design =="Case-control study"))
Numec <- nrow(subset(data, data$Study_design =="Ecological study"))

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$Weight <- weights(PM10_res)
data <- do.call(rbind, lapply(split(data, data$Study_design), function(sub) {
  sub[order(sub$Weight), ]
}))
PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
PM10_sub <- rma(yi = logHR_10,sei = SE_10,mods = ~Study_design, data = data,method = "DL",slab = data$Study)

#Calculate the random effect of subgroups
res.ch <- rma(yi = logHR_10,sei = SE_10,subset = (Study_design=="Cohort study"),data = data,method = "DL",slab = data$Study)
res.cc <- rma(yi = logHR_10,sei = SE_10,subset = (Study_design=="Case-control study"),data = data,method = "DL",slab = data$Study)
res.ec <- rma(yi = logHR_10,sei = SE_10,subset = (Study_design=="Ecological study"),data = data,method = "DL",slab = data$Study)

#Draw forest plots
ppi <- 600
png(filename = 'PM10Design.png',width=7*ppi, height=11*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study Design","HR [95%CI]"), showweights = TRUE,
       addfit = FALSE, order = Study_design, rows = c(3:(Numch+2),(Numch+7):(Numch+Numcc+6),(Numch+Numcc+11):(Numch+Numcc+Numec+10)),
       xlab = "Hazard Ratio",xlim = c(-2,2),ylim = c(-3,Numch+Numcc+Numec+14),lwd = 1.5,pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8,efac = 0.5)
text(x = 1.2,y = Numch+Numcc+Numec+13,labels = "Weight",font = 2, cex = 0.8)
text(-2,c(Numch+3, Numch+Numcc+7, Numch+Numcc+Numec+11),pos = 4, c("Cohort Studies","Case-control Studies","Ecological Studies"),font = 2,cex = 0.8)

#Add heterogeneity for subgroup analyses
format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
text(x = -2, y = 1, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.ch$I2, 2), "%, ",format_p(res.ch$QEp)),
     cex = 0.8)
text(x = -2, y = Numch+5, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.cc$I2, 2), "%, ",format_p(res.cc$QEp)),
     cex = 0.8)
text(x = -2, y = Numch+Numcc+9, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.ec$I2, 2), "%, ",format_p(res.ec$QEp)),
     cex = 0.8)
text(x = -2, y = -1, pos = 4,labels = paste0("Test for subgroup differences: ", format_p(PM10_sub$QMp)),
     cex = 0.8, font = 2)

addpoly(res.ch, row = 2, mlab = "Random effect model for cohort", col = "skyblue", border = "skyblue")
addpoly(res.cc, row = Numch+6, mlab = "Random effect model for case-control", col = "skyblue", border = "skyblue")
addpoly(res.ec, row = Numch+Numcc+10, mlab = "Random effect model for ecological", col = "skyblue", border = "skyblue")
dev.off()

#***2.Study location (North America, Europe)***
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis.xlsx", sheet = "PM10-Study_location")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

data$Study_location <- factor(data$Study_location, levels = c("North America","Europe"))
Numna <- nrow(subset(data, data$Study_location =="North America"))
Numeu <- nrow(subset(data, data$Study_location =="Europe"))

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$Weight <- weights(PM10_res)
data <- do.call(rbind, lapply(split(data, data$Study_location), function(sub) {
  sub[order(sub$Weight), ]
}))
PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
PM10_sub <- rma(yi = logHR_10,sei = SE_10,mods = ~Study_location, data = data,method = "DL",slab = data$Study)

#Calculate the random effect of subgroups
res.na <- rma(yi = logHR_10,sei = SE_10,subset = (Study_location=="North America"),data = data,method = "DL",slab = data$Study)
res.eu <- rma(yi = logHR_10,sei = SE_10,subset = (Study_location=="Europe"),data = data,method = "DL",slab = data$Study)

#Draw forest plots
format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
ppi <- 600
png(filename = 'PM10Location.png',width=6*ppi, height=9*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study Location","HR [95%CI]"), showweights = TRUE,
       addfit = FALSE,order = Study_location, rows = c(3:(Numna+2),(Numna+7):(Numna+Numeu+6)),
       xlab = "Hazard Ratio",xlim = c(-3,2.5),ylim = c(-3,Numna+Numeu+10),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1.2,y = Numna+Numeu+9,labels = "Weight",font = 2, cex = 0.8)
text(-3,c(Numna+3, Numna+Numeu+7),pos = 4, c("North America","Europe"),font = 2,cex = 0.8)

#Add heterogeneity for subgroup analyses
text(x = -3, y = 1, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.na$I2, 2), "%, ",format_p(res.na$QEp)),
     cex = 0.8)
text(x = -3, y = Numna+5, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.eu$I2, 2), "%, ",format_p(res.eu$QEp)),
     cex = 0.8)
text(x = -3, y = -1, pos = 4,labels = paste0("Test for subgroup differences: ", format_p(PM10_sub$QMp)),
     cex = 0.8, font = 2)

addpoly(res.na, row = 2, mlab = "Random effect model for North America", col = "skyblue", border = "skyblue")
addpoly(res.eu, row = Numna+6, mlab = "Random effect model for Europe", col = "skyblue", border = "skyblue")
dev.off()

#***3.Menopausal status (pre vs post)***
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis.xlsx", sheet = "PM10-Menopausal")
View(data)

data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

data$Menopausal_status <- factor(data$Menopausal_status, levels = c("Premenopause","Postmenopause"))
Numpre <- nrow(subset(data, data$Menopausal_status =="Premenopause"))
Numpost <- nrow(subset(data, data$Menopausal_status =="Postmenopause"))

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$Weight <- weights(PM10_res)
data <- do.call(rbind, lapply(split(data, data$Menopausal_status), function(sub) {
  sub[order(sub$Weight), ]
}))
PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
PM10_sub <- rma(yi = logHR_10,sei = SE_10,mods = ~Menopausal_status, data = data,method = "DL",slab = data$Study)

#Calculate the random effect of subgroups
res.pre <- rma(yi = logHR_10,sei = SE_10,subset = (Menopausal_status=="Premenopause"),data = data,method = "DL",slab = data$Study)
res.post <- rma(yi = logHR_10,sei = SE_10,subset = (Menopausal_status=="Postmenopause"),data = data,method = "DL",slab = data$Study)

#Draw forest plots
format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
ppi <- 600
png(filename = 'PM10Menopausal.png',width=7*ppi, height=9*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Menopausal Status","HR [95%CI]"), showweights = TRUE,
       addfit = FALSE,order = Menopausal_status, rows = c(3:(Numpre+2),(Numpre+7):(Numpre+Numpost+6)),
       xlab = "Hazard Ratio", xlim = c(-2,2),ylim = c(-3,Numpre+Numpost+10),lwd = 1.5,pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = Numpre+Numpost+9,labels = "Weight",font = 2, cex = 0.8)
text(-2,c(Numpre+3, Numpre+Numpost+7),pos = 4, c("Premenopause","Postmenopause"),font = 2,cex = 0.8)

#Add heterogeneity for subgroup analyses
text(x = -2, y = 1, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.pre$I2, 2), "%, ",format_p(res.pre$QEp)),
     cex = 0.8)
text(x = -2, y = Numpre+5, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.post$I2, 2), "%, ",format_p(res.post$QEp)),
     cex = 0.8)
text(x = -2, y = -1, pos = 4,labels = paste0("Test for subgroup differences: ", format_p(PM10_sub$QMp)),
     cex = 0.8, font = 2)

addpoly(res.pre, row = 2, mlab = "Random effect model for premenopausal", col = "skyblue", border = "skyblue")
addpoly(res.post, row = Numpre+6, mlab = "Random effect model for postmenopausal", col = "skyblue", border = "skyblue")

dev.off()

#***4.Tumour subtype (ER+/PR+ vs ER-/PR-)***
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/Updated Meta-analysis.xlsx", sheet = "PM10-Subtype")
View(data)

data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

data$Tumor_subtype <- factor(data$Tumor_subtype, levels = c("ER+/PR+","ER-/PR-"))
Numpos <- nrow(subset(data, data$Tumor_subtype =="ER+/PR+"))
Numneg <- nrow(subset(data, data$Tumor_subtype =="ER-/PR-"))

PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$Weight <- weights(PM10_res)
data <- do.call(rbind, lapply(split(data, data$Tumor_subtype), function(sub) {
  sub[order(sub$Weight), ]
}))
PM10_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
PM10_sub <- rma(yi = logHR_10,sei = SE_10,mods = ~Tumor_subtype, data = data,method = "DL",slab = data$Study)

#Calculate the random effect of subgroups
res.pos <- rma(yi = logHR_10,sei = SE_10,subset = (Tumor_subtype=="ER+/PR+"),data = data,method = "DL",slab = data$Study)
res.neg <- rma(yi = logHR_10,sei = SE_10,subset = (Tumor_subtype=="ER-/PR-"),data = data,method = "DL",slab = data$Study)

#Draw forest plots
format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}
ppi <- 600
png(filename = 'PM10Subtype.png',width=7*ppi, height=10*ppi,res = ppi)
forest(PM10_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Hormone Receptor Status","HR [95%CI]"), showweights = TRUE,
       addfit = FALSE,order = Tumor_subtype, rows = c(3:(Numpos+2),(Numpos+7):(Numpos+Numneg+6)),
       xlab = "Hazard Ratio",xlim = c(-2,2),ylim = c(-3,Numpos+Numneg+10),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1.2,y = Numpos+Numneg+9,labels = "Weight",font = 2, cex = 0.8)
text(-2,c(Numpos+3, Numpos+Numneg+7),pos = 4, c("ER+/PR+","ER-/PR-"),font = 2,cex = 0.8)

#Add heterogeneity for subgroup analyses
text(x = -2, y = 1, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.pos$I2, 2), "%, ",format_p(res.pos$QEp)),
     cex = 0.8)
text(x = -2, y = Numpos+5, pos = 4,labels = paste0("Heterogeneity: I² = ", round(res.neg$I2, 2), "%, ",format_p(res.neg$QEp)),
     cex = 0.8)
text(x = -2, y = -1, pos = 4,labels = paste0("Test for subgroup differences: ", format_p(PM10_sub$QMp)),
     cex = 0.8, font = 2)

addpoly(res.pos, row = 2, mlab = "Random effect model for ER+/PR+", col = "skyblue", border = "skyblue")
addpoly(res.neg, row = Numneg+6, mlab = "Random effect model for ER-/PR-", col = "skyblue", border = "skyblue")

dev.off()