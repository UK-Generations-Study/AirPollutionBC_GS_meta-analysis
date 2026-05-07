###PM2.5 Bias Assessment Analysis-Subgroup Analysis
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
library("readxl")

###Set the working path
setwd("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Figures")

###Only prospective cohort studies (19 studies)
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis 05052026.xlsx", sheet = "PM2.5-Prospective")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

PM2.5_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$weights <- weights(PM2.5_res)  #Extract weights
data_sorted <- data[order(-data$weights),]  #Sort data by descending weight
PM2.5_res_sorted <- rma(yi = logHR_10, sei = SE_10, data = data_sorted, method = "DL", slab = data_sorted$Study)  #Re-run meta-analysis with sorted data
PM2.5_res <- PM2.5_res_sorted

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}

ppi <- 600
png(filename = 'PM2.5Prospective.png',width=6*ppi, height=9*ppi,res = ppi)
forest(PM2.5_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study","HR [95%CI]"), showweights = TRUE,
       xlab = "Hazard Ratio", mlab = paste0("Random effect model\nHeterogeneity: I² = ", round(PM2.5_res$I2, 2),"%, ",format_p(PM2.5_res$QEp)),
       xlim = c(-2,2),ylim = c(-3,length(PM2.5_res$yi)+3),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = length(PM2.5_res$yi) + 2,labels = "Weight",font = 2, cex = 0.8)
dev.off()


###Adjusted for SES (17 studies)
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis 05052026.xlsx", sheet = "PM2.5-SES")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

PM2.5_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$weights <- weights(PM2.5_res)  #Extract weights
data_sorted <- data[order(-data$weights),]  #Sort data by descending weight
PM2.5_res_sorted <- rma(yi = logHR_10, sei = SE_10, data = data_sorted, method = "DL", slab = data_sorted$Study)  #Re-run meta-analysis with sorted data
PM2.5_res <- PM2.5_res_sorted

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}

ppi <- 600
png(filename = 'PM2.5SES.png',width=6*ppi, height=9*ppi,res = ppi)
forest(PM2.5_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study","HR [95%CI]"), showweights = TRUE,
       xlab = "Hazard Ratio", mlab = paste0("Random effect model\nHeterogeneity: I² = ", round(PM2.5_res$I2, 2),"%, ",format_p(PM2.5_res$QEp)),
       xlim = c(-2,2),ylim = c(-3,length(PM2.5_res$yi)+3),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = length(PM2.5_res$yi) + 2,labels = "Weight",font = 2, cex = 0.8)
dev.off()


###Exposure measurement error (14 studies)
data <- read_excel("C:/Users/swang01/OneDrive - The Institute of Cancer Research/Desktop/PM and FBC/Updated Meta-analysis 05052026.xlsx", sheet = "PM2.5-Exposure")
View(data)

###Standardise the effect estimates (per 10 µg/m3 increase)
data$logHR <- log(data$Effect)
data$SE <- (log(data$Upper_CI) - log(data$Lower_CI)) / (2 * 1.96)
data$logHR_10 <- data$logHR * (10 / data$Unit_increase)
data$SE_10 <- data$SE * (10 / data$Unit_increase)
data$HR_10 <- exp(data$logHR_10)
data$LowerCI_10 <- exp(data$logHR_10 - 1.96 * data$SE_10)
data$UpperCI_10 <- exp(data$logHR_10 + 1.96 * data$SE_10)

PM2.5_res <- rma(yi = logHR_10,sei = SE_10,data = data,method = "DL",slab = data$Study)
data$weights <- weights(PM2.5_res)  #Extract weights
data_sorted <- data[order(-data$weights),]  #Sort data by descending weight
PM2.5_res_sorted <- rma(yi = logHR_10, sei = SE_10, data = data_sorted, method = "DL", slab = data_sorted$Study)  #Re-run meta-analysis with sorted data
PM2.5_res <- PM2.5_res_sorted

format_p <- function(p) {
  if (p < 0.001) {
    return("p < 0.001")
  } else {
    return(paste0("p = ", format(round(p, 3), nsmall = 3)))
  }
}

ppi <- 600
png(filename = 'PM2.5Exposure.png',width=6*ppi, height=9*ppi,res = ppi)
forest(PM2.5_res, at = log(c(0.5,1,1.5,2)), atransf = exp, header = c("Study","HR [95%CI]"), showweights = TRUE,
       xlab = "Hazard Ratio", mlab = paste0("Random effect model\nHeterogeneity: I² = ", round(PM2.5_res$I2, 2),"%, ",format_p(PM2.5_res$QEp)),
       xlim = c(-2,2),ylim = c(-3,length(PM2.5_res$yi)+3),lwd = 1.5, col = "maroon",border = "maroon",pch = 16, width = 2 ,shade = TRUE, colshade = "#EBEBEB",
       cex = 0.8, efac = 0.5)
text(x = 1,y = length(PM2.5_res$yi) + 2,labels = "Weight",font = 2, cex = 0.8)
dev.off()