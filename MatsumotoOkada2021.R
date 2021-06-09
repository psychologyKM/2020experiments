setwd("XXX/2020experiments")
requiredPackages <- c("cowplot", "dplyr", "ggplot2", "ggthemes", "gtable", "multcomp", "plotrix", "psych","remotes", "reshape2", "RColorBrewer", "stringr", "tidyverse")
installList <- requiredPackages[!is.element(requiredPackages,installed.packages())]
lapply(installList, install.packages)
library(tidyverse)
library(ggplot2)
library(ggthemes)
desc <- function(v) {
  return(c(median(v, na.rm = T), mean(v, na.rm = T), sd(v, na.rm = T)))
}


# STUDY01 -------------------------------------------------------------------

dat01 <- readRDS("datasets/rData/dat01.rdata")
dat01$sum.likeW <- rowSums(dat01[,grep("likeW",colnames(dat01))])/4
dat01$sum.likeM <- rowSums(dat01[,grep("likeM",colnames(dat01))])/4
dat01$sum.admire <- rowSums(dat01[,grep("admire",colnames(dat01))])/4
dat01$sum.empathy <- (rowSums(dat01[,grep("empathy",colnames(dat01))]))/4
dat01$sum.mentalAware <- (rowSums(dat01[,grep("mentalAware",colnames(dat01))]))/4
dat01$sum.physicalAware <- (rowSums(dat01[,grep("physicalAware",colnames(dat01))]))/4
dat01$sum.imagine <- (rowSums(dat01[,grep("imagine",colnames(dat01))]))/4
dat01$sum.inspire <- (rowSums(dat01[,grep("inspire",colnames(dat01))]))/5

# descriptive statistics

v_desc <- c(grep("frequency",colnames(dat01)), grep("sum",colnames(dat01)))

for(i in v_desc) {
  tmp <- tapply(dat01[,i],dat01$condition,desc) 
  print(colnames(dat01)[i])
  print(tmp)
}

# Cronbach's alpha
psych::alpha(dat01[,30:34])

# regression analyses

oneway.test(frequency ~ condition, data = dat01, var.equal = T)

amod <- lm(sum.physicalAware ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.mentalAware ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.admire ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.likeW ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.inspire ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.empathy ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

amod <- lm(sum.imagine ~ condition + frequency + condition:frequency, data = dat01)
glht.res.t <- multcomp::glht(amod, linfct = multcomp::mcp(condition = "Tukey"))
summary(glht.res.t)
summary(amod)

# other analyses

cols <- data.frame(dat01$condition, dat01$sum.likeM, dat01$sum.likeW)
cor(subset(cols, dat01.condition == "Tracing")[,2:3], use = "pairwise.complete.obs")
cor(subset(cols, dat01.condition == "NoTracing")[,2:3], use = "pairwise.complete.obs")
cor(subset(cols, dat01.condition == "Control")[,2:3], use = "pairwise.complete.obs")


# STUDY02 (rating)-----------------------------------------------------------------

dat02 <- readRDS("datasets/rData/dat02.rdata")
dat02$sum.admire <- rowSums(dat02[,grep("admire",colnames(dat02))])/4
dat02$sum.empathy <- (rowSums(dat02[,grep("empathy",colnames(dat02))]))/4
dat02$sum.mentalAware <- (rowSums(dat02[,grep("mentalAware",colnames(dat02))]))/4
dat02$sum.physicalAware <- (rowSums(dat02[,grep("physicalAware",colnames(dat02))]))/4
dat02$sum.imagine <- (rowSums(dat02[,grep("imagine",colnames(dat02))]))/4
dat02$sum.inspire <- (rowSums(dat02[,grep("inspire",colnames(dat02))]))/5

# descriptive statistics

v_desc <- c(grep("frequency",colnames(dat02)), grep("sum",colnames(dat02)))

for(i in v_desc) {
  tmp <- tapply(dat02[,i],dat02$condition,desc) 
  print(colnames(dat02)[i])
  print(tmp)
}

# Cronbach's alpha
psych::alpha(dat02[,23:27])

# regression analyses 

t.test(frequency ~ condition, data = dat02, var.equal = T)
summary(lm(sum.physicalAware ~ place + condition*frequency, data = dat02))
summary(lm(sum.mentalAware ~ place + condition*frequency, data = dat02))
summary(lm(sum.admire ~ place + condition*frequency, data = dat02))
summary(lm(sum.inspire ~ place + condition*frequency, data = dat02))
summary(lm(sum.empathy ~ place + condition*frequency, data = dat02))
summary(lm(sum.imagine ~ place + condition*frequency, data = dat02))


# STUDY2 (heart rate) -----------------------------------------------------

HR <- read.csv("datasets/rData/Mprime.csv", header=T)
HR = rbind(subset(HR, order == 0 & baseline != 15), subset(HR, order != 0 & firstTime != 15 & secondTime != 15))

dat02$Mprime_base <- NA
dat02$Mprime_first <- NA
dat02$Mprime_second <- NA

for (i in 1:nrow(dat02)){
  currentIDn = dat02$IDn[i]
  currentBase = subset(HR,(IDn == currentIDn & order == 0))
  currentFirstAndSecond = subset(HR,(IDn == currentIDn & order != 0))
  currentAll <- c(currentBase$baseline, currentFirstAndSecond$firstTime, currentFirstAndSecond$secondTime)
  if (nrow(currentBase) > 0 && 
      nrow(currentFirstAndSecond) > 1 && 
      sd(currentAll) < 2){
    dat02$Mprime_base[i] <- currentBase$baseline
    dat02$Mprime_first[i] <- mean(currentFirstAndSecond$first)
    dat02$Mprime_second[i] <- mean(currentFirstAndSecond$second)
  }
}

dat02$MprimeRatioF <- dat02$Mprime_first / dat02$Mprime_base
dat02$MprimeRatioS <- dat02$Mprime_second / dat02$Mprime_base
dat02$hb <- 1800 / dat02$Mprime_base
dat02$hp1 <- 1800 / dat02$Mprime_first
dat02$hp2 <- 1800 / dat02$Mprime_second

tapply(dat02$hb ,dat02$condition, desc) 
tapply(dat02$hp1 ,dat02$condition, desc) 
tapply(dat02$hp2 ,dat02$condition, desc) 

summary(lm(log(1 / MprimeRatioF) ~ place + condition*frequency, data = dat02))
summary(lm(log(1 / MprimeRatioS) ~ place + condition*frequency, data = dat02))

table(subset(dat02, !is.na(Mprime_base))[,c(1,28)])

summary(lm(log(1 / MprimeRatioS) ~ place + condition*years, data = subset(dat02, frequency == 0)))
summary(lm(log(1 / MprimeRatioS) ~ place + condition*years, data = subset(dat02, frequency > 0)))


# FIGURES -----------------------------------------------------------------

#Figure 3

dat01.melt <- reshape2::melt(data.frame(dat01$condition, dat01$sum.admire, dat01$sum.empathy, dat01$sum.inspire))
dat01.melt <- dplyr::mutate(dat01.melt, variable = stringr::str_replace_all(variable, pattern = "dat01.sum.",replacement   = ""))
dat01.melt <- dplyr::mutate(dat01.melt, variable = stringr::str_replace_all(variable, pattern = "admire",replacement = "ADMIRATION"))
dat01.melt <- dplyr::mutate(dat01.melt, variable = stringr::str_replace_all(variable, pattern = "empathy",replacement = "EMPATHY"))
dat01.melt <- dplyr::mutate(dat01.melt, variable = stringr::str_replace_all(variable, pattern = "inspire",replacement = "INSPIRATION"))
dat01.melt2 <- transform(dat01.melt, Condition = factor(dat01.condition, levels = c("Tracing", "NoTracing", "Control")))
cols <- RColorBrewer::brewer.pal(6, "Paired")
p1 <- ggplot(data = dat01.melt2, aes(x = Condition , y = value, fill = Condition)) + 
  theme_classic() +
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 6,length = 7),limits=c(0, 6)) +
  scale_fill_manual(values = cols[c(5, 3, 1)]) +
  facet_grid(. ~ variable) + 
  labs(title = "Study 1", x = NULL, y = "Rating Score") +
  theme(legend.position = "none",
        axis.title.y = element_text(angle = 0, size = 10, vjust = 0.5),
        axis.text.x = element_text(size = 10)) + 
  scale_x_discrete(labels = c("Tracing\nGroup", "No-Tracing\nGroup", "Control\nGroup"))

dat02.melt <- reshape2::melt(data.frame(dat02$condition, dat02$sum.admire))
dat02.melt <- dplyr::mutate(dat02.melt, variable = stringr::str_replace_all(variable, pattern = "dat02.sum.",replacement   = ""))
dat02.melt <- dplyr::mutate(dat02.melt, variable = stringr::str_replace_all(variable, pattern = "admire",replacement = "ADMIRATION"))
dat02.melt2 <- transform(dat02.melt, Condition = factor(dat02.condition, levels = c("e", "c")))

p2 <- ggplot(data = dat02.melt2, aes(x = Condition , y = value, fill = Condition)) + 
  theme_classic() +
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 6,length = 7),limits=c(0, 6)) +
  scale_fill_manual(values = cols[c(5, 1)]) +
  facet_grid(. ~ variable) + 
  labs(title = "Study 2", x = NULL, y = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10)) + 
  scale_x_discrete(labels = c("Tracing\nGroup", "Control\nGroup"))

cowplot::plot_grid(p1, p2, rel_widths = c(4,1))

# Figure 7

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
my_color = rainbow(6, alpha=0.6)

raincloud <- function(title, ylab, frequencyLL, frequencyUL, showLegend){
    dat <- subset(dat_f, frequency >= frequencyLL & frequencyUL >= frequency)
    p <- ggplot(dat, aes(x = condition, y = log(1/MprimeRatioS))) +
      geom_flat_violin(scale = "count", trim = F) +
      stat_summary(fun.data = mean_sdl, 
                 fun.args = list(mult = 1), 
                 geom = "pointrange", 
                 position = position_nudge(0.05)) + 
      geom_dotplot(data = subset(dat, condition == "e"), 
                 aes (fill = factor(years)), binaxis = "y", 
                 dotsize = 2,
                 binwidth = 0.005,
                 stackdir = "down", 
                 stackgroups = T,
                 stackratio = 1,
                 binpositions = "all",
                 position = position_nudge(-0.025),
                 width = 0,
                 show.legend = showLegend) + 
      geom_dotplot(data = subset(dat, condition == "c"), 
                 aes (fill = factor(years)), binaxis = "y", 
                 dotsize = 2,
                 binwidth = 0.005,
                 stackdir = "down", 
                 stackgroups = T,
                 stackratio = 1,
                 binpositions = "all",
                 position = position_nudge(-0.025),
                 width = 0,
                 show.legend = showLegend) + 
      scale_fill_manual(values = my_color[c(5,3,2,1)],
                        labels = c("0: never or less than a year", "1: one to five years", "2: five to ten years", "3: ten years or more"),
                        guide = guide_legend(override.aes = list(size = 3, alpha = 0.6, color = "black"))) +
      theme_classic() +
      scale_x_discrete(labels = c("Tracing\nGroup", "Control\nGroup")) +
      scale_y_continuous(limits = c(-0.25, 0.25)) +
      labs(title = title, x = NULL, y =  ylab, fill = "Length of time spent learning calligraphy") 
    return(p)
}

dat_f <- transform(dat02, condition = factor(condition, levels = c("e", "c")))
dat_f <- transform(dat_f, years = factor(years, levels = c("0","1","2","3")))
dat_f <- dat_f[!is.na(dat_f$MprimeRatioS),]

p_ylab <- gtable::gtable_filter(ggplotGrob(raincloud(NULL, "Heart Rate Change Ratio for Second Time Period (log)", 0, 3, FALSE)), pattern = "ylab-l")
p3 <- raincloud("Low Frequency Group", NULL, 0, 0, FALSE)
p4 <- raincloud("High Frequency Group", NULL, 1, 3, FALSE)
p_legend <- gtable::gtable_filter(ggplotGrob(raincloud(NULL, NULL, 0, 3, TRUE)), pattern = "guide-box")

cowplot::plot_grid(p_ylab, p3, p4, p_legend, nrow = 1, rel_widths = c(0.1,1.5,1.5,1))
