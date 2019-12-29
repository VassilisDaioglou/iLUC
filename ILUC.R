# R script uder to evaluate and present results of "Meeting SDGs 2 & 13 within global constraints" study
# ---- START ----
# clear memory
rm(list=ls()) 

# Load Libraries
#library(reshape);
library(reshape2);
library(ggplot2);
library(plyr);
library(dplyr)
library(data.table);
library(tidyr)
library(stringr)
library(xlsx)
library(ggpubr)
library(grid)
library(gridExtra)
library(gdata)

#
# ---- DATA ----
ppi <- 300
# set directory path 
setwd("C:/Users/Asus/Documents/Github/iLUC/")
# Read Data File
DATA=read.csv("data/ILUCfactor_4R.csv", sep=";", dec=".", stringsAsFactors = FALSE, colClasses = "character")
DATA[] <- lapply(DATA, as.character)
colnames(DATA)[1] <- "AUTHOR"
DATA=melt(DATA, id.vars=c("AUTHOR","YEAR","TITLE","MODEL","METHOD","BIOFUEL","FEEDSTOCK","TYPE"), na.rm=TRUE)
DATA$YEAR = as.numeric(substr(DATA$YEAR, start=1, stop=4))
DATA$value = as.numeric(substr(DATA$value, start=1, stop=4))
DATA = na.omit(DATA)

# Make relevant data file
DATA2 = DATA
DATA2 = subset(DATA2, select=-c(TITLE,MODEL,TYPE,variable))
DATA2$FuelID = DATA2$BIOFUEL
DATA2$FuelID[DATA2$BIOFUEL=="1st generation ethanol"] <- "Ethanol"
DATA2$FuelID[DATA2$BIOFUEL=="Advanced Ethanol"] <- "Ethanol"

DATA2$FeedID = DATA2$FEEDSTOCK
DATA2$FeedID <-gsub( "Waste Cooking Oil","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Sorghum","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Oilseeds","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "wheat","Wheat",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Short Rotation Coppice","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Eucalyptus/Poplas/Switchgrass","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Switchgrass","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Advanced-miscanthus","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Wheat straw","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Willow-poplar","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Jatropha","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Cassava-Molasses","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Corn Stover","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Miscanthus","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Barley","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Maize Sillage","Other",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Cereal straw","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Perrenials","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Forest Residues","Advanced",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "Rapeseed/Canola","Rapeseed",DATA2$FeedID,fixed=F)
DATA2$FeedID <-gsub( "multiple","Other",DATA2$FeedID,fixed=F)

DATA2$TechID = paste(DATA2$FeedID, DATA2$FuelID)

DATA2$TechID_order = factor(DATA2$TechID, levels=c("Sugarcane Ethanol",
                                             "Maize Ethanol",
                                             "Sugarbeet Ethanol",
                                             "Wheat Ethanol",
                                             "Advanced Ethanol",
                                             "Other Ethanol",
                                             "Rapeseed Biodiesel",
                                             "Soybean Biodiesel",
                                             "Palm Biodiesel",
                                             "Sunflower Biodiesel",
                                             "Other Biodiesel"))

DATA2$FeedID_order = factor(DATA2$FeedID, levels=c("Sugarcane",
                                                   "Maize",
                                                   "Sugarbeet",
                                                   "Wheat",
                                                   "Advanced",
                                                   "Rapeseed",
                                                   "Soybean",
                                                   "Palm",
                                                   "Sunflower",
                                                   "Other"))


# ---- DATA: Sensitivity ----
DATA2.sens = DATA2
DATA2.sens$StudyID = paste(DATA2.sens$AUTHOR,DATA2.sens$YEAR)
DATA2.sens = subset(DATA2.sens, StudyID=="Valin 2015"|
                                StudyID=="Plevin 2010"|
                                StudyID=="Plevin 2015"|
                                StudyID=="Laborde 2011"|
                                StudyID=="Taheripour 2013"|
                                StudyID=="Mullins 2011"|
                                StudyID=="Moreira 2014")

DATA2.sens$StudyID_order = factor(DATA2.sens$StudyID, levels=c("Plevin 2010",
                                                               "Laborde 2011",
                                                               "Mullins 2011",
                                                               "Taheripour 2013",
                                                               "Moreira 2014",
                                                               "Plevin 2015",
                                                               "Valin 2015"))

DATA2.sens$SensID = paste(DATA2.sens$StudyID,DATA2.sens$TechID)

Ranges <- aggregate(DATA2.sens$value, by=list(StudyID_order=DATA2.sens$StudyID_order,TechID_order=DATA2.sens$TechID_order,METHOD=DATA2.sens$METHOD,SensID=DATA2.sens$SensID), FUN=min, na.rm=F)
colnames(Ranges)[5]<-"min"

Ranges2 <- aggregate(DATA2.sens$value, by=list(StudyID_order=DATA2.sens$StudyID_order,TechID_order=DATA2.sens$TechID_order,METHOD=DATA2.sens$METHOD,SensID=DATA2.sens$SensID), FUN=max, na.rm=F)
colnames(Ranges2)[5]<-"max"

Ranges3 <- aggregate(DATA2.sens$value, by=list(StudyID_order=DATA2.sens$StudyID_order,TechID_order=DATA2.sens$TechID_order,METHOD=DATA2.sens$METHOD,SensID=DATA2.sens$SensID), FUN=median, na.rm=F)
colnames(Ranges3)[5]<-"mean"

Ranges$max <- Ranges2[match(Ranges$SensID,Ranges2$SensID),5]
Ranges$mean <- Ranges3[match(Ranges$SensID,Ranges3$SensID),5]

rm(Ranges2,Ranges3)
#
# ---- DIAGNOSTICS ----
RefList = as.data.table(table(DATA2$AUTHOR))
ObsCount = length(DATA2$value)

# Overall
Stats = data.frame(Technology="Total")
Stats$Mean=mean(DATA2$value)
Stats$Median=median(DATA2$value)
Stats$Minimum=min(DATA2$value)
Stats$Maximum=max(DATA2$value)
Stats$sd=sd(DATA2$value)
colnames(Stats)[6] <- "SD"

# Per Technology
Stats2 <- aggregate(DATA2$value, by=list(REGION=DATA2$TechID_order), FUN=mean, na.rm=TRUE)
Stats3 <- aggregate(DATA2$value, by=list(REGION=DATA2$TechID_order), FUN=median, na.rm=TRUE)
Stats4 <- aggregate(DATA2$value, by=list(REGION=DATA2$TechID_order), FUN=min, na.rm=TRUE)
Stats5 <- aggregate(DATA2$value, by=list(REGION=DATA2$TechID_order), FUN=max, na.rm=TRUE)
Stats6 <- aggregate(DATA2$value, by=list(REGION=DATA2$TechID_order), FUN=sd, na.rm=TRUE)

StatsFuel <- merge(Stats2, Stats3, by.x="REGION", by.y="REGION")
colnames(StatsFuel)[2:3] <- c("Mean","Median")
StatsFuel <- merge(StatsFuel, Stats4, by.x="REGION", by.y="REGION")
colnames(StatsFuel)[4] <- "Minimum"
StatsFuel <- merge(StatsFuel, Stats5, by.x="REGION", by.y="REGION")
colnames(StatsFuel)[5] <- "Maximum"
StatsFuel <- merge(StatsFuel, Stats6, by.x="REGION", by.y="REGION")
colnames(StatsFuel)[6] <- "SD"
colnames(StatsFuel)[1] <- "Technology"

# Combined
StatsFuel = rbind(StatsFuel,Stats)

rm(Stats,Stats2,Stats3,Stats4,Stats5,Stats6)

StatsFuel= StatsFuel %>% mutate(sd_per_mean=SD/Mean)
colnames(StatsFuel)[6:7] <- c("Std. Dev.","Std. Dev / Mean")

#
# ---- LABELS <empty>----
# var_labels <- c("Population"="Population (Mil.)",
#                 
# FontSize=15 # For presentation
# FontSize3=12 # For presentation - axes
# 
# FontSize2=8 # For draft
# 
# #
# ---- FIGURES ----
# ---- > FIG: f(tech) ----

#DATA2$TechID_order2 <- paste(DATA2$TechID_order,2)

ILUC <- ggplot(DATA2) + 
  geom_boxplot(aes(x=TechID_order, y=value), lwd=0.2, alpha=0, width=0.3) +
  geom_point(aes(x=as.numeric(TechID_order)+0.5, y=value, colour=METHOD, shape=METHOD), size=1.2, position=position_dodge(0.5)) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + geom_vline(xintercept=0,size = 0.1, colour='black') +
  geom_vline(xintercept=c(1.8,2.8,3.8,4.8,5.8,6.8,7.8,8.8,9.8,10.8), size=0.1, colour="black") +
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab("") +
  theme_bw() +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=0.5, vjust=0.5), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  theme(axis.ticks.x = element_line(colour="transparent")) +
  scale_colour_manual(values=c("cornsilk4","blue","firebrick","forestgreen","cornflowerblue"),
                    name ="",
                    breaks=c("LCA","CGE","CD","PE","Empirical"),
                    labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,2,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) 
ILUC

#
# ---- > FIG: f(t,tech) ----
ILUC2 <- ggplot(DATA2) + 
  geom_point(aes(x=YEAR, y=value, colour=METHOD, shape=METHOD), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("ILUC factors across technologies and assessment methods") + theme(plot.title = element_text(face="bold", size=8)) +
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab("Year of Publication") +
  scale_x_continuous(breaks=seq(2008,max(DATA2$YEAR),1), limits=c(2008,max(DATA2$YEAR))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("cornsilk4","blue","firebrick","forestgreen","cornflowerblue"),
                    name ="",
                    breaks=c("LCA","CGE","CD","PE","Empirical"),
                    labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,2,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  facet_wrap(~TechID_order)
  #facet_grid(BIOFUEL~FEEDSTOCK)
ILUC2

#
# ---- > FIG: Variation ----
VAR <- ggplot(DATA2.sens) + 
  geom_point(aes(x=StudyID_order, y=value, colour=METHOD, shape=METHOD), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab(expression("")) +
  theme_bw() + theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=75, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("cornsilk4","blue","forestgreen","cornflowerblue"),
                      name ="",
                      breaks=c("LCA","CGE","CD","PE","Empirical"),
                      labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  facet_wrap(~TechID_order)
VAR


VAR2 <- ggplot(Ranges) + 
  geom_errorbar(mapping=aes(x=StudyID_order, ymin=min, ymax=max), width=0.2, size=0.2, color="black") +
  geom_point(aes(x=StudyID_order, y=mean, colour=METHOD, shape=METHOD), size=1.2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab(expression("")) +
  theme_bw() + theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=75, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("cornsilk4","blue","forestgreen","cornflowerblue"),
                      name ="",
                      breaks=c("LCA","CGE","CD","PE","Empirical"),
                      labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  facet_wrap(~TechID_order)
VAR2

VAR3 <- ggplot(Ranges) + 
  geom_errorbar(mapping=aes(x=TechID_order, ymin=min, ymax=max), width=0.2, size=0.2, color="black") +
  geom_point(aes(x=TechID_order, y=mean, colour=StudyID_order, shape=StudyID_order), size=1.2) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab(expression("")) +
  theme_bw() + theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=75, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  # scale_colour_manual(values=c("cornsilk4","blue","forestgreen","cornflowerblue"),
  #                     name ="",
  #                     breaks=c("LCA","CGE","CD","PE","Empirical"),
  #                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  # ) +
  # scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9),name="",
  #                    breaks=c("LCA","CGE","CD","PE","Empirical"),
  #                    labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  facet_wrap(~METHOD)
VAR3

#
# ---- > FIG: f(t,fuel) ----
Ethanol <- ggplot(subset(DATA2, BIOFUEL=="1st generation ethanol"|BIOFUEL=="Advanced Ethanol")) + 
  geom_point(aes(x=YEAR, y=value, colour=METHOD, shape=METHOD), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ggtitle("Ethanol") + theme(plot.title = element_text(face="bold", size=8)) +
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab("Year of Publication") +
  scale_x_continuous(breaks=seq(2008,max(DATA2$YEAR),1), limits=c(2008,max(DATA2$YEAR))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  theme(legend.position="none", legend.text=element_text(size=7), legend.title=element_text(face="bold.italic")) +
  scale_colour_manual(values=c("cornsilk4","blue","firebrick","forestgreen","cornflowerblue"),
                      name ="",
                      breaks=c("LCA","CGE","CD","PE","Empirical"),
                      labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,2,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  facet_wrap(~FeedID_order, nrow=1)
Ethanol

Biodiesel <- ggplot(subset(DATA2, BIOFUEL=="Biodiesel")) + 
  geom_point(aes(x=YEAR, y=value, colour=METHOD, shape=METHOD), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("Biodiesel") + theme(plot.title = element_text(face="bold", size=8)) +
  ylab(expression("ILUC Factor, kgCO"[2]*"-eq/GJ"[Biofuel])) + xlab("Year of publication") +
  scale_x_continuous(breaks=seq(2008,max(DATA2$YEAR),1), limits=c(2008,max(DATA2$YEAR))) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 7, face="bold")) +
  theme(text= element_text(size=7, face="plain"), axis.text.x = element_text(angle=66, size=7, hjust=1), axis.text.y = element_text(size=7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2), panel.grid.minor = element_blank()) +
  scale_colour_manual(values=c("cornsilk4","blue","firebrick","forestgreen","cornflowerblue"),
                      name ="",
                      breaks=c("LCA","CGE","CD","PE","Empirical"),
                      labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")
  ) +
  scale_shape_manual(values=c(0,1,2,4,5),name="",
                     breaks=c("LCA","CGE","CD","PE","Empirical"),
                     labels=c("HLCA","CGE","Causal Descriptive","PE","Empirical")) +
  theme(legend.position="bottom", legend.text=element_text(size=6), legend.title=element_text(face="bold.italic")) +
  facet_wrap(~FeedID_order, nrow=1)
Biodiesel

layout<-rbind(1,1,1,1,1,
              2,2,2,2,2,2,2) 
ILUC3 <- grid.arrange(Ethanol,Biodiesel,layout_matrix=layout)
rm(layout)

#
# # ---- OUTPUT: FOR DRAFT ----
# write.xlsx(StatsFuel, file="output/StatsFuel.xlsx", row.names=FALSE, showNA = TRUE)
#
# tiff('output/Daioglou_Fig1.tiff', units="in", width=6, height=3.5, res=ppi, compression = 'lzw')
# print(plot(ILUC))
# dev.off()
# #
# tiff("output/Daioglou_Fig2.tiff", units="in", width=6, height=5, res=ppi, compression = 'lzw')
# print(plot(VAR2))
# dev.off()
# 
# png("output/Fig_all.png", width=6*ppi, height=3.5*ppi, res=ppi)
# print(plot(ILUC))
# dev.off()
# #
# png("output/Fig_all_year.png", width=5*ppi, height=5*ppi, res=ppi)
# print(plot(ILUC2))
# dev.off()
# #
# png("output/Fig_all_year2.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(ILUC3))
# dev.off()
# 
# png("output/Variation.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(VAR))
# dev.off()
# 
# png("output/Variation2.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(VAR2))
# dev.off()
# 
# png("output/Variation3.png", width=6*ppi, height=5*ppi, res=ppi)
# print(plot(VAR3))
# dev.off()
