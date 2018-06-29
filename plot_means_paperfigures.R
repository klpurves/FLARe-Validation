### 
###
###   plots for validation paper  - JAMA 5 figure limit 
###
###     KAHHHHH
###
###


# Set up data ######

# Clear workspace

rm(list=ls())


## Load dependencies ####

library(ggplot2)
library(psych)
library(dplyr)
library(foreign)
library(ggExtra)
library(grid)
library(gridExtra)
library(cowplot)
library(plotly)


### set up plotting palletes etc #########

dodge <- position_dodge(width = 0.85)

cbPalette <- c("#D55E00","#0072B2",  "#CC79A7","#FF9933", "#6699FF", "#009E73", "#F0E442")

PMpallette <- c("#002D64","#ACB9EA")                                                #### CS+ /  CS - colours (acq, ext, ren, affective)
genpallete <- c("#002D64","#27437D", "#445A97", "#5F72B2","#7A8CCE","#ACB9EA")      #### CS+, Gen, CS- colours (generalisation)


### SET UP TEXT SIZE HERE ########


x.title <- 12
y.title <- 24

x.text <-  24
y.text <- 24
axis.text <-  24

strip.title <-

legend.title <- 
legend.text <-
legend.size <-

caption <- 30
  
figure.w <- 20000
figure.l <- 25000
res <- 700
units = 'mm'


label.size <- 32



## Set up my paths and filenames for data access and saving KIRSTIN ####

path <- "/Volumes/groups/Eley\ Group/Projects/FLARe/FLARe Remote/Data_Management_Remote/Step4_Master_Datasets/Dataset/"
orderpath <-  "/Volumes/groups/Eley Group/Projects/FLARe/Data_Storage/FLARe_Task_VALIDATION/ZOHO/"
plotsave <- "/Users/kirstin/Dropbox/SGDP/FLARe/Papers/FC_validation/Figures/ForPaper"
data <- "/Volumes/groups/Eley\ Group/Projects/FLARe/Test-retest\ study/Data_Management_App/Step4_Master_Datasets/Dataset/4_full_master_dataset_apptrt_ec_0418.sav"
datl <- "/Volumes/groups/Eley Group/Projects/FLARe/Test-retest study/Data_Management_Lab/Step4_Master_Datasets/Dataset/4_full_master_dataset_LabTRT_ec_0418.sav"


## Set up my paths and filenames for data access and saving  SARAH ####

path <- "/Volumes/groups/Eley\ Group/Projects/FLARe/FLARe Remote/Data_Management_Remote/Step4_Master_Datasets/Dataset/"
orderpath <-  "/Volumes/groups/Eley Group/Projects/FLARe/Data_Storage/FLARe_Task_VALIDATION/ZOHO/"
data <- "/Volumes/groups/Eley\ Group/Projects/FLARe/Test-retest\ study/Data_Management_App/Step4_Master_Datasets/Dataset/4_full_master_dataset_apptrt_ec_0418.sav"
datl <- "/Volumes/groups/Eley Group/Projects/FLARe/Test-retest study/Data_Management_Lab/Step4_Master_Datasets/Dataset/4_full_master_dataset_LabTRT_ec_0418.sav"


plotsave <- "/Users/kirstin/Dropbox/SGDP/FLARe/Papers/FC_validation/Figures/ForPaper"



## Set the working directory to wherever I am saving my figures...
setwd(plotsave)


datorder <- "Remote_participant_zoho_info.csv"
fileorder <- paste(orderpath, datorder,sep="")

filen <- "master_dataset_remote_ec_1217.sav"
file1 <- paste(path, filen, sep="")

## Read in the stage zero datafile

datval <- read.spss(file1,use.value.labels = FALSE,trim_values = TRUE,
                    reencode = TRUE, use.missings = TRUE, to.data.frame = TRUE)

datorder<- read.csv(fileorder,header=T,sep=",")


## Read in the stage zero datafile

datapp <- read.spss(data,use.value.labels = FALSE,trim_values = TRUE,
                    reencode = TRUE, use.missings = TRUE, to.data.frame = TRUE)

datlab <- read.spss(datl,use.value.labels = FALSE,trim_values = TRUE,
                    reencode = TRUE, use.missings = TRUE, to.data.frame = TRUE)





## Obtain session order variable (from zoho data) ###


ord <- subset(datorder, select=c("ID.number",
                                 "Condition"))

ord[1] <- apply(ord[1],1,function(x) gsub("FID","",x))     
ord[1] <- apply(ord[1],1,function(x) gsub(" ","",x))

colnames(ord) <- c("Subject_ID","Condition.1")

ord <- ord[order(ord$Subject_ID),]


##  Now specify that lab THEN app is 1; app THEN lab is 2. Basically melt the two possible combos together for each.

ord$Condition <- ifelse((ord$Condition.1 ==  "1 (B)") | (ord$Condition.1 == "2 (A)"),1,2)

ord <- subset(ord, select = c("Subject_ID","Condition"))

# Make the subject ID numeric for ord so merge includes everyone

ord[1] <- apply(ord[1],1,function (x) as.numeric(x))


datval <- merge(ord,datval, by="Subject_ID")


#### clean up all the datasets #########

## VAL app #######
cols <- c("Subject_ID",
          "FCV1appCSmMea","FCV1appCSpMea",
          "FCV2appCSm","FCV2appCSp",
          "FCV2appGS1","FCV2appGS2","FCV2appGS3","FCV2appGS4",
          "FCV3appCSmMea","FCV3appCSpMea",
          "FCV4appCSmMea","FCV4appCSpMea",
          "FCQ0appAroCSm","FCQ0appAroCSp",
          "FCQ1appAroCSm","FCQ1appAroCSp",
          "FCQ2appAroCSm","FCQ2appAroCSp",
          "FCQ0appValCSm","FCQ0appValCSp",
          "FCQ1appValCSm","FCQ1appValCSp",
          "FCQ2appValCSm","FCQ2appValCSp",
          "FCQ0appFeaCSm","FCQ0appFeaCSp",
          "FCQ1appFeaCSm","FCQ1appFeaCSp",
          "FCQ2appFeaCSm","FCQ2appFeaCSp")

valapp <- subset(datval, select=cols)



## VAL lab #######
cols <- c("Subject_ID",
          "FCV1labCSmMea","FCV1labCSpMea",
          "FCV2labCSm","FCV2labCSp",
          "FCV2labGS1","FCV2labGS2","FCV2labGS3","FCV2labGS4",
          "FCV3labCSmMea","FCV3labCSpMea",
          "FCV4labCSmMea","FCV4labCSpMea",
          "FCQ0labAroCSm","FCQ0labAroCSp",
          "FCQ1labAroCSm","FCQ1labAroCSp",
          "FCQ2labAroCSm","FCQ2labAroCSp",
          "FCQ0labValCSm","FCQ0labValCSp",
          "FCQ1labValCSm","FCQ1labValCSp",
          "FCQ2labValCSm","FCQ2labValCSp",
          "FCQ0labFeaCSm","FCQ0labFeaCSp",
          "FCQ1labFeaCSm","FCQ1labFeaCSp",
          "FCQ2labFeaCSm","FCQ2labFeaCSp")

vallab <- subset(datval, select=cols)

### App TRT #######


## Select columns needed from the master ####

cols <- c("Subject_ID",
          "FCA1_1CSmMea","FCA1_1CSpMea",
          "FCA1_2CSm","FCA1_2CSp",
          "FCA1_2GS1","FCA1_2GS2","FCA1_2GS3","FCA1_2GS4",
          "FCA1_3CSmMea","FCA1_3CSpMea",
          "FCA1_4CSmMea","FCA1_4CSpMea",
          "FCQA1_0AroCSm","FCQA1_0AroCSp",
          "FCQA1_1AroCSm","FCQA1_1AroCSp",
          "FCQA1_2AroCSm","FCQA1_2AroCSp",
          "FCQA1_0ValCSm","FCQA1_0ValCSp",
          "FCQA1_1ValCSm","FCQA1_1ValCSp",
          "FCQA1_2ValCSm","FCQA1_2ValCSp",
          "FCQA1_0FeaCSm","FCQA1_0FeaCSp",
          "FCQA1_1FeaCSm","FCQA1_1FeaCSp",
          "FCQA1_2FeaCSm","FCQA1_2FeaCSp")
data <- subset(datapp, select=cols)


### Lab TRT ########

cols <- c("Subject_ID",
          "FCL1.1CSmMea","FCL1.1CSpMea",
          "FCL1.2CSm","FCL1.2CSp",
          "FCL1.2GS1","FCL1.2GS2","FCL1.2GS3","FCL1.2GS4",
          "FCL1.3CSmMea","FCL1.3CSpMea",
          "FCL1.4CSmMea","FCL1.4CSpMea",
          "FCLQ1.0AroCSm","FCLQ1.0AroCSp",
          "FCLQ1.1AroCSm","FCLQ1.1AroCSp",
          "FCLQ1.2AroCSm","FCLQ1.2AroCSp",
          "FCLQ1.0ValCSm","FCLQ1.0ValCSp",
          "FCLQ1.1ValCSm","FCLQ1.1ValCSp",
          "FCLQ1.2ValCSm","FCLQ1.2ValCSp",
          "FCLQ1.0FeaCSm","FCLQ1.0FeaCSp",
          "FCLQ1.1FeaCSm","FCLQ1.1FeaCSp",
          "FCLQ1.2FeaCSm","FCLQ1.2FeaCSp")


datl <- subset(datlab, select=cols)


### give them all common names for this bit (hash out if ndeeded later) ####

nameset <- c("Subject_ID",
             "FC1CSmMea","FC1CSpMea",
             "FC2CSm","FC2CSp",
             "FC2GS1","FC2GS2","FC2GS3","FC2GS4",
             "FC3CSmMea","FC3CSpMea",
             "FC4CSmMea","FC4CSpMea",
             "FCQ0AroCSm","FCQ0AroCSp",
             "FCQ1AroCSm","FCQ1AroCSp",
             "FCQ2AroCSm","FCQ2AroCSp",
             "FCQ0ValCSm","FCQ0ValCSp",
             "FCQ1ValCSm","FCQ1ValCSp",
             "FCQ2ValCSm","FCQ2ValCSp",
             "FCQ0FeaCSm","FCQ0FeaCSp",
             "FCQ1FeaCSm","FCQ1FeaCSp",
             "FCQ2FeaCSm","FCQ2FeaCSp")


names(data) <- nameset
names(datl) <- nameset
names(valapp) <- nameset
names(vallab) <- nameset

### create the mean affective score for all ####


valapp$FCQ0_NA_CSm <- (valapp$FCQ0FeaCSm+
                      valapp$FCQ0ValCSm+
                      valapp$FCQ0AroCSm)/3

valapp$FCQ0_NA_CSp <-(valapp$FCQ0FeaCSp+
                     valapp$FCQ0ValCSp+
                    valapp$FCQ0AroCSp)/3

valapp$FCQ1_NA_CSm <-(valapp$FCQ1FeaCSm+
                     valapp$FCQ1ValCSm+
                     valapp$FCQ1AroCSm)/3

valapp$FCQ1_NA_CSp <-(valapp$FCQ1FeaCSp+
                     valapp$FCQ1ValCSp+
                     valapp$FCQ1AroCSp)/3

valapp$FCQ2_NA_CSm <-(valapp$FCQ2FeaCSm+
                     valapp$FCQ2ValCSm+
                     valapp$FCQ2AroCSm)/3

valapp$FCQ2_NA_CSp <-(valapp$FCQ2FeaCSp+
                     valapp$FCQ2ValCSp+
                     valapp$FCQ2AroCSp)/3



vallab$FCQ0_NA_CSm <- (vallab$FCQ0FeaCSm+
                      vallab$FCQ0ValCSm+
                      vallab$FCQ0AroCSm)/3

vallab$FCQ0_NA_CSp <-(vallab$FCQ0FeaCSp+
                     vallab$FCQ0ValCSp+
                     vallab$FCQ0AroCSp)/3

vallab$FCQ1_NA_CSm <-(vallab$FCQ1FeaCSm+
                     vallab$FCQ1ValCSm+
                     vallab$FCQ1AroCSm)/3

vallab$FCQ1_NA_CSp <-(vallab$FCQ1FeaCSp+
                     vallab$FCQ1ValCSp+
                     vallab$FCQ1AroCSp)/3

vallab$FCQ2_NA_CSm <-(vallab$FCQ2FeaCSm+
                     vallab$FCQ2ValCSm+
                     vallab$FCQ2AroCSm)/3

vallab$FCQ2_NA_CSp <-(vallab$FCQ2FeaCSp+
                     vallab$FCQ2ValCSp+
                     vallab$FCQ2AroCSp)/3



data$FCQ0_NA_CSm <- (data$FCQ0FeaCSm+
                         data$FCQ0ValCSm+
                         data$FCQ0AroCSm)/3

data$FCQ0_NA_CSp <-(data$FCQ0FeaCSp+
                        data$FCQ0ValCSp+
                        data$FCQ0AroCSp)/3

data$FCQ1_NA_CSm <-(data$FCQ1FeaCSm+
                        data$FCQ1ValCSm+
                        data$FCQ1AroCSm)/3

data$FCQ1_NA_CSp <-(data$FCQ1FeaCSp+
                        data$FCQ1ValCSp+
                        data$FCQ1AroCSp)/3

data$FCQ2_NA_CSm <-(data$FCQ2FeaCSm+
                        data$FCQ2ValCSm+
                        data$FCQ2AroCSm)/3

data$FCQ2_NA_CSp <-(data$FCQ2FeaCSp+
                        data$FCQ2ValCSp+
                        data$FCQ2AroCSp)/3




datl$FCQ0_NA_CSm <- (datl$FCQ0FeaCSm+
                       datl$FCQ0ValCSm+
                       datl$FCQ0AroCSm)/3

datl$FCQ0_NA_CSp <-(datl$FCQ0FeaCSp+
                      datl$FCQ0ValCSp+
                      datl$FCQ0AroCSp)/3

datl$FCQ1_NA_CSm <-(datl$FCQ1FeaCSm+
                      datl$FCQ1ValCSm+
                      datl$FCQ1AroCSm)/3

datl$FCQ1_NA_CSp <-(datl$FCQ1FeaCSp+
                      datl$FCQ1ValCSp+
                      datl$FCQ1AroCSp)/3

datl$FCQ2_NA_CSm <-(datl$FCQ2FeaCSm+
                      datl$FCQ2ValCSm+
                      datl$FCQ2AroCSm)/3

datl$FCQ2_NA_CSp <-(datl$FCQ2FeaCSp+
                      datl$FCQ2ValCSp+
                      datl$FCQ2AroCSp)/3


# set up facet labels ########

labnames <- c(
  `Baseline` = "Baseline",
  `Post Extinction` = "Post extinction",
  `Post Renewal` = "Post renewal",
  `Acquisition` = "Acquisition",
  `Generalisation` = "Generalisation",
  `Extinction` = "Extinction",
  `Renewal` = "Renewal",
  `Trials` = "Trial number"
)

# FIGURE 1: Means by Trial ######


## Subset out the trial by trial variables #####


cols <- c("Subject_ID",
          "FCV1appCSm_1","FCV1appCSm_2",
          "FCV1appCSm_3","FCV1appCSm_4",
          "FCV1appCSm_5","FCV1appCSm_6",
          "FCV1appCSm_7","FCV1appCSm_8",
          "FCV1appCSm_9","FCV1appCSm_10",
          "FCV1appCSm_11","FCV1appCSm_12",
          "FCV1appCSp_1","FCV1appCSp_2",
          "FCV1appCSp_3","FCV1appCSp_4",
          "FCV1appCSp_5","FCV1appCSp_6",
          "FCV1appCSp_7","FCV1appCSp_8",
          "FCV1appCSp_9","FCV1appCSp_10",
          "FCV1appCSp_11","FCV1appCSp_12",
          "FCV3appCSm_1","FCV3appCSm_2",
          "FCV3appCSm_3","FCV3appCSm_4",
          "FCV3appCSm_5","FCV3appCSm_6",
          "FCV3appCSm_7","FCV3appCSm_8",
          "FCV3appCSm_9","FCV3appCSm_10",
          "FCV3appCSm_11","FCV3appCSm_12",
          "FCV3appCSm_13","FCV3appCSm_14",
          "FCV3appCSm_15","FCV3appCSm_16",
          "FCV3appCSm_17","FCV3appCSm_18",
          "FCV3appCSp_1","FCV3appCSp_2",
          "FCV3appCSp_3","FCV3appCSp_4",
          "FCV3appCSp_5","FCV3appCSp_6",
          "FCV3appCSp_7","FCV3appCSp_8",
          "FCV3appCSp_9","FCV3appCSp_10",
          "FCV3appCSp_11","FCV3appCSp_12",
          "FCV3appCSp_13","FCV3appCSp_14",
          "FCV3appCSp_15","FCV3appCSp_16",
          "FCV3appCSp_17","FCV3appCSp_18",
          "FCV4appCSp_1","FCV4appCSp_2",
          "FCV4appCSp_3","FCV4appCSp_4",
          "FCV4appCSm_1","FCV4appCSm_2",
          "FCV4appCSm_3","FCV4appCSm_4")

valeapp <- subset(datval, select=cols)


colsl <- c("Subject_ID",
           "FCV1lab_CSm_1","FCV1lab_CSm_2",
           "FCV1lab_CSm_3","FCV1lab_CSm_4",
           "FCV1lab_CSm_5","FCV1lab_CSm_6",
           "FCV1lab_CSm_7","FCV1lab_CSm_8",
           "FCV1lab_CSm_9","FCV1lab_CSm_10",
           "FCV1lab_CSm_11","FCV1lab_CSm_12",
           "FCV1lab_CSp_1","FCV1lab_CSp_2",
           "FCV1lab_CSp_3","FCV1lab_CSp_4",
           "FCV1lab_CSp_5","FCV1lab_CSp_6",
           "FCV1lab_CSp_7","FCV1lab_CSp_8",
           "FCV1lab_CSp_9","FCV1lab_CSp_10",
           "FCV1lab_CSp_11","FCV1lab_CSp_12",
           "FCV3lab_CSm_1","FCV3lab_CSm_2",
           "FCV3lab_CSm_3","FCV3lab_CSm_4",
           "FCV3lab_CSm_5","FCV3lab_CSm_6",
           "FCV3lab_CSm_7","FCV3lab_CSm_8",
           "FCV3lab_CSm_9","FCV3lab_CSm_10",
           "FCV3lab_CSm_11","FCV3lab_CSm_12",
           "FCV3lab_CSm_13","FCV3lab_CSm_14",
           "FCV3lab_CSm_15","FCV3lab_CSm_16",
           "FCV3lab_CSm_17","FCV3lab_CSm_18",
           "FCV3lab_CSp_1","FCV3lab_CSp_2",
           "FCV3lab_CSp_3","FCV3lab_CSp_4",
           "FCV3lab_CSp_5","FCV3lab_CSp_6",
           "FCV3lab_CSp_7","FCV3lab_CSp_8",
           "FCV3lab_CSp_9","FCV3lab_CSp_10",
           "FCV3lab_CSp_11","FCV3lab_CSp_12",
           "FCV3lab_CSp_13","FCV3lab_CSp_14",
           "FCV3lab_CSp_15","FCV3lab_CSp_16",
           "FCV3lab_CSp_17","FCV3lab_CSp_18",
           "FCV4lab_CSp_1","FCV4lab_CSp_2",
           "FCV4lab_CSp_3","FCV4lab_CSp_4",
           "FCV4lab_CSm_1","FCV4lab_CSm_2",
           "FCV4lab_CSm_3","FCV4lab_CSm_4")

valelab <- subset(datval, select=colsl)



## means 

exmeaa <- colMeans(valeapp[2:69], na.rm = T )
exmeal <- colMeans(valelab[2:69], na.rm = T )

## SD

exsda <- apply(na.omit(valeapp[2:69]),2,sd)
exsdl<- apply(na.omit(valelab[2:69]),2,sd)


datt <- data.frame(Stimulus = character(136),
                   Mean = numeric(136),
                   SD = numeric(136),
                   Mode = character(136))

datt$Variable <- rep(names(valeapp[2:69]))


datt$Phase <- apply(datt,1,function(x) ifelse(+(any(grep("FCV1", x))),"Acquisition",
                                              ifelse(+(any(grep("FCV4", x))),"Renewal", "Extinction")))

datt$Stimulus <- apply(datt,1,function(x) ifelse(+(any(grep("CSm", x))),"CS-","CS+"))


datt$Mean[1:68] = exmeaa
datt$Mean[69:136] = exmeal

datt$SD[1:68] = exsda
datt$SD[69:136] = exsdl


datt$Mode <- NaN

datt$Mode[1:68] <- c(rep("App"))
datt$Mode[69:136] <- (rep("Lab"))


## trial numbers 
datt$Trials <- NA

datt$Trials[1:12] <- seq(1,12)
datt$Trials[13:24] <- seq(1,12)
datt$Trials[25:42] <- seq(1,18)
datt$Trials[43:60] <- seq(1,18)
datt$Trials[61:64] <- seq(1,4)
datt$Trials[65:68] <- seq(1,4)
datt$Trials[69:80] <- seq(1,12)
datt$Trials[81:92] <- seq(1,12)
datt$Trials[93:110] <- seq(1,18)
datt$Trials[111:128] <- seq(1,18)
datt$Trials[129:132] <- seq(1,4)
datt$Trials[133:136] <- seq(1,4)


#computation of the standard error of the mean

datt$SEM <- datt$SD/sqrt(83)

#95% confidence intervals of the mean

datt$ci_low <- datt$Mean-(2*datt$SEM)
datt$ci_high <- datt$Mean+(2*datt$SEM)


## Add phase labels ####


### order factor levels ######

datt$Mode <- factor(datt$Mode, 
                    levels = c("Lab","App"))


datt$Phase <- factor(datt$Phase, 
                     levels = c("Acquisition","Extinction", "Renewal"))

datt$Stimulus <- factor(datt$Stimulus, 
                        levels = c("CS+","CS-"))


### seperate into lab and app for aesthetic purposes #####

datta <- datt[(datt$Mode == "App"),]
dattl <- datt[(datt$Mode == "Lab"),]

# Set up bar components

### PLOT 1 #######

  
Valextapp <-  ggplot(datta,
                     aes(Trials,Mean,
                         color=Stimulus))         +
  geom_point(position='dodge')                       +
  geom_line(size=1)                                        +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high,
                  fill=Stimulus),
              linetype=2, alpha=0.1)               +
  scale_fill_manual(values=c(PMpallette)) +
  facet_grid(. ~ Phase, scales = "free_x",
             space = "free_x",
             labeller = as_labeller(labnames))             +
  theme_bw()                                         +
  scale_color_manual(values=PMpallette)                +        
  labs(y="Mean expectancy rating\n",
      subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = axis.text),
        strip.text = element_text(face = "bold",
                                  size = strip.title),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title = element_text(face = "bold",
                                  size = 28),
        legend.text = element_text(face = "bold",
                                   size = 20),
        legend.title = element_text(size = 28),
        panel.spacing.x=unit(0, "lines"))              +
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))      +
  scale_x_continuous(breaks = round(seq(min(datta$Trials), max(datta$Trials), by = 1),1))+
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  


### PLOT 2 #######

Valextlab <-  ggplot(dattl,
                     aes(Trials,Mean,
                         color=Stimulus))         +
  geom_point(position='dodge')                       +
  geom_line()                                        +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high,
                  fill=Stimulus),
              linetype=2, alpha=0.1)               +
  scale_fill_manual(values=PMpallette) +
  
  facet_grid(. ~ Phase, scales = "free_x",
             space = "free_x",
             labeller = as_labeller(labnames))             +
  theme_bw()                                         +
  scale_color_manual(values=PMpallette)                +        
  labs(y="Mean expectancy rating\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 18),
        strip.text = element_text(face = "bold",
                                  size = 24),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title = element_text(face = "bold",
                                  size = 28),
        legend.text = element_text(face = "bold",
                                   size = 20),
        legend.title = element_text(size = 28),
        panel.spacing.x=unit(0, "lines"))              +
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))      +
  scale_x_continuous(breaks = round(seq(min(datta$Trials), max(datta$Trials), by = 1),1))+
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  


### COMBINE ACQ, EXT and RENPLOTS #######

exleg <- plot_grid(Valextlab,Valextapp, ncol=1)


## Create GENERALISATION for figure 1 ####

## Subset out the trial by trial variables #####


cols <- c("Subject_ID",
          "FCV2appCSm",
          "FCV2appCSp",
          "FCV2appGS1",
          "FCV2appGS2",
          "FCV2appGS3",
          "FCV2appGS4")

valeapp <- subset(datval, select=cols)


colsl <- c("Subject_ID",
           "FCV2labCSm",
           "FCV2labCSp",
           "FCV2labGS1",
           "FCV2labGS2",
           "FCV2labGS3",
           "FCV2labGS4")

valelab <- subset(datval, select=colsl)



## means 

exmeaa <- colMeans(valeapp[2:7], na.rm = T )
exmeal <- colMeans(valelab[2:7], na.rm = T )

## SD

exsda <- apply(na.omit(valeapp[2:7]),2,sd)
exsdl<- apply(na.omit(valelab[2:7]),2,sd)


datg <- data.frame(Stimulus = character(12),
                   Mean = numeric(12),
                   SD = numeric(12),
                   Mode = character(12))

datg$Variable <- rep(names(valeapp[2:7]))


datg$Phase <- "Generalisation"

datg$Stimulus <- apply(datg,1,function(x) ifelse(+(any(grep("CSm", x))),"CS-",
                                                 ifelse(+(any(grep("CSp", x))),"CS+",
                                                        ifelse(+(any(grep("GS1", x))),"GS1",
                                                               ifelse(+(any(grep("GS2", x))),"GS2",
                                                                      ifelse(+(any(grep("GS3", x))),"GS3","GS4"))))))


datg$Mean[1:6] = exmeaa
datg$Mean[7:12] = exmeal

datg$SD[1:6] = exsda
datg$SD[7:12] = exsdl


datg$Mode <- NaN

datg$Mode[1:6] <- c(rep("App"))
datg$Mode[7:12] <- (rep("Lab"))



#computation of the standard error of the mean

datg$SEM <- datg$SD/sqrt(83)

#95% confidence intervals of the mean

datg$ci_low <- datg$Mean-(2*datg$SEM)
datg$ci_high <- datg$Mean+(2*datg$SEM)


## Add phase labels ####


### order factor levels ######

datg$Mode <- factor(datg$Mode, 
                    levels = c("Lab","App"))


datg$Stimulus <- factor(datg$Stimulus, 
                        levels = c("CS+","GS1","GS2","GS3", "GS4","CS-"))


### seperate into lab and app for aesthetic purposes #####

datta <- datg[(datg$Mode == "App"),]
dattl <- datg[(datg$Mode == "Lab"),]

### LINE VERSION OF GENERALISATION  ##########



## Plot ####

ValGapp <-  ggplot(datta,
                   aes(Stimulus, Mean,
                       fill = Stimulus))  +
  geom_line(aes(group = 1,
                color = Stimulus),
               size=1) +
  geom_errorbar(aes(ymin = Mean - SEM, 
                    ymax = Mean + SEM,
                    color=Stimulus), 
                size=1,
                position = position_dodge(.9),
                width = .1)                          +
  geom_ribbon(aes(ymin = Mean - SEM, 
              ymax = Mean + SEM)) +
  scale_color_manual(values=genpallete)               +
  facet_grid(. ~ Phase,
             labeller = as_labeller(labnames))       +
  theme_bw()                                         +
  
  labs(y="Mean expectancy rating\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 36),
        axis.title.x = element_blank(),
        axis.text = element_text(face = "bold",
                                   size = 18),
        strip.text = element_text(face = "bold",
                                  size = 24),
        legend.position = "none",
        axis.title.y = element_text(face = "bold",
                                    size = 28),
        legend.text = element_text(face = "bold",
                                   size = 26),
        legend.title = element_text(face = "bold",
                                    size = 34))                  + 
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))                       +
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  




ValGlab <-  ggplot(dattl,
                   aes(Stimulus, Mean,
                       fill = Stimulus))  +
  geom_line(aes(group = 1,
                color = Stimulus),
            size=1) +
  geom_errorbar(aes(ymin = Mean - SEM, 
                    ymax = Mean + SEM,
                    color=Stimulus), 
                size=1,
                position = position_dodge(.9),
                width = .1)                          +
  geom_ribbon(aes(ymin = Mean - SEM, 
                  ymax = Mean + SEM)) +
  scale_color_manual(values=genpallete)               +
  facet_grid(. ~ Phase,
             labeller = as_labeller(labnames))       +
  theme_bw()                                         +
  
  labs(y="Mean expectancy rating\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold",
                                  size = 36),
        axis.title.x = element_blank(),
        axis.text.y = element_text(face = "bold",
                                   size = 18),
        axis.text.x = element_text(face="bold",
                                   size=18),
        strip.text = element_text(face = "bold",
                                  size = 24),
        legend.position = "none",
        axis.title.y = element_text(face = "bold",
                                    size = 28),
        legend.text = element_text(face = "bold",
                                   size = 26),
        legend.title = element_text(face = "bold",
                                    size = 34))                  + 
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))                       +
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  





## Stack trial by trial GEN ######


### combine them with cow_plot

exG <- plot_grid(ValGlab,ValGapp, ncol=1)



## Create AFFECTIVE ratings for figure 1 ####

## Subset out the trial by trial variables #####


cols <- c("Subject_ID",
          "FCQ0_NA_CSm",
          "FCQ0_NA_CSp",
          "FCQ1_NA_CSm",
          "FCQ1_NA_CSp",
          "FCQ2_NA_CSm",
          "FCQ2_NA_CSp")

valeapp <- subset(valapp, select=cols)


colsl <- c("Subject_ID",
           "FCQ0_NA_CSm",
           "FCQ0_NA_CSp",
           "FCQ1_NA_CSm",
           "FCQ1_NA_CSp",
           "FCQ2_NA_CSm",
           "FCQ2_NA_CSp")

valelab <- subset(vallab, select=colsl)



## means 

exmeaa <- colMeans(valeapp[2:7], na.rm = T )
exmeal <- colMeans(valelab[2:7], na.rm = T )

## SD

exsda <- apply(na.omit(valeapp[2:7]),2,sd)
exsdl<- apply(na.omit(valelab[2:7]),2,sd)


datt <- data.frame(Stimulus = character(12),
                   Mean = numeric(12),
                   SD = numeric(12),
                   Mode = character(12))

datt$Variable <- rep(names(valeapp[2:7]))


datt$Phase <- apply(datt,1,function(x) ifelse(+(any(grep("FCQ0", x))),"Baseline",
                                              ifelse(+(any(grep("FCQ1", x))),"Post\nExtinction", "Post\nRenewal")))


datt$Stimulus <- apply(datt,1,function(x) ifelse(+(any(grep("CSm", x))),"CS-","CS+"))

datt$Mean[1:6] = exmeaa
datt$Mean[7:12] = exmeal

datt$SD[1:6] = exsda
datt$SD[7:12] = exsdl


datt$Mode <- NaN

datt$Mode[1:6] <- c(rep("App"))
datt$Mode[7:12] <- (rep("Lab"))



#computation of the standard error of the mean

datt$SEM <- datt$SD/sqrt(83)

#95% confidence intervals of the mean

datt$ci_low <- datt$Mean-(2*datt$SEM)
datt$ci_high <- datt$Mean+(2*datt$SEM)


## Add phase labels ####


### order factor levels ######

datt$Mode <- factor(datt$Mode, 
                    levels = c("Lab","App"))


datt$Stimulus <- factor(datt$Stimulus, 
                        levels = c("CS+","CS-"))


## Add column for facet to keep fpormatting consistent

datt$Aff <- "Affective ratings"

### seperate into lab and app for aesthetic purposes #####

datta <- datt[(datt$Mode == "App"),]
dattl <- datt[(datt$Mode == "Lab"),]

## Plot - line version  ####

ValAfapp <-  ggplot(datta,
                     aes(Phase,Mean,
                         color=Stimulus))         +
  geom_point(position='dodge')                       +
  geom_line(aes(group = Stimulus))                +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high,
                  fill=Stimulus,
                  group =Stimulus),
              linetype=2, alpha=0.1)               +
  
  scale_fill_manual(values=PMpallette) +
  facet_grid(. ~ Aff, scales = "free_x")             +
  theme_bw()                                         +
  scale_color_manual(values=PMpallette)                +        
  labs(y="Mean expectancy rating\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 18),
        strip.text = element_text(face = "bold",
                                  size = 24),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                  size = 28),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 20),
        legend.title = element_text(size = 28),
        panel.spacing.x=unit(0, "lines"))              +
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))      +
  # scale_x_continuous(breaks = c(seq(1,3))) +
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  



ValAflab <-  ggplot(dattl,
                    aes(Phase,Mean,
                        color=Stimulus))         +
  geom_point(position='dodge')                       +
  geom_line(aes(group = Stimulus))                +
  geom_ribbon(aes(ymin=ci_low, ymax=ci_high,
                  fill=Stimulus,
                  group =Stimulus),
              linetype=2, alpha=0.1)               +
  
  scale_fill_manual(values=c(PMpallette)) +
  facet_grid(. ~ Aff, scales = "free_x")             +
  theme_bw()                                         +
  scale_color_manual(values=PMpallette)                +        
  labs(y="Mean expectancy rating\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 18),
        strip.text = element_text(face = "bold",
                                  size = 24),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                     size = 28),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 20),
        legend.title = element_text(size = 28),
        panel.spacing.x=unit(0, "lines"))              +
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))      +
  # scale_x_continuous(breaks = c(seq(1,3))) +
  theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))  


## Stack trial by trial AFF ######


### combine them with cow_plot

exAf <- plot_grid(ValAflab,ValAfapp, ncol=1)


## COMBINE ALL FIGURE 1 PANELS #####

## rows then columns with own titles ####

### titles and subtitles and caption ######


Maintitle = "Figure 1. Validation study mean ratings for all stimuli and study phases"
labsub = "Lab sessions"
appsub = "App sessions"

l1 <- expression(paste(bold("Figure 1.")," Plots visualising mean ratings per stimuli across all experimental phases for all participants in the validation data, where participants took part in lab and App testing one week apart. ",bold("Panel A")," presents plots showing"))
l2 <- expression(paste("the average participant expectancy rating for each stimulus for each trial during acquisition, extinction and renewal testing phases for lab", italic(" (top)"), "and App", italic(" (bottom)"), " sessions respectively. Points represent mean and shading",sep=" "))
l3 <- expression(paste("represents standard error of the mean. ",bold("Panel B")," presents bar plots showing the average participant expectancy rating for each stimulus during the generalisation phase for lab", italic(" (top)"), "and App", italic(" (bottom)"), " sessions respectively.",sep=" "))
l4 <- expression(paste("Error bars represent standard error of the mean. ",bold("Panel C")," presents plots showing the average participant affective rating for each stimulus before beginning the experiment",italic(" (Baseline)"), ",after the extinction phase" , sep=" "))
l5 <- expression(paste("",italic("(Post extinction)"),", and after the renewal phase",italic(" (Post renewal)"),".Points represent mean and shading represents standard error of the mean.",sep=" "))


## Create the 2 rows #######

row.1 <- plot_grid(Valextlab,ValGlab,ValAflab,ncol =3,
                   labels = "AUTO", label_size = label.size,
                   rel_widths = c(1.2,0.5,0.65))
row.1 <- plot_grid(NULL, row.1,ncol =1,rel_heights = c(0.1,1))

row.2 <- plot_grid(Valextapp,ValGapp,ValAfapp,ncol=3,rel_widths = c(1.2,0.5,0.65))
row.2 <- plot_grid(NULL, row.2,ncol =1,rel_heights = c(0.01,1))


## Add title and subtitle to rows #######

row.1.1 <- row.1 +  draw_label(labsub, 
                               fontface='bold',
                               fontfamily = "Trebuchet MS",
                               colour = "black", 
                               size = 34,
                               x = 0.09, 
                               y = 1,
                               vjust =1, 
                               hjust =1) 
  
row.1.3 <- plot_grid(NULL, row.1.1,ncol =1,rel_heights = c(0.1,1))
#   
# row.1.1 <-   row.1.3 + draw_label(Maintitle,
#                                       fontface='bold',
#                                       fontfamily = "Trebuchet MS",
#                                       colour = "black",
#                                       size = 40,
#                               x = 0.48, 
#                               y = 1,
#                               vjust = 1, 
#                               hjust = 1)  

row.1.1 <- plot_grid(NULL, row.1.1,ncol =1,rel_heights = c(0.01,1))

  
row.2.1 <- row.2 +  draw_label(appsub, 
                               fontface='bold',
                               fontfamily = "Trebuchet MS",
                               colour = "black", 
                               size = 34,
                               x = 0.09, 
                               y = 1,
                               vjust =1, 
                               hjust =1) 
  

## and final figure (without caption) #########

fig1 <- plot_grid(row.1.1,NULL, row.2.1, NULL, ncol=1, rel_heights = c(1,0.05,.9,0.05))

## expand plot margins slightly

fig1.1 <- fig1 + theme(plot.margin = unit(c(1, 0.6, 0.6, 0.6), "cm"))

## add caption ######

pl1 = add_sub(fig1.1,l1, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 28,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))


pl2 = add_sub(pl1,l2, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 28,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl3 = add_sub(pl2,l3, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 28,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))

pl4 = add_sub(pl3,l4, 
                fontface='italic',
                fontfamily = "Trebuchet MS",
                colour = "black", 
                size = 28,
                hjust=0,
                vjust=1,
                x=0.01,
                y=1,
              vpadding=grid::unit(0, "lines"))

fig1.cap = add_sub(pl4,l5, 
                   fontface='italic',
                   fontfamily = "Trebuchet MS",
                   colour = "black", 
                   size = 28,
                   hjust=0,
                   vjust=1,
                   x=0.01,
                   y=1)


### SAVE FIGURE 1 #######

png("Fig1.Validation_trial_means.png",width=figure.l,height=figure.w,unit=units,res=res)
ggdraw(fig1.cap)
dev.off()



## FIGURE 2: T-TEST #####



#### create data subsets #####


acqext <- c("Subject_ID",
            "FC1CSmMea","FC1CSpMea",
            "FC3CSmMea","FC3CSpMea")

acqextren <- c("Subject_ID",
               "FC1CSmMea","FC1CSpMea",
               "FC2CSm","FC2CSp",
               "FC2GS1","FC2GS2","FC2GS3","FC2GS4",
               "FC3CSmMea","FC3CSpMea",
               "FC4CSmMea","FC4CSpMea")

allc <- c("Subject_ID",
               "FC1CSmMea","FC1CSpMea",
               "FC2CSm","FC2CSp",
               "FC2GS1","FC2GS2","FC2GS3","FC2GS4",
               "FC3CSmMea","FC3CSpMea",
               "FC4CSmMea","FC4CSpMea",
         "FCQ0_NA_CSm","FCQ0_NA_CSp",
         "FCQ1_NA_CSm","FCQ1_NA_CSp",
         "FCQ2_NA_CSm","FCQ2_NA_CSp")

gen <- c("Subject_ID",
         "FC2CSm","FC2CSp",
         "FC2GS1","FC2GS2","FC2GS3","FC2GS4")

aff <- c("Subject_ID",
         "FCQ0_NA_CSm","FCQ0_NA_CSp",
         "FCQ1_NA_CSm","FCQ1_NA_CSp",
         "FCQ2_NA_CSm","FCQ2_NA_CSp")
  
## acq + ext ####
vala13 <- subset(valapp,select=acqext)
vall13 <- subset(vallab,select=acqext)
da13 <- subset(data,select=acqext)
dl13 <- subset(datl,select=acqext)


## acq + ext + gen + ren  ####
vala134 <- subset(valapp,select=acqextren)
vall134 <- subset(vallab,select=acqextren)
da134 <- subset(data,select=acqextren)
dl134 <- subset(datl,select=acqextren)



## acq + ext + gen + ren + affective ####
valall <- subset(valapp,select=allc)
vallall <- subset(vallab,select=allc)


## Gen ####
vala2 <- subset(valapp,select=gen)
vall2 <- subset(vallab,select=gen)
da2 <- subset(data,select=gen)
dl2 <- subset(datl,select=gen)


## AFFECTIVE tTEST ####

valaA <- subset(valapp,select=aff)
vallA <- subset(vallab,select=aff)
daA <- subset(data,select=aff)
dlA <- subset(datl,select=aff)

## make a combined validation set for affective ######

val <- rbind(valaA,vallA)

## means

afmeaa <- colMeans(valaA[2:7], na.rm = T )
afmeal<- colMeans(vallA[2:7], na.rm = T )

## SD

afsda <- apply(na.omit(valaA[2:7]),2,sd)
afsdl<- apply(na.omit(vallA[2:7]),2,sd)


dat <- data.frame(Stimulus = character(12),
                  Mean = numeric(12),
                  SD = numeric(12),
                  Mode = character(12))

dat$Stimulus <- rep(names(valaA[2:7]))

dat$Mean[1:6] = afmeaa
dat$Mean[7:12] = afmeal

dat$SD[1:6] = afsda
dat$SD[7:12] = afsdl


dat$Mode <- c("App","App","App","App","App","App",
              "Lab","Lab","Lab","Lab","Lab","Lab")

#computation of the standard error of the mean

dat$SEM <- dat$SD/sqrt(83)

#95% confidence intervals of the mean

dat$ci_low <- dat$Mean-(2*dat$SEM)
dat$ci_high <- dat$Mean+(2*dat$SEM)

## Add phase labels ####


dat$Phase <- apply(dat,1,function(x) ifelse(+(any(grep("FCQ0", x))),"Baseline",
                                            ifelse(+(any(grep("FCQ1", x))),"Post Extinction","Post Renewal")))

dat$Stimulus <- apply(dat,1,function(x) ifelse(+(any(grep("CSm", x))),"CS-","CS+"))


dat$Mode <- factor(dat$Mode, 
                      levels = c("Lab","App"))

dat$Stimulus <- factor(dat$Stimulus, 
                   levels = c("CS+","CS-"))

## mean bar plot - affective validation #####


dodge <- position_dodge(width = 0.85)


ValAFtest <-  ggplot(dat,
                 aes(Mode, Mean,
                     fill = Stimulus))  +
  geom_bar(position = "dodge",
           width = 0.9,
           stat="identity") +
   geom_errorbar(aes(ymin = Mean - SEM, 
                    ymax = Mean + SEM), 
                position = dodge,
                width = .1)                          +
  scale_fill_manual(values=PMpallette)                +
  facet_wrap( ~ Phase,
             labeller = as_labeller(labnames))       +
  theme_bw()                                         +
  labs(y="Mean affective rating",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 22),
        strip.text = element_text(face = "bold",
                                  size = 28),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                     size = 26),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 24),
        legend.title = element_text(face = "bold",
                                    size = 26),
        panel.spacing.x=unit(0, "lines"))                         + 
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))                       +
  theme(plot.margin = unit(c(0.3, 0.0, 0.3, 0.3), "cm"))



## create annotations #####

## Segments ####

ann_line<-data.frame(Phase = c("Post Extinction","Post Extinction","Post Extinction","Post Extinction","Post Extinction","Post Extinction",
                               "Post Renewal","Post Renewal","Post Renewal","Post Renewal","Post Renewal","Post Renewal"),
                     Stimulus = c("CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-"),
                     x=c(.8,.8,1.2,1.78,1.78,2.2,.8,.8,1.2,1.78,1.78,2.2),
                     xend=c(.8,1.2,1.2,1.78,2.2,2.2,.8,1.2,1.2,1.78,2.2,2.2),
                     y=c(6.5,7,7,6.5,7,7,6,6.5,6.5,6,6.5,6.5),
                     yend=c(7,7,6.5,7,7,6.5,6.5,6.5,6,6.5,6.5,6))
                     
  
  
ann_text <- data.frame(
  label = c("***","***","***","***"),
  Phase   = c("Post Extinction","Post Extinction","Post Renewal","Post Renewal"),
  Stimulus = c("CS+","CS-","CS+","CS-"),
  x     = c(1,2,1,2),
  y     = c(7,7,6.5,6.5)
)


## Add to plot
 
p <- ValAFtest + geom_segment(data=ann_line,aes(x=x,xend=xend,y=y,yend=yend)) 
afan <- p + geom_text(data=ann_text,aes(x=x,y=y,label=label),size=11)





## EXPECTANCY tTEST #####



## acq + ext + ren ####

vala134 <- subset(valapp,select=acqextren)
vall134 <- subset(vallab,select=acqextren)



## means

exmeaa <- colMeans(vala134[2:13], na.rm = T )
exmeal <- colMeans(vall134[2:13], na.rm = T )

## SD

exsda <- apply(na.omit(vala134[2:13]),2,sd)
exsdl<- apply(na.omit(vall134[2:13]),2,sd)


date <- data.frame(Stimulus = character(24),
                  Mean = numeric(24),
                  SD = numeric(24))

date$Stimulus <- rep(names(vala134[2:13]))

date$Mean[1:12] = exmeaa
date$Mean[13:24] = exmeal

date$SD[1:12] = exsda
date$SD[13:24] = exsdl


date$Mode <- NaN
date$Mode[1:12] <- rep("App",12)
date$Mode[13:24] <- rep("Lab",12)


#computation of the standard error of the mean

date$SEM <- date$SD/sqrt(83)

#95% confidence intervals of the mean

date$ci_low <- date$Mean-(2*date$SEM)
date$ci_high <- date$Mean+(2*date$SEM)

## Add phase labels ####


date$Phase <- apply(date,1,function(x) ifelse(+(any(grep("FC1", x))),"Acquisition",
                                              ifelse(+(any(grep("FC2", x))),"Generalisation",
                                                     ifelse(+(any(grep("FC3", x))),"Extinction", "Renewal"))))

date$Stimulus <- apply(date,1,function(x) ifelse(+(any(grep("CSm", x))), "CS-",
                                                 ifelse(+(any(grep("CSp", x))),"CS+",
                                                        ifelse(+(any(grep("GS1", x))),"GS1",
                                                               ifelse(+(any(grep("GS2", x))),"GS2",
                                                                      ifelse(+(any(grep("GS3", x))),"GS3","GS4"))))))

### order factor levels ######

date$Mode <- factor(date$Mode, 
                   levels = c("Lab","App"))

date$Phase <- factor(date$Phase,
                     levels = c("Acquisition","Generalisation","Extinction","Renewal"))


date$Stimulus <- factor(date$Stimulus, 
                       levels = c("CS+","GS1","GS2","GS3","GS4","CS-"))

##########################################################
##
## Make the grouped bar plot
##
#########################################################

# Set up bar components


dodge <- position_dodge(width = 0.85)

Valexttest <-  ggplot(date,
                     aes(Mode, Mean,
                         fill = Stimulus))  +
  geom_bar(position = "dodge",
           width = 0.9,
           stat="identity") +
  geom_errorbar(aes(ymin =  Mean - SEM,
                    ymax = Mean + SEM),
                position = position_dodge(width=.9),
                width = .1)                          +
  facet_wrap( ~ Phase,
             nrow =2,
             labeller = as_labeller(labnames))                              +
  theme_bw()                                         +
  scale_fill_manual(values=genpallete)                +        
  labs(y="Mean expectancy rating",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 22),
        strip.text = element_text(face = "bold",
                                  size = 28),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                     size = 26),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 24),
        legend.title = element_text(face = "bold",
                                    size = 26),
        panel.spacing.x=unit(0, "lines"))     + 
  scale_y_continuous(limits = c(0,9),
                     breaks = c(seq(0,9)))    



     
## Segments ####

ann_line<-data.frame(Phase = c("Acquisition","Acquisition","Acquisition","Acquisition","Acquisition","Acquisition",
                               "Extinction","Extinction","Extinction","Extinction","Extinction","Extinction",
                               "Renewal","Renewal","Renewal","Renewal","Renewal","Renewal",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation",
                               "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation"),
                     Stimulus = c("CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-",
                                  "CS+","CS-","CS+","CS-","CS+","CS-"),
                     x=c(.8,.8,1.2,1.78,1.78,2.2,.8,.8,1.2,1.78,1.78,2.2,.8,.8,1.2,1.78,1.78,2.2,
                         .62,.62,.77, #l1 L
                         .77,.77,.92, 
                         .92,.92,1.07, 
                         1.07,1.07,1.22, 
                         1.22,1.22,1.37, 
                         1.62,1.62,1.77, #l1 R
                         1.77,1.77,1.92, 
                         1.92,1.92,2.07,
                         2.07,2.07,2.22, 
                         2.22,2.22,2.37, 
                         .77,.77,.92, # 2  L
                         .92,.92,1.07, 
                         1.07,1.07,1.22, 
                         1.22,1.22,1.37,  
                         1.77,1.77,1.92, # 2  R
                         1.92,1.92,2.07, 
                         2.07,2.07,2.22, 
                         2.22,2.22,2.37, 
                         .92,.92,1.22, # 3  L
                         1.22,1.22,1.37,  
                         1.92,1.92,2.22, # 3  R
                         2.22,2.22,2.37,
                         1.07,1.07,1.37,  # 4 L
                         2.07,2.07,2.37), # 4 R
                     xend=c(.8,1.2,1.2,1.78,2.2,2.2,.8,1.2,1.2,1.78,2.2,2.2,.8,1.2,1.2,1.78,2.2,2.2,
                            .62,.77,.77,
                            .77,.92,.92,
                            .92,1.07,1.07,
                            1.07,1.22,1.22,
                            1.22,1.37,1.37,
                            1.62,1.77,1.77,
                            1.77,1.92,1.92,
                            1.92,2.07,2.07,
                            2.07,2.22,2.22,
                            2.22,2.37,2.37,
                            .77,.92,.92,  # row 2
                            .92,1.07,1.07,
                            1.07,1.22,1.22,
                            1.22,1.37,1.37,
                            1.77,1.92,1.92,  # row 2 R
                            1.92,2.07,2.07,
                            2.07,2.22,2.22,
                            2.22,2.37,2.37,
                            .92,1.22,1.22,    # row 3 L
                            1.22,1.37,1.37,
                            1.92,2.22,2.22,    # row 3 R
                            2.22,2.37,2.37,
                            1.07,1.37,1.37, # row 4 L
                            2.07,2.37,2.37),
                     y=c(8,8.5,8.5,8,8.5,8.5,3.5,4,4,3.5,4,4,5.2,5.7,5.7,5.2,5.7,5.7,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     8.5,9,9,
                     6.75,7.25,7.25, ## row 2
                     6.75,7.25,7.25,
                     6.75,7.25,7.25,
                     6.75,7.25,7.25,
                     6.75,7.25,7.25,
                     6.75,7.25,7.25,
                     6.75,7.25,7.25,
                     6.75,7.25,7.25, ## row 3
                     5,5.5,5.5,
                     5,5.5,5.5,
                     5,5.5,5.5,
                     5,5.5,5.5,
                     3.25,3.75,3.75,  ## row 4
                     3.25,3.75,3.75),
                     yend=c(8.5,8.5,8,8.5,8.5,8,4,4,3.5,4,4,3.5,5.7,5.7,5.2,5.7,5.7,5.2,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            9,9,8.5,
                            7.25,7.25,6.75,  ## row 2
                            7.25,7.25,6.75,
                            7.25,7.25,6.75,
                            7.25,7.25,6.75,
                            7.25,7.25,6.75,  
                            7.25,7.25,6.75,
                            7.25,7.25,6.75,
                            7.25,7.25,6.75,   # row 3
                            5.5,5.5,5,
                            5.5,5.5,5,
                            5.5,5.5,5,
                            5.5,5.5,5,
                            3.75,3.75,3.25, # row 4
                            3.75,3.75,3.25))



ann_text <- data.frame(
  label = c("***","***","***","***","***","***",
            "***","***","***","***","***","***","***","***"),
  Phase   = c("Extinction","Extinction","Acquisition","Acquisition","Renewal","Renewal",
              "Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation","Generalisation"),
  Stimulus = c("CS+","CS-","CS+","CS-","CS+","CS-",
               "CS+","CS-","CS+","CS-","CS+","CS-","CS+","CS-"),
  x     = c(1,2,1,2,1,2,
            1,2,1.07,2.07,1.14,2.14,1.21,2.21),
  y     = c(4,4,8.5,8.5,5.7,5.7,
            9,9,7.25,7.25,5.5,5.5,3.75,3.75)
)


## Add to plot

e <- Valexttest + geom_segment(data=ann_line,aes(x=x,xend=xend,y=y,yend=yend)) 
exan <- e + geom_text(data=ann_text,aes(x=x,y=y,label=label),size=11)

### combine expectancy and affective ######

leg <- get_legend(exan)

#exan <- exan + theme(legend.position="none")
#afan <- afan + theme(legend.position="none")

exan <- exan + theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))
afan <- afan + theme(plot.margin = unit(c(0.6, 0.6, 0.6, 0.6), "cm"))


alltogethernow <- plot_grid(exan,afan,nrow = 2,rel_heights = c(3,2),
                            labels="AUTO",
                            label_size = 32)

wlegend <- plot_grid(alltogethernow,leg,
                     rel_widths = c(1.7,.4),
                     rel_heights = c(1.7,.4))

## Add captions and titles #####


## rows then columns with own titles ####

### titles and subtitles and caption ######


Maintitle = "Figure 2. Validation study app versus lab mean comparisons"

l1 <- expression(paste(bold("Figure 2.")," Plots visualising mean ratings per stimuli per experimental phase for all participants in the validation data, where participants"))
l2 <- expression(paste("took part in lab and App testing one week apart. Error bars represent the standard error of the mean. Significant differences following",sep=" "))
l3 <- expression(paste("paired sampled t-tests are indicated with a '***' label. ",bold("Panel A"), " presents bar plots showing the average participant expectancy rating for each",sep=" "))
l4 <- expression(paste("stimulus during acquisition, extinction and renewal testing phases for lab", italic(" (left)"), "and App", italic(" (right)"), " sessions respectively.", bold("Panel B")," presents bar", sep=" "))
l5 <- expression(paste("plots showing the average participant affective rating for each stimulus before beginning the experiment",italic(" (Baseline),")," after the extinction",sep=" "))
l6 <- expression(paste( "phase ",italic("(Post extinction)"),", and after the renewal phase",italic(" (Post renewal)",italic(" (Baseline),"),"for lab", italic(" (left)"), "and App", italic(" (right)"), " sessions respectively.",sep=" ")))


## add title #####
# 
# alltit <-   alltogethernow + theme(plot.margin = unit(c(1, 0.6, 0.6, 0.6), "cm")) +
#                                  draw_label(Maintitle,
#                                   fontface='bold',
#                                   fontfamily = "Trebuchet MS",
#                                   colour = "black",
#                                   size = 32,
#                                   x = 1.01, 
#                                   y = 1.03,
#                                   vjust = 1, 
#                                   hjust = 1)  



alltit <- alltogethernow + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
## add caption ####


## add caption ######

pl1 = add_sub(alltit,l1, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))


pl2 = add_sub(pl1,l2, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl3 = add_sub(pl2,l3, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))

pl4 = add_sub(pl3,l4, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl5 = add_sub(pl4,l5, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))

pl6 = add_sub(pl5,l6, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))



## Save it ##### 
# 
png("Fig2.Validation_combined_ttest.png",width=22000,height=26000,res=1200)
ggdraw(pl6)
dev.off()


## FIGURE 3: ANXIETY ######

## read in anxiety correlation table ####

anx <- read.csv("/Volumes/groups/Eley Group/Kirstin/FLARe_Analyses/Validation/AnxCorrelations/Datasets/Anxiety_corr_bootstrappedCI_table.csv",
                header = T)


## set up factor orders ###

anx$Stimulus <- factor(anx$Stimulus, levels = c("CS+","GS1","GS2","GS3","GS4","CS-"))
anx$Modality <- factor(anx$Modality, levels = c("Lab","App"))
anx$Phase <- factor(anx$Phase, levels = c("Acquisition","Generalisation","Extinction","Renewal","Post Extinction","Post Renewal"))


# Where absolute r will be needed has been create flip the cis and make absolute 

anx$citemp <- NaN
anx$citemp <- ifelse(anx$r < 0,anx$ci_low,NaN)
anx$ci_low <- ifelse(anx$r < 0,anx$ci_high,anx$ci_low)
anx$ci_high <- ifelse(anx$r < 0,anx$citemp,anx$ci_high)
anx$ci_high <- abs(anx$ci_high)

## fix CI for neg values - set bottom to low

anx$ci_low <- ifelse(anx$ci_low < 0, 0, anx$ci_low)

# ensure negative values are set to zero for confidence intervals and correlations aer absolute. Add column to flag if r was negative 

anx$negr <- ifelse(anx$r < 0,1,NaN)

## Now make r absolute

anx$r <- abs(anx$r)

anx[1,5] <- 0
anx[12,5] <- 0


## make a column to indicate significance and a symbol column for neg values

anx$psym <- ifelse(anx$p <= 0.001,"***",
                   ifelse(anx$p <= 0.01,"**",
                          ifelse(anx$p <= 0.055,"*","")))

anx$negsym <- ifelse(!is.na(anx$negr),"-","")

## make a column to indicate x and y positions

anx$y <- rep(0.7,32)
anx$x <- seq(0.7,0.4)
  

## seperate expectancy and affective

anxex <- anx[((anx$Phase == "Acquisition") | 
                (anx$Phase == "Generalisation") | 
                (anx$Phase == "Extinction") |
                (anx$Phase == "Renewal")),]

anxaf <- anx[((anx$Phase == "Post Extinction") | 
                (anx$Phase == "Post Renewal")),]


## Expectancy ######

anxexp <-  ggplot(anxex,
               aes(Modality, r,
                          fill = Stimulus))  +
  geom_bar(position = "dodge",
           width = 0.9,
           stat="identity") +
  geom_errorbar(aes(ymin =  ci_low,
                    ymax = ci_high),
                position = position_dodge(width=.9),
                width = .1)                          +
  facet_wrap( ~ Phase,
              nrow =2,
              labeller = as_labeller(labnames))                              +
  theme_bw()                                         +
  scale_fill_manual(values=genpallete)                +        
  labs(y="Pearson's r",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 22),
        strip.text = element_text(face = "bold",
                                  size = 28),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                     size = 26),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 24),
        legend.title = element_text(face = "bold",
                                    size = 26),
        panel.spacing.x=unit(0, "lines"))     + 
  scale_y_continuous(limits = c(0,0.8),
                     labels = scales::comma)




### affective #####

anxafp <-  ggplot(anxaf,
                  aes(Modality, r,
                      fill = Stimulus))  +
  geom_bar(position = "dodge",
           width = 0.9,
           stat="identity") +
  geom_errorbar(aes(ymin =  ci_low,
                    ymax = ci_high),
                position = position_dodge(width=.9),
                width = .1)                          +
  facet_wrap( ~ Phase,
              nrow =1,
              labeller = as_labeller(labnames))                              +
  theme_bw()                                         +
  scale_fill_manual(values=PMpallette)                +        
  labs(y="Pearson's r",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 28),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 22),
        strip.text = element_text(face = "bold",
                                  size = 28),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.y =  element_text(face = "bold",
                                     size = 26),
        axis.title.x =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 24),
        legend.title = element_text(face = "bold",
                                    size = 26),
        panel.spacing.x=unit(0, "lines"))     + 
  scale_y_continuous(limits = c(0,0.8),
                     labels = scales::comma)



## af labels daatset

anxtext <- subset(anxaf, select = c("Modality","Stimulus","Phase","psym","p", "r", "y","x","negsym"))

anxtext$x <- 0.78
anxtext$x[2] <- 1.23
anxtext$y[2] <- 0.45


anxaf1 <- anxafp + geom_text(data=anxtext,aes(Modality,label=psym,y=y,x=x),size=11,na.rm=T)


## Expectancy labels daatset

anxtexte <- subset(anxex, select = c("Modality","Stimulus","Phase","psym","p", "r", "y","x","negsym"))

anxtexte$x <- 1.78

anxtexte$y[21] <- 0.55
anxtexte$x[22] <- 2.23
anxtexte$y[22] <- 0.76
anxtexte$x[19] <- 2.08
anxtexte$y[19] <- 0.57
anxtexte$x[14] <- 2.23
anxtexte$y[14] <- 0.69
anxtexte$y[24] <- 0.69
anxtexte$x[24] <- 2.23



anxex1 <- anxexp + geom_text(data=anxtexte,aes(Modality,label=psym,x=x,y=y),size=11,na.rm=T)

## indicate negative correlations



anxtexte$ny <- 0.4
anxtexte$ny[1] <- 0.28 
anxtexte$ny[6] <- 0.44 
anxtexte$ny[12] <- 0.38 



anxex2 <- anxex1 + geom_text(data=anxtexte,aes(Modality,label=negsym,y=ny),
                             position = position_dodge(.9),size=8,na.rm=T)


## combine plots


exanx <- anxex2 + theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
afanx <- anxaf1 + theme(plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))


alltog <- plot_grid(exanx,afanx,nrow = 2,rel_heights = c(5,3),
                            labels="AUTO",
                            label_size = 32)


## Add captions and titles #####


## rows then columns with own titles ####

### titles and subtitles and caption ######


Maintitle = "Figure 3. Associations between anxiety and App task"

l1 <- expression(paste(bold("Figure 3.")," Plots visualising the absolute correlations between mean ratings per stimuli per experimental phase for the first week only in"))
l2 <- expression(paste("validation, app test re-test or lab test-retest. Correlations presented for app", italic(" (n =89) "),"and lab", italic(" (n = 91) "), "based testing seperately. Negative",sep=" "))
l3 <- expression(paste("correlations are indicated by the "-" symbol. Error bars represent bootstrapped 95% confidence intervals. Significant correlations are",sep=" "))
l4 <- expression(paste("indicated by *", italic(" (p < 0.05) "), "**", italic(" (p < 0.01) "), "or ***", italic(" (P < 0.001). "),bold("Panel A"), " presents bar plots showing the Pearson's correlations between average", sep=" "))
l5 <- expression(paste("participant expectancy rating for each stimulus during acquisition, extinction and renewal testing phases for lab", italic(" (left)"), "and App", italic(" (right)"),sep=" "))
l6 <- expression(paste("sessions respectively.", bold("Panel B")," presents bar plots showing the Pearson's correlation between average participant affective rating for each",sep=" "))
l7 <- expression(paste("stimulus after the extinction phase ",italic("(Post extinction)"),", and after the renewal phase",italic(" (Post renewal)"), " for lab", italic(" (left)"), "and App", italic(" (right)"), " sessions", sep=" "))
l8 <- expression(paste("respectively."))
## add title #####
# 
# alltit <-   alltogethernow + theme(plot.margin = unit(c(1, 0.6, 0.6, 0.6), "cm")) +
#                                  draw_label(Maintitle,
#                                   fontface='bold',
#                                   fontfamily = "Trebuchet MS",
#                                   colour = "black",
#                                   size = 32,
#                                   x = 1.01, 
#                                   y = 1.03,
#                                   vjust = 1, 
#                                   hjust = 1)  



alltit <- alltog + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
## add caption ####


## add caption ######

pl1 = add_sub(alltit,l1, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))


pl2 = add_sub(pl1,l2, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl3 = add_sub(pl2,l3, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))

pl4 = add_sub(pl3,l4, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl5 = add_sub(pl4,l5, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))

pl6 = add_sub(pl5,l6, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl7 = add_sub(pl6,l7, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))

pl8 = add_sub(pl7,l8, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = 20,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))



## Save it ##### 
# 
png("Fig3.anxiety_absolute_correlations.png",width=22000,height=26000,res=1200)
ggdraw(pl8)
dev.off()




## FIGURE 4: ICC ######

## read in the tables 
tabs <- "/Volumes/groups/Eley Group/Kirstin/FLARe_Analyses/ICC_tables_allstudies/"
val <- read.csv(paste(tabs,"Validation_ICC_table_complete.csv",sep=""))
app <- read.csv(paste(tabs,"AppTRT_ICC_table_complete.csv",sep=""))
lab <- read.csv(paste(tabs,"LabTRT_ICC_table_complete.csv",sep=""))
anx <- read.csv("/Volumes/groups/Eley Group/Kirstin/FLARe_Analyses/Validation/AnxCorrelations/Datasets/Anxiety_corr_bootstrappedCI_table.csv",
                header = T)

## merge the ICC ones

dat <- rbind(val,lab,app)

## drop percent change for generalisation and app/lab first from validation, and seperate out the generalisation group

dat <- dat[(dat$Condition == "Whole\n Group"),]
dat <- dat[(dat$Stimulus != "GS_Pch"),]
data <-  dat[(dat$Phase == "Baseline" | dat$Phase == "Post Extinction" | dat$Phase == "Post Renewal"),]
datg <- dat[(dat$Phase == "Generalisation"),]
dat <- dat[(dat$Phase != "Generalisation" & 
              dat$Phase != "Baseline" & dat$Phase != "Post Extinction" & dat$Phase != "Post Renewal"),]

# 
# 
# datg$Study <- ifelse(datg$Study == "Lab test-retest","Lab\ntest-retest",
#                      ifelse(datg$Study == "App test-retest", "App\ntest-retest","Validation"))

dat$Stimulus <- factor(dat$Stimulus, levels = c("CS+","CS-","GS1","GS2","GS3","GS4"))
dat$Study <- factor(dat$Study, levels = c("App test-retest","Lab test-retest","Validation"))
dat$Phase <- factor(dat$Phase, levels = c("Acquisition","Extinction","Renewal","Post Extinction","Post Renewal"))

datg$Stimulus <- factor(datg$Stimulus, levels = c("CS+","GS1","GS2","GS3","GS4","CS-"))
datg$Study <- factor(datg$Study, levels = c("Validation","Lab test-retest","App test-retest"))



data$Stimulus <- factor(data$Stimulus, levels = c("CS+","CS-"))
data$Study <- factor(data$Study, levels = c("App test-retest","Lab test-retest","Validation"))
data$Phase <- factor(data$Phase, levels = c("Baseline","Post Extinction","Post Renewal"))



### diverging bar - CS- on the left and CS+ on the right ####

dat$ICC <- ifelse(dat$Stimulus == "CS-",dat$ICC*(-1),dat$ICC)
dat$ci_down <- ifelse(dat$Stimulus == "CS-",dat$ci_down*(-1),dat$ci_down)
dat$ci_up <- ifelse(dat$Stimulus == "CS-",dat$ci_up*(-1),dat$ci_up)

data$ICC <- ifelse(data$Stimulus == "CS-",data$ICC*(-1),data$ICC)
data$ci_down <- ifelse(data$Stimulus == "CS-",data$ci_down*(-1),data$ci_down)
data$ci_up <- ifelse(data$Stimulus == "CS-",data$ci_up*(-1),data$ci_up)


  ## grouped by phase



# Diverging Barcharts - Acquisition #####

div <- ggplot(dat, aes(x=Study, 
                        y=ICC, 
                        label="Intraclass Correlations"))                                    + 
  geom_bar(stat='identity', 
           aes(fill=Stimulus), width=.5)                             +
  geom_errorbar(aes(ymin =  ci_down,
                    ymax = ci_up),
                position = position_dodge(width=.9),
                width = .1)                          +
  theme_bw() +
  scale_fill_manual(values=PMpallette)                +
  facet_wrap( ~ Phase,
              nrow=5)       +
#   geom_hline(aes(yintercept = 0))                  +
  geom_hline(aes(yintercept = 0))                  +
  coord_flip() +
  labs(y="\nIntraclass Correlation",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 38),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 32),
        strip.text = element_text(face = "bold",
                                  size = 38),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.x =  element_text(face = "bold",
                                     size = 36),
        axis.title.y =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 34),
        legend.title = element_text(face = "bold",
                                    size = 36),
        panel.spacing.x=unit(0, "lines"),
        legend.position = "none")     + 
  scale_y_continuous(limits = c(-1,1),
                     breaks=seq(-1,1,0.2),
                     labels = c(1,.8,.6,.4,.2,0,.2,.4,.6,.8,1))



# Diverging Barcharts - Affective panel #####

diva <- ggplot(data, aes(x=Study, 
                       y=ICC, 
                       label="Intraclass Correlations"))                                    + 
  geom_bar(stat='identity', 
           aes(fill=Stimulus), width=.5)                             +
  geom_errorbar(aes(ymin =  ci_down,
                    ymax = ci_up),
                position = position_dodge(width=.9),
                width = .1)                          +
  theme_bw() +
  scale_fill_manual(values=PMpallette)                +
  facet_wrap( ~ Phase,
              nrow=5)       +
  #   geom_hline(aes(yintercept = 0))                  +
  geom_hline(aes(yintercept = 0))                  +
  coord_flip() +
  labs(y="\nIntraclass Correlation",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 38),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 32),
        strip.text = element_text(face = "bold",
                                  size = 38),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        axis.title.x =  element_text(face = "bold",
                                     size = 36),
        axis.title.y =  element_blank(),
        legend.text = element_text(face = "bold",
                                   size = 34),
        legend.title = element_text(face = "bold",
                                    size = 36),
        legend.position = "left",
        legend.title.align=0.5,
        panel.spacing.x=unit(0, "lines"))     + 
  scale_y_continuous(limits = c(-1,1),
                     breaks=seq(-1,1,0.2),
                     labels = c(1,.8,.6,.4,.2,0,.2,.4,.6,.8,1)) +
  scale_x_discrete(position = "top")


## Generalisation  panel 




# Diverging Barcharts

divg <- ggplot(datg, aes(x=Study, 
                       y=ICC, 
                       label="Intraclass Correlations"))                                    + 
  geom_bar(stat='identity', 
           aes(fill=Stimulus), width=.5)                             +
  geom_errorbar(aes(ymin =  ci_down,
                    ymax = ci_up),
                position = position_dodge(width=.9),
                width = .1)                          +
  theme_bw() +
  scale_fill_manual(values=genpallete)                +
  facet_wrap(~ Stimulus,
              nrow=1)       +
  #   geom_hline(aes(yintercept = 0))                  +
  geom_hline(aes(yintercept = 0))                  +
 # coord_flip() +
  labs(y="\nIntraclass Correlation\n",
       subtitle = "\n\n")                 +
  theme(plot.subtitle = element_text(face = "italic",
                                     size = 38),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(face = "bold",
                                 size = 32),
        strip.text = element_text(face = "bold",
                                  size = 38),
        legend.key = element_rect(size = 3),
        legend.key.size = unit(3, 'lines'),
        # axis.title.x =  element_text(face = "bold",
        #                              size = 26),
        axis.title.x= element_blank(),
        axis.text.x = element_text(angle = 40,  hjust=1),
     # axis.ticks.y = element_blank(),
        axis.title.y =  element_text(face = "bold",
                                     size = 36),
        legend.text = element_text(face = "bold",
                                  size = 34),
        legend.title = element_text(face = "bold",
                                    size = 36),
     legend.position = "none",
        panel.spacing.x=unit(0, "lines"))     + 
  scale_y_continuous(limits = c(0,1),
                     breaks=seq(0,1,0.2),
                     labels = c(0,.2,.4,.6,.8,1)) #+
#  guides(fill = guide_legend(reverse=TRUE))


## combine the panels ####


plot2 <- plot_grid(div,diva,
                   labels = "AUTO",
                   label_size=32,
                   rel_widths = c(1,1.2))

plot2mar <- plot2 + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

plot <- plot_grid(plot2mar,divg,
                  labels=c("","C"),
                  nrow=2,
                  rel_heights = c(3,1.5),
                  rel_widths = c(1,1.4),
                  label_size=32)
                   
             

## Add captions and titles #####


## rows then columns with own titles ####

### titles and subtitles and caption ######


Maintitle = "Figure 4. Intraclass correlations between testing sessions"

blank <- " "
l1 <- expression(paste(bold("Figure 4.")," Plots visualising within person intraclass correlations between first and second testing sessions for all experimental phases of all studies",sep=" "))
l2 <- expression(paste("seperately. Error bars represent 95% confidence intervals.",bold(" Panel A"), " presents bar plots showing the 2-way absolute agreement intraclass correlation",sep=" "))
l3 <- expression(paste("of average participant expectancy rating for the CS-",italic(" (left)")," and CS+ ",italic(" (right)"), "for Acquisition, Extinction and Renewal phases.", bold(" Panel B"), " presents bar",sep=" "))
l4 <- expression(paste("plots showing the 2-way absolute agreement intraclass correlation of average participant affective rating for the CS-",italic(" (left)")," and CS+ ",italic(" (right) "), "for",sep=" "))
l5 <- expression(paste("Baseline, Post Extinction and Post Renewal phases.",bold(" Panel C"), " presents bar plots showing the 2-way absolute agreement intraclass correlation of" ,sep=" "))
l6 <- expression(paste("average participant expectancy rating for all stimuli during the generalisation phase of the experiment." ,sep=" ")) 

## add title #####
# 
# alltit <-   alltogethernow + theme(plot.margin = unit(c(1, 0.6, 0.6, 0.6), "cm")) +
#                                  draw_label(Maintitle,
#                                   fontface='bold',
#                                   fontfamily = "Trebuchet MS",
#                                   colour = "black",
#                                   size = 32,
#                                   x = 1.01, 
#                                   y = 1.03,
#                                   vjust = 1, 
#                                   hjust = 1)  



alltit <- plot + theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"))

## add caption ####


## add caption ######

first <- add_sub(alltit,blank, 
                 fontface='italic',
                 fontfamily = "Trebuchet MS",
                 colour = "black", 
                 size = 80,
                 hjust=0,
                 vjust=0,
                 x=0.15,
                 y=0.0,
                 vpadding=grid::unit(0, "lines"))

pl1 = add_sub(first,l1, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))


pl2 = add_sub(pl1,l2, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl3 = add_sub(pl2,l3, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=0,
              x=0.01,
              y=0.0,
              vpadding=grid::unit(0, "lines"))

pl4 = add_sub(pl3,l4, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))


pl5 = add_sub(pl4,l5, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))

pl6 = add_sub(pl5,l6, 
              fontface='italic',
              fontfamily = "Trebuchet MS",
              colour = "black", 
              size = caption,
              hjust=0,
              vjust=1,
              x=0.01,
              y=1,
              vpadding=grid::unit(0, "lines"))




## SAVE IT ##### 
# 
png("Fig4.All_ICC_divergent.png", width=figure.w,height=figure.l,unit=units,res=res)
ggdraw(pl6)
dev.off()
