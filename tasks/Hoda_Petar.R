# Description ----
###
### R script for analysis of Hoda
###
### Hoda 
###
### History:
### - 18-07-2021 file created
### - 02-02-2022 changes
### - 21-05-2022 Added graph for global distributions
### - 09-07-2022 Different models for phenotypic features 
### - 25-10-2022 visualizing models 
### - 30-01-2023 changes for Petar and the Shiny app

options(digits=4)                     # set number of post-decimal digits in output to 4
rm(list=ls())                         # clear workspace


# Libraries:----
#install.packages("readxl")
library(tidyverse)
library(data.table)
library(readxl)
library(ggplot2)

# Read in data:----
#Desktop Bernhard:

dat <- read_excel("tasks/G11&G12_V4_Petar.xlsx", sheet = "G11&G12_V3") %>% 
  as.data.frame()

## Structure of data set:
str(dat)
names(dat)
dim(dat)
## Make factors:
dat$Block=factor(dat$Block)
dat$ACC=factor(dat$ACC)
dat$Ind=factor(dat$Ind)
dat$Environment=factor(dat$Environment)
dat$SEL=factor(as.numeric(dat$SelectionLevel>1),labels=c("ancestor or control","selected"))
dat$SOILvsSALT=factor(as.numeric(dat$SelectionLevel>2),labels=c("not soil","soil"))
dat$CONvsANC=factor(as.numeric(dat$SelectionLevel>0),labels=c("ancestor","not ancestor"))
dat$Population=factor(dat$Population)
dat$AvsC=factor(dat$AvsC,labels=c("apomictic","crossed"))
dat$psvsss=as.numeric(dat$PSDvsSSD>1)*0+as.numeric(dat$PSDvsSSD==1)*1+as.numeric(dat$PSDvsSSD==0)*2
dat$PSvsSS=factor(dat$psvsss,labels=c("SSD","PSD","EHP"))

# Tab-1 'Data Selection'----
## Button-1 name: Dependent factor ----
## Button-1 options: Select the measurement (Multiple selection should be possible)----

#G12SY #G11FT #G11HT #G12FT #G12HT #G12MB #G12N 
# By selection na.remove should be applied

## Button-2 name: Independent factor ----
## Button-2 options: Select the independent factor:----

# Block (trays) default is always selected
# ACC efault is always selected
# Ind 
# Environment
# SEL
# SOILvsSALT
# CONvsANC
# CONvsANC
# Population
# AvsC
# PSvsSS

# Graph 1 (histogram of selected variables + initial statistics) ----
# First graph by selection of the in/dependent variables 
hist(dat$G11Heightcm)
tapply(dat$G11Heightcm,mean,na.rm=T)

unique(dat$Environment)
unique(dat$Population)
unique(dat$ACC)
unique(dat$SEL)

names(dat)

unique(dat$Block)

library(tidyr)
extract_numeric(dat$Block)

dat %<>% dplyr::mutate(Block_num = extract_numeric(Block))

data <- dat
select_hist_var <- "G11Heightcm"
cc <- select_hist_var


input <- list()

input$acc_factor <- NULL
input$svs_factor <- NULL
input$cva_factor <- NULL
input$pvs_factor <- NULL
input$dep_factor <- NULL
input$pol_factor <- NULL
input$env_factor <- NULL
input$avc_factor <- NULL

# if(is.null(input$acc_factor) & is.null(input$svs_factor) & is.null(input$cva_factor) & is.null(input$pvs_factor) & is.null(input$dep_factor) & is.null(input$pol_factor) & is.null(input$env_factor) & is.null(input$avc_factor)){
#   data <- data
# } else{
  data %<>% dplyr::filter(ACC %in% input$acc_factor | Environment %in% input$env_factor | Population %in% input$pol_factor | SEL %in% input$sel_factor | SOILvsSALT %in% input$svs_factor | CONvsANC %in% input$cva_factor | AvsC %in% input$avc_factor | PSvsSS %in% input$pvs_factor) %>%
    dplyr::filter(Block_num == as.numeric(input$block_factor))
# }


hist <- ggplot(data = data, aes_string(cc)) + 
  geom_histogram(binwidth = 0.05,
                 col="red", 
                 aes(y = ..density..,
                     fill = ..count..)) +
  scale_fill_gradient("Count", low = "blue", high = "red")+
  stat_function(fun = dnorm,
                color = "orange",
                size = 1.5,
                args = list(mean = mean(as.numeric(data[cc][,]), na.rm = TRUE), sd = sd(as.numeric(data[cc][,]), na.rm = TRUE)))+
  # geom_text(x = min(as.numeric(data[cc][,])) + sd(as.numeric(data[cc][,]), na.rm = TRUE), y =  sd(as.numeric(data[cc][,]), na.rm = TRUE) * 3, label = paste0("Mean: ", mean(as.numeric(data[cc][,]), na.rm = TRUE), "\nSD: ", sd(as.numeric(data[cc][,]), na.rm = TRUE))) +
  labs(title=paste("Variable Histogram: ","Dependent - ", cc, sep = ""), 
       subtitle = paste0("Mean: ", mean(as.numeric(data[cc][,]), na.rm = TRUE), "\nSD: ", sd(as.numeric(data[cc][,]), na.rm = TRUE))) +
  labs(x="Sample values", y="Density") +
  theme_bw()

hist


xlsx_data <- readxl::read_excel(path = "tasks/G11&G12_V4_Petar.xlsx", sheet = "Description") %>% 
  as.data.frame()

xlsx_data

kbl(xlsx_data) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kbl(dat[1:10, ]) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

names(dat)[2:8]

