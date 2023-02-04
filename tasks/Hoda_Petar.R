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



