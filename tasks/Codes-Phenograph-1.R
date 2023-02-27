######################################################################
###
### R script for PhenoGraph
###
### Hoda / Petar
###
### ggplot
### lm models 
### lmer

library(dplyr)
library(magrittr)
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(kableExtra)
library(shinyBS)
library(rsconnect)
library(auth0)
library(DT)
library(readxl)
library(kableExtra)
library(knitr)
library(ggbeeswarm)

library(performance)
library(easystats)

dat <- read_excel("tasks/G11&G12_V4_Petar.xlsx", sheet = "G11&G12_V3") %>% 
  as.data.frame()

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

data <- dat


# Selection and filters
# ------------------------------------------------------------------------------

column <- "G11Heightcm"

unique(dat$Environment)
unique(dat$ACC)
unique(dat$Population)


input <- list()

input$acc_factor <- c("Sha", "Col-0")
input$pol_factor <- c("PSD", "SSD-1")
input$env_factor <- c("control", "ancestor")
input$block_factor <- 3

data %<>% dplyr::mutate(BL = extract_numeric(Block))

data <- dat

acc_factor <- input$acc_factor
pol_factor <- input$pol_factor
env_factor <- input$env_factor

if(is.null(acc_factor) & is.null(pol_factor) & is.null(env_factor)){
  
  data <- data
  print(2)
} else if(is.null(acc_factor) & !is.null(pol_factor) & !is.null(env_factor)){
  
  data %<>% dplyr::filter(Environment %in% env_factor & Population %in% pol_factor) 
  print(3)
} else if(!is.null(acc_factor) & !is.null(pol_factor) & is.null(env_factor)){
  
  data %<>% dplyr::filter(ACC %in% acc_factor & Population %in% pol_factor) 
  print(4)
} else if(!is.null(acc_factor) & is.null(pol_factor) & !is.null(env_factor)){
  
  data %<>% dplyr::filter(ACC %in% acc_factor & Environment %in% env_factor) 
  print(5)
} else if(is.null(acc_factor) & is.null(pol_factor) & !is.null(env_factor)){
  
  data %<>% dplyr::filter(Environment %in% env_factor)
  print(6)
} else if(is.null(acc_factor) & !is.null(pol_factor) & is.null(env_factor)){
  
  data %<>% dplyr::filter(Population %in% pol_factor) 
  print(7)
} else if(!is.null(acc_factor) & is.null(pol_factor) & is.null(env_factor)){
  
  data %<>% dplyr::filter(ACC %in% acc_factor) 
  print(8)
} else { #if(!is.null(acc_factor) & !is.null(pol_factor) & !is.null(env_factor)){
  
  data %<>% dplyr::filter(ACC %in% acc_factor & Environment %in% env_factor & Population %in% pol_factor) 
  print(9)
}

dat <- data

debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}


debug_contr_error(data)

data <- na.omit(data)

drop_fixed_factors <- function(x) {
  x %>% discard(~is.factor(.x) & length(unique(na.omit(.x)))<2)
}



# 1. The overall analysis
# ------------------------------------------------------------------------------
resp <- "G11Heightcm"

m1 <- lm(terms(data[, resp] ~ Block  #Blocks
                 + ACC  #Accessions
                 + SEL + SOILvsSALT + CONvsANC  #Selection contrasts
                 + AvsC + PSvsSS  #Population contrasts
                 + ACC:(SEL + SOILvsSALT + CONvsANC + AvsC + PSvsSS + Population)  #Interactions of accessions
                 + (SEL + SOILvsSALT + CONvsANC):(AvsC + PSvsSS)  #Interactions selection x population contrasts
                 + ACC:(SEL + SOILvsSALT + CONvsANC):(AvsC + PSvsSS + Population),   #3-way interactions
               keep.order = T), 
         data = data)

anova(m1)

performance::check_model(m1)

kbl(round(anova(m1), 2), caption = paste0("Analysis of Variance Table\n", "Response: ", resp)) %>%
  # knitr::kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# ------------------------------------------------------------------------------


# 2. Interaction with two component
# ------------------------------------------------------------------------------

m2 <- lm(terms(data[, resp] ~ Block
               + ACC*AvsC,
               keep.order = F),
         data = data)

anova(m2)

performance::check_model(m2)



# ------------------------------------------------------------------------------


# visualizing the model 

ggplot(dd_plot) +
  aes(x = Type, y = Measurement, group = Acc) +
  geom_line(aes(linetype=Acc))+
  geom_point(aes(shape = Acc))+
  theme_minimal()




# 3. Interaction with three components (making one group out of two groups)
# ------------------------------------------------------------------------------

m3 <- lm(terms(G11Heightcm ~ Block
               + ACC * (AvsC + PSvsSS),
               keep.order = F),
         data = data)

anova(m3)

performance::check_model(m3)

# 4. Interaction of more components
# ------------------------------------------------------------------------------

m4 <- lm(terms(G11Heightcm ~ Block  #Blocks
             + SEL + SOILvsSALT + CONvsANC  #Selection contrasts
             + AvsC + PSvsSS  #Population contrasts
             +(SEL + SOILvsSALT + CONvsANC):(AvsC + PSvsSS),  #Interactions selection x population contrasts
             keep.order = T), 
         data = data)

anova(m4)

performance::check_model(m4)



# Visualisation of indices of models’ performance
# ------------------------------------------------------------------------------

plot(compare_performance(m1, m2, rank = TRUE))
