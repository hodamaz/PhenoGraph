######################################################################
###
### R script for PhenoGraph
###
### Hoda / Petar
###
### ggplot
### lm models 
### lmer

# the overall analysis
m1 <- lm(terms(y~BL  #Blocks
                 +ACC  #Accessions
                 +SEL+SOILvsSALT+CONvsANC  #Selection contrasts
                 +AvsC+PSvsSS  #Population contrasts
                 +ACC:(SEL+SOILvsSALT+CONvsANC+AvsC+PSvsSS+POP)  #Interactions of accessions
                 +(SEL+SOILvsSALT+CONvsANC):(AvsC+PSvsSS)  #Interactions selection x population contrasts
                 +ACC:(SEL+SOILvsSALT+CONvsANC):(AvsC+PSvsSS+POP)  #3-way interactions
                 ,keep.order=T)
           ,data=dat)
anova(m2Contr)


# interaction with two component
m2 <- lm(terms(y~BL+ACC*AvsC,keep.order=F),data=dat)
anova(m11)

# visualizing the model 
ggplot(dd_plot) +
  aes(x = Type, y = Measurement, group = Acc) +
  geom_line(aes(linetype=Acc))+
  geom_point(aes(shape = Acc))+
  theme_minimal()

# (interaction with three components), (making one group out of two groups)
m3=lm(terms(y~BL+ACC*(AvsC+PSvsSS),keep.order=F),data=dat)
anova(m12)

# (interaction of more components)
m18=lm(terms(y~BL  #Blocks
             +SEL+SOILvsSALT+CONvsANC  #Selection contrasts
             +AvsC+PSvsSS  #Population contrasts
             +(SEL+SOILvsSALT+CONvsANC):(AvsC+PSvsSS)  #Interactions selection x population contrasts
             ,keep.order=T)
       ,data=d)