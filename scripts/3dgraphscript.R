#import libraries
library(gganimate)
library(ggplot2)
library(tidyverse)
library(rgl)
library(matrixStats)
library(readxl)
library(plotly)

#import the data from excel and take a look
blueteam_data1<- blueteam_data2
View(blueteam_data1)

#filter out irrelevant columns
data1 <- blueteam_data1 %>% select(
  SkillSlider, BiasSlider, HonestySlider, 19:42
)

#calculate the mean and variance of credence
mean1<- rowMeans(data1[,4:27])
var1<- rowVars(as.matrix(data1[,4:27]))

#add the rows back to the dataset and refilter
data1$mean <- mean1
data1$var <- var1

data2 <- data1 %>% select(
  SkillSlider, BiasSlider, HonestySlider, mean, var
)

#Calculate AA, VA, AV, and VV and do some necessary restructuring of the data.

#Next, I will calculate the AA, AV, VA, and VV. Each one of these new datasets will be a graph.

AA<- data2 %>% 
  group_by(SkillSlider, BiasSlider, HonestySlider) %>%
  summarize(mean(mean))

AV <- data2 %>% 
  group_by(SkillSlider, BiasSlider, HonestySlider) %>%
  summarize(mean(var))

VA <- data2 %>% 
  group_by(SkillSlider, BiasSlider, HonestySlider) %>%
  summarize(var(mean))

VV <- data2 %>% 
  group_by(SkillSlider, BiasSlider, HonestySlider) %>%
  summarize(var(var))

#convert the data into dataframes
AA<-data.frame(AA)
AV<-data.frame(AV)
VA<-data.frame(VA)
VV<-data.frame(VV)
#change the names of the fourth row
names(AA)[4] <- 'AA1'
names(AV)[4] <- 'AV1'
names(VA)[4] <- 'VA1'
names(VV)[4] <- 'VV1'

#create the graphs
graphaa3d<- plot_ly(x=AA$SkillSlider,
                    y=AA$BiasSlider,
                    z=AA$HonestySlider) %>% 
  add_markers(color = AA$AA1)
graphaa3d

graphav3d <- plot_ly(x=AV$SkillSlider,
                  y=AV$BiasSlider,
                  z=AV$HonestySlider) %>% 
  add_markers(color = AV$AV1) 
graphav3d

graphva3d <- plot_ly(x=VA$SkillSlider,
                     y=VA$BiasSlider,
                     z=VA$HonestySlider) %>% 
  add_markers(color = VA$VA1)
graphva3d

graphvv3d <- plot_ly(x=VV$SkillSlider,
                     y=VV$BiasSlider,
                     z=VV$HonestySlider) %>% 
  add_markers(color = VV$VV1)
graphvv3d