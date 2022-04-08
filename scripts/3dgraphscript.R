#import libraries
library(gganimate)
library(ggplot2)
library(tidyverse)
library(rgl)
library(matrixStats)
library(readxl)
library(plotly)
library(crosstalk)

#import the data from excel and take a look
blueteam_data2 <- read_excel("GitHub/Purple-Modeling/data/blueteam_data2.xlsx", 
                             skip = 6)
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
#use crosstalk to add in the filters on the web side
shared_AA<- SharedData$new(AA)

bscols(
  list(
  filter_checkbox('SkillSlider','Skill Slider', shared_AA, group = AA$SkillSlider,inline = TRUE),
  filter_checkbox('BiasSlider','Bias Slider', shared_AA, group = AA$BiasSlider,inline = TRUE),
  filter_checkbox('HonestySlider','Honesty Slider', shared_AA, group = AA$HonestySlider,inline = TRUE)
),plot_ly(data = shared_AA,
    x=AA$SkillSlider,
    y=AA$BiasSlider,
    z=AA$HonestySlider) %>% 
  add_markers(color = AA$AA1))
graphaa3d


####################AV Graph##########################
shared_AV<- SharedData$new(AV)

bscols(
  list(
    filter_checkbox('SkillSlider','Skill Slider', shared_AV, group = AV$SkillSlider,inline = TRUE),
    filter_checkbox('BiasSlider','Bias Slider', shared_AV, group = AV$BiasSlider,inline = TRUE),
    filter_checkbox('HonestySlider','Honesty Slider', shared_AV, group = AV$HonestySlider,inline = TRUE)
  ),plot_ly(data = shared_AV,
            x=AV$SkillSlider,
            y=AV$BiasSlider,
            z=AV$HonestySlider) %>% 
    add_markers(color = AV$AV1))

###################VA Graph#########################
shared_VA<- SharedData$new(VA)

bscols(
  list(
    filter_checkbox('SkillSlider','Skill Slider', shared_VA, group = VA$SkillSlider,inline = TRUE),
    filter_checkbox('BiasSlider','Bias Slider', shared_VA, group = VA$BiasSlider,inline = TRUE),
    filter_checkbox('HonestySlider','Honesty Slider', shared_VA, group = VA$HonestySlider,inline = TRUE)
  ),
  plot_ly(data = shared_VA,
            x=VA$SkillSlider,
            y=VA$BiasSlider,
            z=VA$HonestySlider) %>% 
    add_markers(color = VA$VA1))

############################################
shared_VV<- SharedData$new(VV)

bscols(
  list(
    filter_checkbox('SkillSlider','Skill Slider', shared_VV, group = VV$SkillSlider,inline = TRUE),
    filter_checkbox('BiasSlider','Bias Slider', shared_VV, group = VV$BiasSlider,inline = TRUE),
    filter_checkbox('HonestySlider','Honesty Slider', shared_VV, group = VV$HonestySlider,inline = TRUE)
  ),
  plot_ly(data = shared_VV,
          x=VV$SkillSlider,
          y=VV$BiasSlider,
          z=VV$HonestySlider) %>% 
    add_markers(color = VV$VV1))
