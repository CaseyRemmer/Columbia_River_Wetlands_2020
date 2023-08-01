
# 2020 Columbia river Wetlands water isotopes and conductivity
require(tidyverse)
require(dplyr)
require(ggplot2)
require(plotly)
require(ggpubr)

### for later!
# sample size
sample_size = data %>% group_by(name) %>% summarize(num=n())

# Plot
data %>%
  left_join(sample_size) %>%
  mutate(myaxis = paste0(name, "\n", "n=", num)) %>%
  ##then gglot boxplots


data<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/clean data/CRW_2020_cleaned_data_for_analysis.csv")
head(data)

#comparing the small creeks, large creeks and rivers

cr_rv<- data %>% dplyr::filter(Type == "sCR" | Type == "lgCR" | Type == "R")
#view(cr_rv)

for (i in 1:nrow(cr_rv)){
  if (cr_rv$Sample.Date[i] >= "2020-04-01" & cr_rv$Sample.Date[i] <= "2020-05-31"){
    cr_rv$Season[i] = "Spring"
    }else if (cr_rv$Sample.Date[i]>="2020-06-01" & cr_rv$Sample.Date[i] <="2020-07-31"){
    cr_rv$Season[i] = "Summer"
    } else if (cr_rv$Sample.Date[i]>="2020-08-01" & cr_rv$Sample.Date[i] <="2020-10-30"){
  cr_rv$Season[i] = "Fall" 
  }
}

##plot as boxplots
O18_lotic_boxplot<- 
  cr_rv %>% 
  mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R"))) %>%
    ggplot(aes(x=Type, y=O18, label = Site.Number))+
      geom_boxplot(aes(fill=Type), alpha=0.5)+
      geom_jitter(aes(colour=Type),show.legend= FALSE)+
      theme_bw()+
      scale_fill_manual(labels = c("small creeks", "large creeks", "Columbia River"),
                     values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
      scale_colour_manual(values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
    facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
      theme(axis.title.x = element_blank(), axis.text.x=element_blank())

H2_lotic_boxplot<- 
  cr_rv %>% 
  mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R"))) %>%
  ggplot(aes(x=Type, y=H2,label = Site.Number))+
  geom_boxplot(aes(fill=Type), alpha=0.5)+
  geom_jitter(aes(colour=Type),show.legend= FALSE)+
  theme_bw()+
  scale_fill_manual(labels = c("small creeks", "large creeks", "Columbia River"),
                    values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

EC_lotic_boxplot<- 
  cr_rv %>% 
  mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R"))) %>%
  ggplot(aes(x=Type, y=EC),label = Site.Number)+
  geom_boxplot(aes(fill=Type), alpha=0.5)+
  geom_jitter(aes(colour=Type),show.legend= FALSE)+
  theme_bw()+
  scale_fill_manual(labels = c("small creeks", "large creeks", "Columbia River"),
                    values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  scale_colour_manual(values = c("#B2ABD2",  "#0571B0", "#80CDC1"))+
  facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

lotic_all_vars<-ggarrange(O18_lotic_boxplot, H2_lotic_boxplot, EC_lotic_boxplot, nrow=3, common.legend = TRUE)


ggplotly(lotic_all_vars, tooltip = c("label","x","y"))       


