
# 2020 Columbia river Wetlands water isotopes and conductivity
require(tidyverse)
require(dplyr)
require(ggplot2)
require(plotly)
require(ggpubr)
require(htmlwidgets)


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

O18_lotic_boxplot_plotly<-
  ggplotly(O18_lotic_boxplot, tooltip = c("label","x","y"))   
saveWidget(O18_lotic_boxplot_plotly, file = "O18_lotic_boxplot_plotly.html")


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

H2_lotic_boxplot_plotly<-
  ggplotly(H2_lotic_boxplot, tooltip = c("label","x","y"))   
saveWidget(H2_lotic_boxplot_plotly, file = "output/H2_lotic_boxplot_plotly.html")



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

EC_lotic_boxplot_plotly<-
  ggplotly(EC_lotic_boxplot, tooltip = c("label","x","y"))   
saveWidget(EC_lotic_boxplot_plotly, file = "output/EC_lotic_boxplot_plotly.html")


lotic_all_vars<-ggarrange(O18_lotic_boxplot, H2_lotic_boxplot, EC_lotic_boxplot, nrow=3, common.legend = TRUE)
ggsave(lotic_all_vars, file = "output/lotic_all_vars.png", width=230, height=190, units = "mm",bg = 'white')


## comparing snow and groundwater


data2<- data %>% dplyr::filter(Type == "sCR" | Type == "lgCR" | Type == "R" | Type == "G")

set_seasons = function(x){
for (i in 1:nrow(x)){
  if (x$Sample.Date[i] >= "2020-04-01" & x$Sample.Date[i] <= "2020-05-31"){
    x$Season[i] = "Spring"
  }else if (x$Sample.Date[i]>="2020-06-01" & x$Sample.Date[i] <="2020-07-31"){
    x$Season[i] = "Summer"
  } else if (x$Sample.Date[i]>="2020-08-01" & x$Sample.Date[i] <="2020-10-30"){
    x$Season[i] = "Fall" 
  }
}
 x2<<-x
   return(x2)
}

set_seasons(data2)
data2<-x2

Ow<-read.csv("output/O_winter.csv")
for(i in 1:nrow(Ow)){Ow$mean[i] = Ow[i,3:7] %>% as.numeric() %>% mean()}
Ow$Type = "Snow"



data2$Type <- factor(data2$Type , levels=c("sCR", "lgCR", "R", "G"))

O18_lotic_w_GW_SW<-                                        ###this one has the right colours to variables
  data2 %>% 
  #mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R", "G"))) %>%
  ggplot()+
  geom_boxplot(aes(x=Type, y=O18, fill=Type), alpha=0.5)+
  geom_point(aes(x=Type, y=O18, colour=Type),show.legend= FALSE)+
  geom_boxplot(data=Ow, aes(x=Type, y=mean, fill=Type),alpha=0.5)+
  geom_point(data=Ow, aes(x=Type, y=mean, colour=Type),alpha=0.5, show.legend= FALSE)+
  theme_bw()+
  scale_x_discrete(limits=c(levels(data2$Type),"Snow")) +
  scale_fill_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  scale_colour_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

ggsave(O18_lotic_w_GW_SW, file="output/O18_lotic_w_GW_SW.png")
ggplotly(O18_lotic_w_GW_SW, tooltip = c("label","x","y") )

 #----------------------------making the snow H isotopes----------## last years sites

H1 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H01c.asc") #Jan
H2 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H02c.asc") #Feb
H3 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H03c.asc") #March
H11 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H11c.asc") #Nov
H12 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/H12c.asc") #Dec



winterH<-cbind(H1,H2,H3,H11,H12)

##crop to map extent
winterHc<-winterH
e<-extent(-117,-115.5,50,51.33)

for (i in 1:5) {
  winterHc[[i]]<-crop(winterH[[i]], e)
}

##extract values of o at sampling points and place in a table
sites<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2019/cleaned_data/GIS coordinates for all CRD sites.csv")

Hw<-data.frame(matrix(ncol=6, nrow=nrow(sites)))
Hw[,1]=sites$Site.number
names(Hw)[1]<-"site number"
sites2<- st_as_sf(sites, coords = c("Easting", "Northing"), crs="+proj=utm +zone=11 +datum=WGS84")
sites2<-st_transform(sites2, crs=st_crs(H1))

for (i in 1:5) {
  Hw[,i+1]=raster::extract(winterHc[[i]],sites2)
  names(Hw)[i+1]<- paste("H", i)
}

write.csv(Hw, file = "H_winter.csv")
for(i in 1:nrow(Hw)){Hw$mean[i] = Hw[i,2:6] %>% as.numeric() %>% mean()}
Hw$Type = "Snow"


H2_lotic_w_GW_SW<-
  data2 %>% 
  #mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R", "G"))) %>%
  ggplot()+
  geom_boxplot(aes(x=Type, y=H2, fill=Type), alpha=0.5)+
  geom_point(aes(x=Type, y=H2, colour=Type),show.legend= FALSE)+
  geom_boxplot(data=Hw, aes(x=Type, y=mean, fill=Type),alpha=0.5)+
  geom_point(data=Hw, aes(x=Type, y=mean, colour=Type),alpha=0.5, show.legend= FALSE)+
  theme_bw()+
  scale_x_discrete(limits=c(levels(data2$Type),"Snow")) +
  scale_fill_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  scale_colour_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

ggsave(H2_lotic_w_GW_SW, file="output/H2_lotic_w_GW_SW.png")


##-------------EC-------------

cond_NEON <- read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2019/cleaned_data/Conductivity/NEON cond data for mixing model_csv.csv") 
cond_NEON$Type="Snow"


EC_lotic_w_GW_SW<-
  data2 %>% 
  #mutate(Type = factor(Type, levels=c("sCR", "lgCR", "R", "G"))) %>%
  ggplot()+
  geom_boxplot(aes(x=Type, y=EC, fill=Type), alpha=0.5)+
  geom_point(aes(x=Type, y=EC, colour=Type),show.legend= FALSE)+
  geom_boxplot(data=cond_NEON, aes(x=Type, y=precipConductivity, fill=Type),alpha=0.5)+
  geom_point(data=cond_NEON, aes(x=Type, y=precipConductivity, colour=Type),alpha=0.5, show.legend= FALSE)+
  theme_bw()+
  scale_x_discrete(limits=c(levels(data2$Type),"Snow")) +
  scale_fill_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  scale_colour_manual(values = c("#6cceea", "#ceea6c", "#80CDC1","#B2ABD2","#FDB863"))+
  facet_wrap(~~factor(Season, levels=c('Spring', 'Summer', 'Fall')))+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank())

ggsave(EC_lotic_w_GW_SW, file="output/EC_lotic_w_GW_SW.png")


lotic_all_vars_w_GW_SW<-ggarrange(O18_lotic_w_GW_SW, H2_lotic_w_GW_SW, EC_lotic_w_GW_SW, nrow=3, common.legend = TRUE)
ggsave(lotic_all_vars_w_GW_SW, file = "output/lotic_all_vars_w_GW_SW.png", width=230, height=190, units = "mm",bg = 'white')


###we want to estimate the snow composition at the creek and river sites, not just at the wetland locations--> so need those co-ordinates
all.sites<-read.csv("clean data/GIS coordinates for all CRD sites.csv")

data2_utm<-left_join(data2, all.sites, join_by(Site.Number == Site.number), multiple = "all")

##-----------a mixing model with isotopes and EC to test the proportion of groundwater and snowmelt------------
##-----------------------------contributing to the creeks-----------------------------------

mix= matrix(c(data %>% filter ( Type == "sCR" |Type == "lgCR") %>% .$O18, 
              data %>% filter ( Type == "sCR" |Type == "lgCR") %>% .$H2,
              data %>% filter ( Type == "sCR" |Type == "lgCR") %>% .$EC), 
            ncol=3, nrow=nrow(data %>% filter ( Type == "sCR" |Type == "lgCR")))
colnames(mix)= c("18O","2H", "EC")

s_names = c("Groundwater", "Snowmelt") ##only need to run once, same for all seasons
s_means = matrix(c(data %>% filter ( Type == "G") %>% .$O18 %>% mean(),Ow$mean %>% mean(),
                      data %>% filter ( Type == "G") %>% .$H2 %>% mean(), Hw$mean %>% mean(), 
                      data %>% filter ( Type == "G") %>% .$EC %>% mean(), cond_NEON$precipConductivity%>% mean()), ncol=3, nrow=2)
s_sds = matrix(c(data %>% filter ( Type == "G") %>% .$O18 %>% sd(),Ow$mean %>% sd(),
                    data %>% filter ( Type == "G") %>% .$H2 %>% sd(), Hw$mean %>% sd(), 
                    data %>% filter ( Type == "G") %>% .$EC %>% sd(), cond_NEON$precipConductivity%>% sd()), ncol=3, nrow=2)

grp = (c(data2 %>% filter ( Type == "sCR" |Type == "lgCR")%>%.$Season))

##run model

simmr_creeks = simmr_load(mixtures=mix,
                          source_names=s_names,
                          source_means=s_means,
                          source_sds=s_sds,
                          group=grp)



plot(simmr_creeks,tracers=c(1,2), groups=1:3) 
plot(simmr_creeks,tracers=c(1,3),groups=1:3)
ggplotly(plot(simmr_creeks,tracers=c(1,3)))
plot(simmr_creeks,tracers=c(2,3),groups=1:3)

simmr_out = simmr_mcmc(simmr_creeks)
summary(simmr_out,type='diagnostics')
posterior_predictive(simmr_out)
prior_viz(simmr_out)
simmr_results_creeks<-summary(simmr_out)
stats<-summary(simmr_out,type='statistics', group=1:3)
quants<-summary(simmr_out_spring,type='quantiles', group=c(1:25))
plot(simmr_out,type='density')
plot(simmr_out,type='boxplot', group=1)
plot(simmr_out,type='matrix')


simmr_results_table<- function (ngroups,tracer) {
  results_table=matrix(data=NA, ncol=2, nrow=ngroups)
  for (i in 1:ngroups) {
    results_table[i,1]= stats$statistics[[i]][[2,1]]
    results_table[i,2]= stats$statistics[[i]][[2,2]]
  }
  colnames(results_table)= c(paste(tracer,"_mean"), paste(tracer,"_sd"))
  rownames(results_table) <- c(unique(grp))
  return(results_table)
}

season_creek_results<-simmr_results_table(ngroups=3, tracer="Groundwater")
write.csv(season_creek_results, file="output/creeks_season_proportions.csv")

###---------run again, each creeks individually---------------
grp = as.integer(1:nrow(data %>% filter ( Type == "sCR" |Type == "lgCR")))

simmr_creeks_ind = simmr_load(mixtures=mix,
                          source_names=s_names,
                          source_means=s_means,
                          source_sds=s_sds,
                          group=grp)
simmr_out = simmr_mcmc(simmr_creeks_ind)
stats<-summary(simmr_out,type='statistics', group=1:24)
ind_creek_results<-simmr_results_table(ngroups=24, tracer="Groundwater")
ind_creek_results<-cbind(ind_creek_results, data %>% filter ( Type == "sCR" |Type == "lgCR"))
write.csv(ind_creek_results, file="output/ind_creek_results_prop.csv")

###------------making some tern plots----------
library(ggtern)
