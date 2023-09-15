
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


data<-read.csv("/clean_data/CRW_2020_cleaned_data_for_analysis.csv")
data<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/clean_data/CRW_2020_clean_data_for_analysis.csv")
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
  geom_boxplot(data=Ow, aes(x=Type, y=mean, fill=Type),alpha=0.5)+ ##mean for full season snow, O3 for only march snow
  geom_point(data=Ow, aes(x=Type, y=mean, colour=Type),alpha=0.5, show.legend= FALSE)+
  theme_bw()+
  scale_x_discrete(limits=c(levels(data2$Type),"Snow")) +
  scale_fill_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
  scale_colour_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
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

Hw<-read.csv("H_winter.csv")
for(i in 1:nrow(Hw)){Hw$mean[i] = Hw[i,3:7] %>% as.numeric() %>% mean()}
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
  scale_fill_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
  scale_colour_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
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
  scale_fill_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
  scale_colour_manual(values = c("#B2ABD2","#ceea6c", "#80CDC1","#6cceea","#FDB863"))+
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
s_means = matrix(c(data %>% filter ( Type == "G") %>% .$O18 %>% mean(),Ow$O.3 %>% mean(),
                      data %>% filter ( Type == "G") %>% .$H2 %>% mean(), Hw$H.3 %>% mean(), 
                      data %>% filter ( Type == "G") %>% .$EC %>% mean(), cond_NEON$precipConductivity%>% mean()), ncol=3, nrow=2)
s_sds = matrix(c(data %>% filter ( Type == "G") %>% .$O18 %>% sd(),Ow$O.3 %>% sd(),
                    data %>% filter ( Type == "G") %>% .$H2 %>% sd(), Hw$H.3 %>% sd(), 
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
ind_creek_results<-cbind(ind_creek_results, data2 %>% filter ( Type == "sCR" |Type == "lgCR"))
write.csv(ind_creek_results, file="output/ind_creek_results_prop.csv")
ind_creek_results<-read.csv("output/ind_creek_results_prop.csv")

###------------making some plots----------

ind_creek_results$Snowmelt_mean<-1-ind_creek_results$`Groundwater _mean`
head(ind_creek_results)

perc_GW_creeks_plot<-
  ind_creek_results %>% group_by(Season) %>%
  mutate(Season = factor(Season, levels=c("Spring", "Summer", "Fall"))) %>%
  ggplot(aes(y=Site.Name, x=Season, fill=`Groundwater._mean`))+
  geom_tile(color= "white",linewidth=0.1)+
  geom_text(aes(label=paste(round(`Groundwater._mean`,2)*100,"%")))+
  scale_fill_gradient2(midpoint=mean(ind_creek_results$`Groundwater._mean`),low="#FDB863", mid="#B2ABD2", high="blue", limits=c(0.5,1))

ggsave(perc_GW_creeks_plot, file = "output/perc_GW_creeks_plot.png", width=230, height=190, units = "mm",bg = 'white')



##------##--------main mixing model using River, Precipitation and Groundwater on the Wetland sites of 2020--------------------------

EC_data<-read.csv("clean_data/NEON_EC_2020.csv")
EC_data$collectDate<-as.Date(EC_data$collectDate, format = "%Y-%m-%d") ##converting dates from factor to dates 

gis2<-read.csv("raw data/Locations & numbers of sites for CW 2020_csvofsites.csv")
gis2<-gis2[,-(5:7)]
names(gis2)<-c("Site.Number","Site.Name","Easting", "Northing")
gis2$Northing<-as.integer(gis2$Northing)

data_gis<-left_join(data, gis2, by=join_by("Site.Number"=="Site.Number"), multiple= "all")
data_gis<-rows_patch(data_gis, gis2, by= "Site.Number")


data_gis[1:3,]$Easting=524643
data_gis[1:3,]$Northing=5656772

data_gis$Sample.Date <- as.Date(data_gis$Sample.Date, format = "%Y-%m-%d")


set_seasons(data_gis)
data_gis<-set_seasons(data_gis)

EC_spring<-filter(EC_data, collectDate >= "2020-04-01" & collectDate <= "2020-05-31")
EC_spring1<-subset(EC_spring, select=c(domainID, collectDate, precipConductivity, precipConductivityUncertainty))
colnames(EC_spring1)<-colnames(cond_NEON_S)
EC_spring2<-rbind(EC_spring1, cond_NEON_Sp)

EC_summer<-filter(EC_data, collectDate >= "2020-06-01" & collectDate <= "2020-07-31")
EC_summer1<-subset(EC_summer, select=c(domainID, collectDate, precipConductivity, precipConductivityUncertainty))
colnames(EC_summer1)<-colnames(cond_NEON_S)
EC_summer2<-rbind(EC_summer1, cond_NEON_S)

EC_Fall<-filter(EC_data, collectDate >= "2020-08-01" & collectDate <= "2020-09-30")
EC_fall1<-subset(EC_Fall, select=c(domainID, collectDate, precipConductivity, precipConductivityUncertainty))
colnames(EC_fall1)<-colnames(cond_NEON_S)
EC_fall2<-rbind(EC_fall1, cond_NEON_F)

##---- Spring data mixing model--------


#Locations & numbers of sites for CW 2020_csvofsites

##extract values of o at sampling points and place in a table for the spring


#d<- data_gis[,-9]
d<-data_gis%>% drop_na(Easting)
Hs_2020<-data.frame(matrix(ncol=6, nrow=nrow(d)))
names(Hs_2020)[1]<-"Site.Number"
Hs_2020[,1]=d$Site.Number
d2<- st_as_sf(d, coords = c("Easting", "Northing"), crs="+proj=utm +zone=11 +datum=WGS84")
d2<-st_transform(d2, crs=st_crs(H[[1]]))

for (i in 1:6) {
  Hs_2020[,i+1]=raster::extract(H[[i]],d2)
  names(Hs_2020)[i+1]<- paste0("H",i+3)
}

Os_2020<-data.frame(matrix(ncol=6, nrow=nrow(d)))
Os_2020[,1]=d$Site.Number
names(Os_2020)[1]<-"Site.Number"

for (i in 1:6) {
  Os_2020[,i+1]=raster::extract(O[[i]],d2)
  names(Os_2020)[i+1]<- paste0("O", i+3)
}


write.csv(Os_2020, file= "O18_summer_precip.csv")
##Os_2020<-read.csv("output/O18_summer_precip.csv")
write.csv(Hs_2020, file= "H2_summer_precip.csv")
#Hs_2020<-read.csv("output/H2_summer_precip.csv")
##
##
###the spring isotopes are very enriched- could be the results of evaporation-- might be necessary to calculate the dI
##to get a true sense of the inputs, otherwise it just comes out as high precipitation because of the enriched isotopes
## the spring precip source is also more depleted in the spring, so that pushes the values even further to that outcome
## ie. the wetlands are more enriched than precip (which isnt really possible)
###so next step is to calculate dI, using the approach used for the Prarie Potholes, and then run the model using the dIs
##after that is done, I need to figure out how I made the colours for the tern plots and finalize those
## present a set of figures and decide on the main points of the results

##need to calculate DI , need temp and RH to do that.
# next step is to compare BRISCO and GOLDEN climate stations, decide which to use for T and rh
## next step is to calculate the E/I's using the new rh values

EC_1<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/raw data/climate_data/EC_raw/1173220.csv")
EC_2<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/raw data/climate_data/EC_raw/1173209.csv")
brisco<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/raw data/climate_data/FLNRO-WMB/865.csv")

## EDA on data 
EC_1$time<-as.Date(EC_1$time, format= "%Y-%m-%d")
class(EC_1$time)
EC_2$time<-as.Date(EC_2$time, format= "%Y-%m-%d")
brisco$time<-as.Date(brisco$time, format= "%Y-%m-%d")

EC_1$relative_humidity<-as.numeric(EC_1$relative_humidity)
EC_1<-EC_1 %>% drop_na(relative_humidity)
brisco$rel_hum<-as.numeric(brisco$rel_hum)
brisco<-brisco %>% drop_na(rel_hum)
EC_1$air_temperature<-as.numeric(EC_1$air_temperature)
EC_1<-EC_1 %>% drop_na(air_temperature)
brisco$air_temp<-as.numeric(brisco$air_temp)
brisco<-brisco %>% drop_na(air_temp)

spring_rh<-
  (EC_1%>%filter(time >= "2020-04-01" & time <="2020-05-31") %>% .$relative_humidity %>% mean() +
  brisco%>%filter(time >= "2020-04-01" & time <="2020-05-31") %>% .$rel_hum %>% mean())/2
summer_rh<-
  (EC_1%>%filter(time >= "2020-06-01" & time <="2020-07-31") %>% .$relative_humidity %>% mean() +
     brisco%>%filter(time >= "2020-06-01" & time <="2020-07-31") %>% .$rel_hum %>% mean())/2
fall_rh<-
  (EC_1%>%filter(time >= "2020-08-01" & time <="2020-09-30") %>% .$relative_humidity %>% mean() +
     brisco%>%filter(time >= "2020-08-01" & time <= "2020-09-30") %>% .$rel_hum %>% mean())/2
##test
fall_rh<-69.52 
     brisco%>%filter(time >= "2020-08-01" & time <= "2020-09-30") %>% .$rel_hum %>% mean()
spring_rh<-69.52 
     brisco%>%filter(time >= "2020-04-01" & time <="2020-05-31") %>% .$rel_hum %>% mean()
summer_rh<-69.52 
     brisco%>%filter(time >= "2020-06-01" & time <="2020-07-31") %>% .$rel_hum %>% mean()



spring_temp<-
  (EC_1%>%filter(time >= "2020-04-01" & time <="2020-05-31") %>% .$air_temperature %>% mean() +
     brisco%>%filter(time >= "2020-04-01" & time <="2020-05-31") %>% .$air_temp %>% mean())/2
summer_temp<-
  (EC_1%>%filter(time >= "2020-06-01" & time <="2020-07-31") %>% .$air_temperature %>% mean() +
     brisco%>%filter(time >= "2020-06-01" & time <="2020-07-31") %>% .$air_temp %>% mean())/2
fall_temp<-
  (EC_1%>%filter(time >= "2020-08-01" & time <="2020-09-30") %>% .$air_temperature %>% mean() +
     brisco%>%filter(time >= "2020-08-01" & time <= "2020-09-30") %>% .$air_temp %>% mean())/2


results<-matrix(ncol=11, nrow=nrow(data))
colnames(results)<-c("EI", "starO", "starH", "sslO", "sslH", "EO", "EH", "slope", "input", "IO", "IH")

Oma<-raster("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/clean_data/IsotopeMaps/Oma.asc")
Hma<-raster("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/clean_data/IsotopeMaps/Hma.asc")

Hma_2020<-data.frame(matrix(ncol=2, nrow=nrow(d)))
names(Hma_2020)[1]<-"Site.Number"
Hma_2020[,1]=d$Site.Number
#d2<- st_as_sf(d, coords = c("Easting", "Northing"), crs="+proj=utm +zone=11 +datum=WGS84")
#d2<-st_transform(d2, crs=st_crs(Hma[[1]]))

Hma_2020[,2]<-raster::extract(Hma,d2)
write.csv(Hma_2020, file="output/H_2020_annual_precip.csv")

Oma_2020<-data.frame(matrix(ncol=2, nrow=nrow(d)))
Oma_2020[,1]=d$Site.Number
names(Oma_2020)[1]<-"Site.Number"
Oma_2020[,2]<-raster::extract(Oma,d2)
write.csv(Oma_2020, file="output/O_2020_annual_precip.csv")

##tesing new rh values
results<-matrix(ncol=11, nrow=nrow(data))
colnames(results)<-c("EI", "starO", "starH", "sslO", "sslH", "EO", "EH", "slope", "input", "IO", "IH")


isotopic_framework<-  ##need Os & Hs from below
  function (data){
    data2<-data%>% filter (Type=="W")
    results<-matrix(ncol=13, nrow=nrow(data2))
    results[,1]<-data2$Site.Number
    results[,2]<-data2$Sample.Date
    colnames(results)<-c("Site.Number","date", "starO", "starH", "sslO", "sslH", "EO", "EH", "slope", "input", "IO", "IH","EI")
    for (i in 1:nrow(data2)){
      if (data2$Sample.Date[i] >= "2020-04-01" & data2$Sample.Date[i] <= "2020-05-31"){
        K = spring_temp + 273.15
      }else if (data2$Sample.Date[i] >= "2020-05-01" & data2$Sample.Date[i] <= "2020-06-31"){
        K = summer_temp + 273.15
      } else if (data2$Sample.Date[i] >= "2020-08-01" & data2$Sample.Date[i] <= "2020-10-30"){
        K = fall_temp + 273.15 }
      
      if (data2$Sample.Date[i] >= "2020-04-01" & data2$Sample.Date[i] <= "2020-05-31"){
        rh = spring_rh/100
      }else if (data2$Sample.Date[i] >= "2020-05-01" & data2$Sample.Date[i] <= "2020-06-31"){
        rh = summer_rh/100
      } else if (data2$Sample.Date[i] >= "2020-08-01" & data2$Sample.Date[i] <= "2020-10-30"){
        rh = fall_rh/100 }
      
      # α*
      a18O<-exp((-7.685+6.7123*((10^3)/K)-1.664*((10^6)/(K^2))+0.35041*((10^9)/(K^3)))/1000)
      a2H<- exp((1158.8*(K^3/10^9)-1620.1*(K^2/10^6)+794.84*(K/10^3)-161.04+2.9992*(10^9/K^3))/1000)
      ##ε*
      e18O<-a18O-1
      e2H<-a2H-1
      #εk
      ek18O<-0.0142*(1-rh)
      ek2H<-0.0125*(1-rh)
      ##alternate version dAS for Prarie pothole (sSSL calculated)
      #needs dPs
      dPs18O<-Os_2020 %>% filter (Site.Number== data2$Site.Number[i]) %>% .[1,2:7] %>% as.numeric(.) %>% mean() ##need to Hs_2020 and Os_2020 from below
      dPs18O<-dPs18O/1000
      dPs2H<-Hs_2020 %>% filter (Site.Number== data2$Site.Number[i]) %>% .[1,2:7] %>% as.numeric(.) %>% mean()
      dPs2H<-dPs2H/1000
      dAs18O<-(dPs18O-e18O)/a18O
      dAs2H<-(dPs2H-e2H)/a2H
      #δp
      dp18O<-Oma_2020 %>% filter (Site.Number== data2$Site.Number[i]) %>% .[1,2] %>% as.numeric(.) %>% mean()
      dp18O<-dp18O/1000
      dp2H<-Hma_2020 %>% filter (Site.Number== data2$Site.Number[i]) %>% .[1,2] %>% as.numeric(.) %>% mean()
      dp2H<-dp2H/1000
      #δ*
      dstar18O<-((rh*dAs18O)+ek18O+(e18O/a18O))/(rh-ek18O-(e18O/a18O))
      dstar2H<-(rh*dAs2H+ek2H+e2H/a2H)/(rh-ek2H-e2H/a2H)
      results[i,3]<-dstar18O
      results[i,4]<-dstar2H
      
      
      #δSSL 
      dssl18O<-a18O*dp18O*(1-rh+ek18O)+a18O*rh*dAs18O+a18O*ek18O+e18O
      dssl2H<-a2H*dp2H*(1-rh+ek2H)+a2H*rh*dAs2H+a2H*ek2H+e2H
      results[i,5]<- dssl18O
      results[i,6]<- dssl2H
      
      #δE
      dE18O<-(((data2$O18[i]/1000)-e18O)/a18O-rh*dAs18O-ek18O)/(1-rh+ek18O)
      dE2H<-(((data2$H2[i]/1000)-e2H)/a2H-rh*dAs2H-ek2H)/(1-rh+ek2H)
      results[i,7]<- dE18O
      results[i,8]<-dE2H
      
      ##slope and intercept
      #slope<-(dstar2H-dp2H)/(dstar18O-dp18O) ##PAD
      slope<-(dE2H-(data2$H2[i]/1000))/(dE18O-(data2$O18[i]/1000))
      int<-data2$H2[i]-slope*data2$O18[i]
      
      results[i,9]<- slope
      results[i,10]<- int
      
      #input water isotopic composition
      #dI18O<-((-19.2/1000)-(int))/(slope-6.7) ##PAD
      ##from Prarie potholes 
      dI18O<-(-20.75-int)/(slope-6.81)
      dI2H<-(slope*dI18O)+int
      results[i,11]<- dI18O
      results[i,12]<- dI2H
      
      ##E/I
      ei<-(dI18O-data2$O18[i])/((dE18O*1000)-data2$O18[i])
      
      results[i,13]<-ei
      
    }
    results<<-as.data.frame(results)
    #write.csv(results, file="framework_results_2020.csv")
    return(results)
  }

isotopic_framework(data)
write.csv(results, file="framework_results_2020_onlybriscorh.csv")

view(results)
results$Site.Number<-as.numeric(results$Site.Number)
colnames(results)[colnames(results) == "date"] <- "Sample.Date"

dI_data<-data


merged_df<- left_join(dI_data, results, by=c("Site.Number", "Sample.Date"))
merged_df$O18<-coalesce(as.numeric(merged_df$IO), merged_df$O18)

dI_data<-merged_df%>%dplyr::select(Site.Number,Site.Name, Type, Sample.Date, O18,H2,EC)

#set_seasons(data)
dI_data<-set_seasons(dI_data)
dI_data$Type[196]<-"L" ##fixing an error

plots<-list()
for (i in 1:nrow(results)){
  plot<-
  ggplot(data = results[i,])+
  geom_point(aes(x= as.numeric(starO)*1000, y =as.numeric(starH)*1000))+
  geom_point(aes(x= as.numeric(EO)*1000, y =as.numeric(EH)*1000), col="pink")+
  geom_abline(slope=8, intercept =10)+
  geom_point(aes(x= as.numeric(IO), y =as.numeric(IH)), col="blue")+
  geom_point(aes(x= as.numeric(sslO)*1000, y =as.numeric(sslH)*1000), col="orange")+
  geom_point(x=spring_riverO, y=spring_riverH, col="red")+
  geom_point(x=summer_riverO, y=summer_riverH, col="red")+
  geom_point(x=fall_riverO, y=fall_riverH, col="red")+
  geom_point(x=spring_gwO, y=spring_gwH, col="purple")+
  geom_point(x=summer_gwO, y=summer_gwH, col="purple")+
  geom_point(x=fall_gwO, y=fall_gwH, col="purple")+
  geom_text(aes(x= -40, y =-100, label= Site.Number))+
  geom_text(aes(x= -40, y =-125, label= date))+
  theme_bw()
  plots[[i]]<-plot
}

  
aes(label = cty)  

spring_gwO<-data %>% filter (Type == "G"& Season == "Spring")%>% .$O18 %>% mean()
spring_gwH<-data %>% filter (Type == "G"& Season == "Spring")%>% .$H2 %>% mean()
summer_gwH<-data %>% filter (Type == "G"& Season == "Summer")%>% .$H2 %>% mean()
summer_gwO<-data %>% filter (Type == "G"& Season == "Summer")%>% .$O18 %>% mean()
fall_gwO<-data %>% filter (Type == "G"& Season == "Fall")%>% .$O18 %>% mean()
fall_gwH<-data %>% filter (Type == "G"& Season == "Fall")%>% .$H2 %>% mean()

##function for running the mixing model ##need to move the dI data over to the data dataframe

mix_model<-
  function (data,s){ ##s=season

if (s=="Spring"){
  ECd<-EC_spring2
  Od<-c(Os_2020$O4, Os_2020$O5)
  Hd<-c(Hs_2020$H4, Hs_2020$H5)
 } else if (s=="Summer"){
    ECd<-EC_summer2
    Od<-c(Os_2020$O6, Os_2020$O7)
    Hd<-c(Hs_2020$H6, Hs_2020$H7)
} else if ( s=="Fall") {
    ECd<- EC_fall2
    Od<-c(Os_2020$O8, Os_2020$O9)
    Hd<-c(Hs_2020$H8, Hs_2020$H9)
  }

mix= matrix(c(data %>% filter ( Type == "W" &Season == s) %>% .$O18, 
              data %>% filter ( Type == "W" &Season == s) %>% .$H2,
              data %>% filter ( Type == "W" & Season == s) %>% .$EC), 
            ncol=3, nrow=nrow(data %>% filter ( Type == "W" & Season ==s)))
colnames(mix)= c("18O","2H", "EC")

s_names = c("River", "Groundwater", "Precipitation") ##only need to run once, same for all seasons
s_means = matrix(c(data %>% filter (Type == "R"& Season == s)%>% .$O18 %>% mean(),
                  data %>% filter ( Type == "G"& Season == s) %>% .$O18 %>% mean(),
                  Od %>% mean(na.rm=TRUE),
                  data %>% filter ( Type == "R" & Season == s) %>% .$H2 %>% mean(), 
                  data %>% filter ( Type == "G" & Season == s) %>% .$H2 %>% mean(),
                  Hd %>% mean(na.rm=TRUE), 
                  data %>% filter ( Type == "R" & Season == s) %>% .$EC %>% mean(),
                  data %>% filter ( Type == "G"& Season == s) %>% .$EC %>% mean(),
                  ECd[[3]]%>% mean()), ncol=3, nrow=3)
s_sds = matrix(c(data %>% filter (Type == "R"& Season == s)%>% .$O18 %>% sd(),
               data %>% filter ( Type == "G"& Season == s) %>% .$O18 %>% sd(),
               Od %>% sd(na.rm=TRUE),
               data %>% filter ( Type == "R"& Season == s) %>% .$H2 %>% sd(), 
               data %>% filter ( Type == "G"& Season == s) %>% .$H2 %>% sd(), 
               Hd %>% sd(na.rm=TRUE), 
               data %>% filter ( Type == "R" & Season == s) %>% .$EC %>% sd(),
               data %>% filter ( Type == "G"& Season == s) %>% .$EC %>% sd(),
               ECd[[3]]%>% sd()), ncol=3, nrow=3)

grp = as.integer(c(1:nrow(data %>% filter ( Type == "W" & Season == s))))

##run model

simmr_in = simmr_load(mixtures=mix,
                          source_names=s_names,
                          source_means=s_means,
                          source_sds=s_sds,
                          group=grp)



tracers12<-plot(simmr_in,tracers=c(1,2), groups=1:nrow(data %>% filter ( Type == "W" &Season == s)))
tracers13<- plot(simmr_in,tracers=c(1,3),groups=1:nrow(data %>% filter ( Type == "W" &Season == s)))
tracers23<-plot(simmr_in,tracers=c(2,3),groups=1:nrow(data %>% filter ( Type == "W" &Season == s)))

simmr_out = simmr_mcmc(simmr_in)
summary(simmr_out,type='diagnostics')
stats<-summary(simmr_out,type='statistics', group=1:nrow(data %>% filter ( Type == "W" &Season == s)))
stats<<-stats
file.name<-paste0(s,"_stats",".csv")
write.csv(stats, file = file.name)
return(stats)
  }

simmr_results_table<- function (ngroups) { ##this is where I am, need to make tables of the results that will be useable in the figure
  results_table=matrix(data=NA, ncol=6, nrow=ngroups)
  for (i in 1:ngroups) {
    results_table[i,1]= stats$statistics[[i]][[2,1]]
    results_table[i,2]= stats$statistics[[i]][[2,2]]
    results_table[i,3]= stats$statistics[[i]][[3,1]]
    results_table[i,4]= stats$statistics[[i]][[3,2]]
    results_table[i,5]= stats$statistics[[i]][[4,1]]
    results_table[i,6]= stats$statistics[[i]][[4,2]]
  }
  colnames(results_table)= c("river_mean","river_sd","gw_mean","gw_sd","precip_mean","precip_sd")
  return(results_table)
}

mix_model(dI_data, "Spring") ## mixture 5 is outside bounds, #24 & 2 are questionable-- lots of sites more enriched than precip
spring_results<-as.data.frame(simmr_results_table(ngroups=36))
spring_results$site_number<- dI_data %>% filter (Type == "W"& Season == "Spring")%>%.$Site.Number
#write.csv(spring_results, file="output/spring_results_proportions_dI.csv")

mix_model(dI_data, "Summer") ##mixture 6(site 35) has high EEC, the input water composition is now lower than the river and groundwater, so that needs to be addressed
summer_results<-as.data.frame(simmr_results_table(ngroups=36))
summer_results$site_number<- dI_data %>% filter (Type == "W"& Season == "Summer")%>%.$Site.Number
write.csv(summer_results, file="output/summer_results_proportions_dI.csv")

mix_model(dI_data, "Fall")
Fall_results<-as.data.frame(simmr_results_table(ngroups=34))
Fall_results$site_number<- dI_data %>% filter (Type == "W"& Season == "Fall")%>%.$Site.Number
write.csv(Fall_results, file="output/fall_results_proportions_dI.csv")

colnames(spring_results)<-c("river_mean","river_sd","gw_mean","gw_sd","precip_mean","precip_sd", "site_number")
colnames(summer_results)<-c("river_mean","river_sd","gw_mean","gw_sd","precip_mean","precip_sd", "site_number")
colnames(Fall_results)<-c("river_mean","river_sd","gw_mean","gw_sd","precip_mean","precip_sd", "site_number")
results_2020_dI<-list(spring_results, summer_results, Fall_results)
##are the input water compisitions too depleted? are they too conservative?


###------------making some plots----------

ggtern(data=spring_results,aes(x=mriver,y=mgw, z=mprecip, color= rowname)) +
  theme_bw()+
  theme_showarrows()+
  geom_point(aes(color = site_number), size=3) +
  geom_text(aes(x=mriver+0.04,y=mgw, z=mprecip),label=spring_results$site_number)+
  #geom_point(data=as.data.frame(p_post),aes(x=River,y=Groundwater, z=Precipitation))+
  #geom_mean_ellipse(data=as.data.frame(p_post1),aes(x=River,y=Groundwater, z=Precipitation))
  geom_errorbarL(aes(Lmin=mriver-riversd, Lmax=mriver+riversd, colour= site_number), alpha=0.3, lwd=0.5)+
  geom_errorbarT(aes(Tmin=mgw+gwsd,Tmax=mgw-gwsd,colour=site_number),alpha=0.3, lwd=0.5)+
  geom_errorbarR(aes(Rmin=mprecip+precipsd,Rmax=mprecip-precipsd,colour=site_number),alpha=0.03, lwd=0.5)+
  labs(x="River", xarrow= "",y="Groundwater", yarrow = "", z="Precipitation", zarrow="Precipitation")+
  theme(legend.position="bottom",
        legend.justification=c(0.5,0.5),
        legend.direction="horizontal",legend.box="horizontal",
        legend.box.just="top",legend.key.size = unit(0.5, "cm"))



tplot<-list()


for (i in 1:3){
  colors <- c(rgb2hex(
    r=round(results_2020_dI[[i]]$gw_mean*255, 0),
    g=round(results_2020_dI[[i]]$river_mean*255,0),
    b=round(results_2020_dI[[i]]$precip_mean*255,0)))
  names(colors) = c(results_2020_dI[[i]]$site_number)
  tern_plt<-
    ggtern(data=as.data.frame(results_2020_dI[[i]]),aes(x=river_mean,y=gw_mean, z=precip_mean, color=as.character(site_number))) +
    scale_color_manual(values=colors)+
    theme_bw()+
    theme_showarrows()+
    geom_point(size=7) +
    geom_text(aes(label = site_number, x=river_mean,y=gw_mean), size = 3, col="white")+
    labs(x="River", xarrow= "",y="Groundwater", yarrow = "", z="Rain", zarrow="")+
    theme(legend.position="none")
  
  file_name <- paste0("tern_plot", sns[i],"dI", ".pdf")
  #ggsave(plot=last_plot(), filename=file_name)
  tplot[[i]]<-tern_plt
}

terns_2020_dI<-grid.arrange(tplot[[1]], tplot[[2]], tplot[[3]], ncol=3)
ggsave(terns_2020_dI, file="terns_2020_dI.png")

