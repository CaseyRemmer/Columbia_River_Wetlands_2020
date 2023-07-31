require(ggplot2)
require(tidyverse)
require(dplyr)
require(rgdal)
require(sf)
require(raster)
require(magrittr)


##2020 data
setwd("C:/Users/Casey/Dropbox/columbia river delta/2020")
dt_2020<-read.csv("CRW_2020_raw data master_SB checked.csv")
head(dt_2020)

## EDA on data 
dt_2020$Sample.Date<-as.Date(dt_2020$Sample.Date, format= "%Y-%m-%d")
class(dt_2020$Sample.Date)

## LEL plots

limitO<--0.551126046
limitH<--44.92532454
sslO<--6.9
sslH<--79.22847312
pO<--19.38
pH<--146.8
asO<--21.46091285
asH<--164.9161743

dataO<-c(limitO,pO,sslO,asO)
dataH<-c(limitH,pH,sslH,asH)
lel<-cbind(dataO,dataH)
lel<-as.data.frame(lel)

## basic delta-delta plot with LEL
dd_plt<-
  ggplot(dt_2020)+
  geom_point(data=lel, aes(x=dataO, y=dataH), size=2)+
  xlab(expression(paste(delta^{18}, "O (\u2030 VSMOW)")))+xlim(c(-25, 5))+
  ylab(expression(paste(delta^{2}, "H(\u2030 VSMOW)")))+ylim(c(-180,-40))+
  geom_line(data=lel[1:2,],aes(x=dataO, y=dataH))+
  geom_abline(intercept=10,slope=8)+
  theme_minimal()


##adding in a categorical variable called "Season" to make subsetting the data smoother

for (i in 1:nrow(dt_2020)){
 if (dt_2020$Sample.Date[i] >= "2020-04-01" & dt_2020$Sample.Date[i] <= "2020-05-31"){
   dt_2020$Season[i]="Sp"}
  else if (dt_2020$Sample.Date[i] >= "2020-06-01" & dt_2020$Sample.Date[i] <= "2020-07-31"){
     dt_2020$Season[i]="Sm"} 
  else if (dt_2020$Sample.Date[i] >= "2020-08-01"){
       dt_2020$Season[i]="F"}
}
view(dt_2020)
write.csv(dt_2020, "data_2020_R_checkpoint1.csv")

##adding to the plot

dd_plt3<-
  dd_plt +geom_point(data=dt_2020, aes(x=O18, y=H2, fill=Type, color=Type), size=3, alpha=0.5)+
  facet_wrap(~factor(Season,levels=c("Sp", "Sm", "F")), nrow=3)+
  scale_fill_hue()+
  scale_color_discrete()

require(ggpubr)
ggsave(plot=last_plot(), filename = "delta_plots.pdf", height=10, width=7)

##importing the precipitation data-- this data does not change inter-annually
##because it is sourced externally from the OIPC & NEON

O4 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O04c.asc") #april
O5 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O05c.asc") #may
O6 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O06c.asc") #june
O7 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O07c.asc") #july
O8 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O08c.asc") #august
O9 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O09c.asc") #Sept
bigO<-list(O4,O5,O6,O7,O8,O9)
H4 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H04c.asc") #april
H5 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H05c.asc") #may
H6 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H06c.asc") #june
H7 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H07c.asc") #july
H8 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H08c.asc") #august
H9 <- raster("C:/Users/Casey/Dropbox/columbia river delta/GIS/IsotopeMaps/H09c.asc") #Sept
bigH<-list(H4,H5,H6,H7,H8,H9)

cond_NEON <- read.csv("C:/Users/Casey/Dropbox/columbia river delta/conductivity/NEON cond data for mixing model_csv.csv") 

### Need the site locations to be able to find the precipitation values

old_sites<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/GIS/GIS coordinates for all CRD sites.csv")

colnames(old_sites)[1] <- 'Site.Number'

dt_2020_grnd<-dt_2020 %>% left_join( old_sites, by='Site.Number', keep= NULL)
head(dt_2020_grnd)

dt_2020_grnd %>% filter(is.na(Easting)) 
dt_2020_grnd %>% filter(Site.Number == 43)
##86 is 142 

##entering some missing values from above manually

if (dt_2020_grnd$Site.Number==1){
  dt_2020_grnd$Easting<-553002
  dt_2020_grnd$Northing<-5630511
} 

if (dt_2020_grnd$Site.Number== 142){
  dt_2020_grnd$Site.Number== 86
}

write.csv(dt_2020_grnd %>% filter(is.na(Easting)), "Sites_without_locations.csv")

####temporarily remove the sites with NA so the model can run
dt_2020_grnd<- dt_2020_grnd%>% filter(!is.na(Easting))

dt_2020_grnd<-dt_2020_grnd %>% st_as_sf(coords=c("Easting","Northing")) %>% st_set_crs("+proj=utm +zone=11 +datum=WGS84")
dt_2020_grnd<-st_transform(dt_2020_grnd, crs(O4))
extent(dt_2020_grnd)

crop_iso<-
  function(x,y){
  for (i in 1:length(x)){
    b<-crop(x[[i]],extent(y))
    x[[i]]<-b
  }
crop_result<<-x
    }

crop_iso(bigO,dt_2020_grnd)
 O<-crop_result ##have to save the function output

crop_iso(bigH,dt_2020_grnd)
H<-crop_result

##extracting values at the sites-----------------------------------

precip_extraction<-function(x,y){
  c<-NA
  for (i in 1:2){
   b<-raster::extract(x[[i]], y)
   c<-append(c,b)
   sp<-mean(c, na.rm=TRUE)
   sp.sd<-sd(c,na.rm=TRUE)
  }
  c<-NA
for (i in 3:4){
      b<-raster::extract(x[[i]], y)
      c<-append(c,b)
      sm<-mean(c,na.rm=TRUE)
      sm.sd<-sd(c,na.rm=TRUE)
}
  c<-NA
  for (i in 5:6){
    b<-raster::extract(x[[i]], y)
    c<-append(c,b)
    f<-mean(c,na.rm=TRUE)
    f.sd<-sd(c,na.rm=TRUE)
  }
result.mean<<-c(sp,sm,f)
result.sd<<-c(sp.sd,sm.sd,f.sd)
result.mean<-result.mean %>% as.data.frame()
result.sd<-result.sd %>% as.data.frame()
row.names(result.mean)<-c("Sp", "Sm", "F")
row.names(result.sd)<-c("Sp.sd", "Sm.sd", "F.sd")
output.mean<<-result.mean
output.sd<<-result.sd
  }

precip_extraction(O,dt_2020_grnd)
precip_mean_O<-output.mean
precip_sd_O<-output.sd

precip_extraction(H,dt_2020_grnd)
precip_mean_H<-output.mean
precip_sd_H<-output.sd
##------------------------
##Conductivity for the model

cond_NEON$date<-as.Date(cond_NEON$date, format = "%Y-%m-%d") ##converting dates from factor to dates 
cond_NEON_Sp<-subset(cond_NEON, date > "2019-04-01" & date < "2019-05-31")
cond_NEON_S<-subset(cond_NEON, date > "2019-06-01" & date < "2019-07-31")
cond_NEON_F<-subset(cond_NEON, date > "2019-08-01" & date < "2019-09-30")

EC_dt<-list(cond_NEON_Sp,cond_NEON_S,cond_NEON_F)
names(EC_dt)<-c("Sp", "Sm", "F")

### settign up to run the model now-----


library(simmr)
require(R2jags)
sns<-c("Sp", "Sm", "F")

results_2020<-list() ##make sure this is an emply list to begin

for (i in 1:3){
W_dt<-dt_2020 %>% filter (Season == sns[i] & Type == "W")
R_dt<-dt_2020 %>% filter (Season == sns[i] & Type == "R")
G_dt<-dt_2020 %>% filter (Season == sns[i] & Type == "G")


mix= matrix(c(W_dt$O18, W_dt$H2, W_dt$EC), ncol=3, nrow=nrow(W_dt))
colnames(mix)= c("O18","H2", "EC")

s_names = c("River", "Groundwater", "Precipitation") ##only need to run once, same for all seasons
s_means = matrix(c(mean(R_dt$O18), mean(G_dt$O18), precip_mean_O[i,],  
                      mean(R_dt$H2), mean(G_dt$H2), precip_mean_H[i,], 
                      mean(R_dt$EC), mean(G_dt$EC), mean(EC_dt[[i]][,3])), 
                      ncol=3, nrow=3)

s_sds = matrix(c(sd(R_dt$O18), sd(G_dt$O18),precip_sd_O[i,],
                    sd(R_dt$H2), sd(G_dt$H2), precip_sd_H[i,] ,
                    sd(R_dt$EC), sd(G_dt$EC), sd(EC_dt[[i]][,4])), 
                    ncol=3, nrow=3)
grp = as.integer(c(1:nrow(W_dt)))


simmr_ = simmr_load(mixtures=mix,
                          source_names=s_names,
                          source_means=s_means,
                          source_sds=s_sds,
                          group = grp)

file_name <- paste0("tracer_plot_12", sns[i], ".jpeg")
jpeg(file=file_name)
plot(simmr_,tracers=c(1,2))
dev.off()

file_name <- paste0("tracer_plot_13", sns[i], ".jpeg")
jpeg(file=file_name)
plot(simmr_,tracers=c(1,3))
dev.off()

file_name <- paste0("tracer_plot_23", sns[i], ".jpeg")
jpeg(file=file_name)
plot(simmr_,tracers=c(2,3))
dev.off()

simmr_out = simmr_mcmc(simmr_)

stats<-summary(simmr_out,type='statistics', group=1:nrow(W_dt))
river=matrix(data=NA, ncol=2, nrow=nrow(W_dt))
  for (p in 1:nrow(W_dt)){
        river[p,] = stats$statistics[[p]][2,]
  }
row.names(river)<-W_dt$Site.Number
colnames(river)<-c("river_mean", "river_sd")
gw=matrix(data=NA, ncol=2, nrow=nrow(W_dt))
for (p in 1:nrow(W_dt)){
  gw[p,] = stats$statistics[[p]][3,] 
}
row.names(gw)<-W_dt$Site.Number
colnames(gw)<-c("gw_mean", "gw_sd")
precip=matrix(data=NA, ncol=2, nrow=nrow(W_dt))
for (p in 1:nrow(W_dt)){
  precip[p,] = stats$statistics[[p]][4,] 
}
row.names(precip)<-W_dt$Site.Number
colnames(precip)<-c("precip_mean", "precip_sd")

run_results<-cbind(river,gw,precip)

results_2020[[i]]<-run_results

}

results_2020
for(i in 1:3){
results_2020[[i]]<-results_2020[[i]]%>% as.data.frame%>%rownames_to_column()
}

##making plots

require(ggtern)

tplots<-list()

for (i in 1:3){
tern_plt<-
  ggtern(data=as.data.frame(results_2020[[i]]),aes(x=river_mean,y=gw_mean, z=precip_mean, color= rowname)) +
  theme_bw()+
  theme_showarrows()+
  geom_point(size=3) +
  geom_text(aes(label = rowname, x=river_mean,y=gw_mean+0.075, z=precip_mean), size = 3)+
  labs(x="", xarrow= "River",y="", yarrow = "Groundwater", z="", zarrow="Precipitation")+
  theme(legend.position="none")

file_name <- paste0("tern_plot", sns[i], ".pdf")
#ggsave(plot=last_plot(), filename=file_name)
tplots[i]<-tern_plt
}

plts3<-
  grid.arrange(tern_plt1, tern_plt2, tern_plt3, ncol=3, top="Spring--> Summer --> Fall")

ggsave(plot=plts3, filename="3 season tern plots.pdf")
