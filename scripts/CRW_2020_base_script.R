
# 2020 Columbia river Wetlands water isotopes and conductivity
require(tidyverse)
require(dplyr)

data<-read.csv("C:/Users/Casey/Dropbox/columbia river delta/Columbia_River_Wetlands_2020/clean data/CRW_2020_cleaned_data_for_analysis.csv")
head(data)

#comparing the small creeks, large creeks and rivers

cr_rv<- data %>% dplyr::filter(Type == "sCR" | Type == "lgCR" | Type == "R")
view(cr_rv)
