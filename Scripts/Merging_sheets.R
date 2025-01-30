library(tidyr)
library(dplyr)

specimen.data<- read.csv("Data_backup/Combined Dataset_specimen_info_2025_01_21_withPredictorVariables.csv")


data<- read.csv("Data_backup/Full_Stripes Datasheet_24_05_15_withPredictorVariables.csv")


# Vertical_stripes

vertical.stripe<- read.csv("Data_backup/Stripes Datasheet_stripes_vertical_24_05_15.csv") 
vertical.stripe<- vertical.stripe %>% slice(1:673) %>% select(1:7)
vertical.stripe <- vertical.stripe %>% drop_na(ï..specimen_id)


vertical.stripe$stripe_distinctness_numerical<-ifelse(vertical.stripe$stripe_distinctness=="no",0,1)
vertical.stripe.by<-by(vertical.stripe$stripe_distinctness_numerical, vertical.stripe$ï..specimen_id, mean)
vertical.stripe.by<-as.data.frame(vertical.stripe.by)

vertical.stripe.by <- cbind(vertical.stripe.by)
vertical.stripe.by <- cbind(rownames(vertical.stripe.by), data.frame(vertical.stripe.by, row.names=NULL))
vertical.stripe.by<-rename(vertical.stripe.by, "ï..specimen_id"="rownames(vertical.stripe.by)","percent_distinct"="vertical.stripe.by")


new.data<- full_join(new.data, vertical.stripe.by, by = "ï..specimen_id")


write.csv(new.data, file="Data_backup/Full_Stripes Datasheet_24_05_15_withPredictorVariables.csv")
