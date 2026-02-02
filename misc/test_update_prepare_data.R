library(startbox)
test_file <- "..//beta users//xavier//data_problad2025_v2.xlsx"
mydata <- load_excel_file(test_file)

## resolve vars
mydata$obs_data$data_G1$UN_LEAF_PC <- rep(0.1,length=nrow(mydata$obs_data$data_G1))
mydata$obs_data$data_G1$UN_LEAF_PC <- as.character(mydata$obs_data$data_G1$UN_LEAF_PC)
resolve_var_cols(data=mydata$obs_data$data_G1,var_cols = c("UN_LEAF_PC","UN_BER_PC"))


prepare_data(mydata, df="data_G3", prep_name = "default_G3_filtered",
             filters = list(xp_trt_name = c("1N-2V","1N-1V"), block_code = c("1","2")))
unique(mydata$prepared_data$default_G3_filtered$xp_trt_name)
unique(mydata$prepared_data$default_G3_filtered$block_code)
unique(mydata$prepared_data$default_G3$block_code)

## check what happens if no TNT found
prepare_data(mydata, df="default_G3_filtered", prep_name = "tests",
             funs=list(eff = efficacy))

## check what happens if direct efficacy calculation
prepare_data(mydata, df="data_G3", prep_name = "test2",
             tnt_mode = "block",
             funs=list(eff = efficacy))
summary(mydata$prepared_data$test2)
library(ggplot2)
ggplot(mydata$prepared_data$test2, aes(xp_trt_name,value)) +
  geom_boxplot()+
  facet_wrap(~calculation)


get_tnt_ids_from_association(mydata,tnt_mode = "block")

prepare_data(mydata,
             df="default_G3", # le nom du dataframe à préparer
             prep_name = "eff_G3_b", # nom du dataframe en sortie
             funs = list(efficacité = efficacy),  # calcul de l'efficacité
             tnt_mode = "block")
head(mydata$prepared_data$eff_G3_b)

mydata$prepared_data$default_G3 %>%
  group_by(xp_trt_name,calculation) %>%
  summarize(mean=mean(value, na.rm=T)) %>%
  filter(xp_trt_name=="TNT")

source("misc/update_prepare_data.R")

extract_tnt_from_metadata(self=mydata,code_tnt = "TNT")
prepare_tnt_association(self=mydata,tnt_mode="all",code_tnt = "TNT")

obsnames(mydata)
prepare_data(mydata, df="data_G3", prep_name = "default_G3")
prepare_data(mydata,
             df="default_G3", # le nom du dataframe à préparer
             prep_name = "eff_G3", # nom du dataframe en sortie
             funs = list(efficacité = efficacy),  # calcul de l'efficacité
             tnt_mode = "block")
mydata$prepared_data$eff_G3
test_stats(mydata, prep_data = "eff_G3")
plot_xpbar(mydata,stat = "eff_G3")


prepare_data(mydata,df="data_G3",prep_name="G3")
test_stats(mydata,prep_data = "G3",block = T)
plot_xpbar(mydata,stats="G3")
plot_xpheat(mydata,stats="G3",calculation_choices = "frequence UN_BER_PC",resids = T)


prepare_data(mydata, df="data_G3",
             funs = list(F0=incidence,
                         F10= function(x) {incidence(x,threshold=10)},
                         F40= function(x) {incidence(x,threshold=40)},
                         F60= function(x) {incidence(x,threshold=60)}),
             filters = list(xp_trt_name=c("1N-1V","1N-2V","1N-4V","TNT")),
             flex=T,
             prep_name = "F")

test_stats(mydata, prep_data = "F")
plot_xpbar(mydata,stats = "F", one_plot = F)
plot_xpheat(mydata,stat= "F")



prepare_data(mydata, df="data_G3",
             funs = list(IA=intensity,
                         F40= function(x) {incidence(x,threshold=40)}),
             filters = list(xp_trt_name=c("1N-1V","1N-2V","1N-4V","TNT")),
             flex=F,
             prep_name = "volume_IA")
prepare_data(mydata, df="volume_IA", funs = list(efficacité = efficacy),
             tnt_mode = "nearest",
             code_tnt = "TNT",
             prep_name = "eff")

#code_tnt <- "TNT"
#mydata$prepared_data$eff %>%
#  dplyr::filter(dplyr::if_any(dplyr::everything(), ~ grepl(code_tnt, .)))

summary(mydata$prepared_data$eff)
head(mydata$prepared_data$eff)
unique(mydata$prepared_data$eff$xp_trt_code)

library(ggplot2)
ggplot(mydata$prepared_data$test2, aes(xp_trt_name,value)) +
  geom_boxplot()+
  facet_wrap(~calculation)

test_stats(mydata,prep_data = "eff")
plot_xpbar(mydata,stats = "eff")


### test avec interne
# Création d'un objet avec fichier Excel existant
# ici le fichier Excel est un fichier exemple fourni avec le package
mydata <- user_data$new(trial_file = system.file("extdata","standard_exemple.xlsx",package="startbox"))
prepare_data(mydata, df="data_G1", prep_name = "default_G1")
prepare_data(mydata,
             df="default_G1", # le nom du dataframe à préparer
             prep_name = "eff_G1", # nom du dataframe en sortie
             funs = list(efficacité = efficacy),  # calcul de l'efficacité
             tnt_mode = "block")
test_stats(mydata, prep_data = "eff_G1")
plot_xpbar(mydata,stat = "eff_G1")
plot_meteo(mydata)
