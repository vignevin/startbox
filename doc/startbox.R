## -----------------------------------------------------------------------------
#| label: setup
library(startbox)


## -----------------------------------------------------------------------------
#| label: get_template_excel
#| include: false

startbox::get_template_excel()



## -----------------------------------------------------------------------------
# Cas 1 : création d'un objet vide
mydata <- user_data$new()

# Cas 2 : création d'un objet avec fichier Excel existant
mydata2 <- user_data$new(trial_file = system.file("extdata","standard_exemple.xlsx",package="startbox"))


## -----------------------------------------------------------------------------
load_metadata_sheets(mydata2)


## -----------------------------------------------------------------------------
load_data_sheets(mydata2)



## -----------------------------------------------------------------------------
wrapper_data(mydata2)


## -----------------------------------------------------------------------------
# Exemple : chargement d'un fichier CSV local
myfilepath <- system.file("extdata","teisso_2024_dataF2.csv",package="startbox")
data1 <- read.csv2(myfilepath)

# Ajout direct dans l'objet user_data avec le nom du fichier
mydata2$add_obs(name = basename(myfilepath), df = data1)


## -----------------------------------------------------------------------------
#| label: load_obs

file_path <- system.file("extdata","maladie_teisso_2024_11h12min_F1_17.06.csv",package="startbox")

# verification si le fichier est issu de topvigne
check_topvigne_csv(filepath = file_path)


## -----------------------------------------------------------------------------
# importation 
data_F1 <- standardise_topvigne_csv(file_path)
head(data_F1)


## -----------------------------------------------------------------------------
# Ajout direct dans l'objet user_data avec le nom du fichier
mydata2$add_obs(name = "F1", df = data_F1)


## -----------------------------------------------------------------------------
#| include: false
#| 
export_data_sheets(mydata2)


## -----------------------------------------------------------------------------
data_F1 <- mydata2$obs_data$F1
head(data_F1)


## -----------------------------------------------------------------------------
# recuperation des codes des traitements dans la colonne xp_trt_code
data_F1$xp_trt_code <- remove_string_pairwise(vec=data_F1$plot_id,
                                              pattern = data_F1$plot_block)

head(data_F1)


## -----------------------------------------------------------------------------

obs_moda <- c(0,0,0,0,10,5,0,2,0,0,0,15,20,50)
obs_tnt <- c(10,20,60,0,100,50,0,20,0,10,20,15,2,50)


incidence(obs_moda)
intensity(obs_moda)
severity_diseased(obs_moda)

# l'efficacité est calculée en comparant une valeur dans une modalité à la valeur de la même variable obtentue dans le témoin non traité
efficacy(incidence(obs_moda),
         value_tnt = incidence(obs_tnt))


## -----------------------------------------------------------------------------
#| lable: load_obs

F_I_placette <- resume_data(data_F1,
                            var_col = "PM_LEAF_PC", 
                            group_cols = c("plot_id","xp_trt_code","plot_block"),
                            funs = list(intensite = intensity, frequence = incidence))
head(F_I_placette)


## -----------------------------------------------------------------------------
head(resume_pivot_wider(F_I_placette))


## -----------------------------------------------------------------------------
F_moda <- resume_data(F_I_placette %>% dplyr::filter(calculation == "frequence PM_LEAF_PC"),
                      var_col = "value",
                      group_cols = c("xp_trt_code"),
                      funs = list(moyenne = mean, ecart_type = sd))
head(resume_pivot_wider(F_moda))


## -----------------------------------------------------------------------------
eff_par_placette <- resume_data(F_I_placette,
            var_col = "value",
            group_cols = c("plot_id"),
            funs=list(efficacite=efficacy))
head(eff_par_placette)



## -----------------------------------------------------------------------------
df_plot_tnt <- data.frame(tnt_id=rep(paste0("TNT",1:4),11),
                          plot_id = sample(unique(F_I_placette$plot_id)))
df_plot_tnt %>%
  dplyr::mutate(tnt_id = dplyr::case_when(
    plot_id == "TNT1" ~ "TNT1",
    plot_id == "TNT2" ~ "TNT2",
    plot_id == "TNT3" ~ "TNT3",
    plot_id == "TNT4" ~ "TNT4",
    TRUE ~ tnt_id,
  )) -> df_plot_tnt

# visualisation de la table d'association
head(df_plot_tnt)


## -----------------------------------------------------------------------------
# calcul de l'efficacité en tenant compte de l'association de chaque placette avec son TNT
eff_placette_tnt <- resume_data(F_I_placette,
            var_col = "value",
            group_cols = c("plot_id"),
            funs=list(efficacite=efficacy),
            df_tnt = df_plot_tnt)
head(eff_placette_tnt)


## -----------------------------------------------------------------------------
## tableau bilan sur la fréquence
F_plot <- resume_data(data_F1,
                            var_col = "PM_LEAF_PC", 
                            group_cols = c("plot_id","xp_trt_code","plot_block"),
                            funs = list(intensité = intensity))

test_stats(data = F_plot, value_col = "value")


## -----------------------------------------------------------------------------

df_complet <- merge(F_plot, mydata2$metadata$moda_desc, all.x=T)
df_complet <- merge(df_complet, mydata2$metadata$plot_desc, all.x=T)
print(df_complet)


## -----------------------------------------------------------------------------
plot_xpbar(df_complet,xcol="xp_trt_code",ycol="value",fillcol="calculation",show_errorbar = T,y=unique(df_complet$calculation),x="Modalité")


## -----------------------------------------------------------------------------
plot_xpheat(df_complet, variable = "value", caption = "IFV")


## -----------------------------------------------------------------------------
library(dplyr)

plot_xpbox(df_complet, calculation_type = "intensité", show_dots = TRUE)

