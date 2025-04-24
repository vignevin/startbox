## R6 tests

### R6 class definition
# see https://thinkr.fr/au-menu-du-jour-r6-partie-1/
# see https://linogaliana.gitlab.io/collaboratif/package.html

library(openxlsx2)
library(openxlsx)
library(dplyr)

# create R6 class to store data
user_data <- R6::R6Class(
  "UserData",
  public = list(
    # excel template path
    excel_model = NULL,
    # data_trial excel
    excel_data_trial =NULL,
    combined_data = NULL,
    #data.frame contenant les données de la feuille placette si elle est remplie 
    plot_desc = NULL,
    
    moda_desc = NULL,
    # obs_data is the list of dataframes with observation data. each list item has the name of the source file
    # if the item is a sheet of an excel file, the name will be filename_sheetname
    obs_data = list(),
    
    
    # initialize function to load the model excel workbook
    initialize = function(excel_model = NULL, excel_data_trial =NULL){
      self$excel_model <- excel_model
      self$excel_data_trial <- excel_data_trial
    },
    
    
    # Methode pour ajouter ou mettre a jour un element
    add_obs = function(name, df) {
      if (name %in% names(self$obs_data)) {
        message(paste("Mise à jour de l'élément :", name))
      } else {
        message(paste("Ajout d'un nouvel élément :", name))
      }
      
      # Ajouter les colonnes prov_name et prov_date
      df$prov_name <- as.character(name)
      df$prov_date <- format(Sys.Date(), "%d/%m/%Y")
      
      # Convertir colonnes *_PC en numeric même si c’est du texte
      pc_cols <- grep("_PC$", names(df), value = TRUE)
      df[pc_cols] <- lapply(df[pc_cols], function(col) as.numeric(as.character(col)))
      
      self$obs_data[[name]] <- df
    },
    
    # Méthode pour afficher les éléments
    show_obs_data = function() {
      lapply(self$obs_data, head)
    },
    
    combine_data_obs = function() {
      if (length(self$obs_data) == 0) {
        message("Aucune donnée à combiner.")
        return(NULL)
      }
      
      # Forcer les types de colonnes sensibles
      self$obs_data <- lapply(self$obs_data, function(df) {
        # Forcer *_PC en numeric
        pc_cols <- grep("_PC$", names(df), value = TRUE)
        df[pc_cols] <- lapply(df[pc_cols], function(col) as.numeric(as.character(col)))
        
        # Forcer prov_name / prov_date en character
        if ("prov_name" %in% names(df)) {
          df$prov_name <- as.character(df$prov_name)
        }
        if ("prov_date" %in% names(df)) {
          df$prov_date <- as.character(df$prov_date)
        }
        
        # DEBUG : voir les types si tu veux
        # print(str(df))
        
        return(df)
      })
      
      # Voir si une des colonnes est mal typée
      types <- sapply(self$obs_data, function(df) sapply(df, class))
      print(types["prov_name", ])
      
      
      # Combiner les données
      combined <- dplyr::bind_rows(lapply(names(self$obs_data), function(nom) {
        df <- self$obs_data[[nom]]
        return(df)
      }))
      
      # Colonnes principales du template (ordre souhaité)
      template_cols <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
      # Colonnes communes à reorder
      valid_cols <- intersect(template_cols, names(combined))
      # Réordonner les colonnes principales + garder les autres à droite
      ordered_cols <- c(valid_cols, setdiff(names(combined), valid_cols))
      # Réordonner le data.frame sans rien perdre
      combined <- combined[, ordered_cols, drop = FALSE]
      
      
      # Si le fichier excel_data_trial n'existe pas encore, on copie excel_model
      if (is.null(self$excel_data_trial) || !file.exists(self$excel_data_trial)) {
        message("Copie du modèle vierge dans excel_data_trial")
        trial_path <- tempfile(fileext = ".xlsx")
        file.copy(from = self$excel_model, to = trial_path, overwrite = TRUE)
        self$excel_data_trial <- trial_path
      }
      
      # Vérifier si une feuille 'placette' existe et est remplie
      if (!is.null(self$excel_data_trial) && file.exists(self$excel_data_trial)) {
        wb_trial <- wb_load(self$excel_data_trial)
        
        if ("placette" %in% wb_trial$sheet_names) {
          placette_data <- wb_read(wb_trial, sheet = "placette")
          
          # Supprimer les lignes entièrement vides
          placette_data <- placette_data[rowSums(is.na(placette_data) | placette_data == "") != ncol(placette_data), ]
          
          if (nrow(placette_data) > 0) {
            message("✅ Données de la feuille 'placette' chargées dans plot_desc.")
            self$plot_desc <- placette_data
          } else {
            message("ℹ️ Feuille 'placette' trouvée mais vide.")
          }
        } else {
          message("ℹ️ Feuille 'placette' absente du fichier Excel.")
        }
      }
      
      # Vérifier si une feuille 'modalite' existe et est remplie
      if ("modalite" %in% wb_trial$sheet_names) {
        modalite_data <- wb_read(wb_trial, sheet = "modalite")
        
        # Supprimer les lignes entièrement vides
        modalite_data <- modalite_data[rowSums(is.na(modalite_data) | modalite_data == "") != ncol(modalite_data), ]
        
        if (nrow(modalite_data) > 0) {
          message("✅ Données de la feuille 'modalite' chargées dans moda_desc.")
          self$moda_desc <- modalite_data
        } else {
          message("ℹ️ Feuille 'modalite' trouvée mais vide.")
        }
      } else {
        message("ℹ️ Feuille 'modalite' absente du fichier Excel.")
      }
      
      
      
      # Charger le fichier avec openxlsx2
      wb <- openxlsx2::wb_load(self$excel_data_trial)
      
      # Supprimer la feuille "data" si elle existe
      if ("data" %in% wb$sheet_names) {
        
        old_data <- wb_read(wb, sheet = "data")
        
        # Harmonisation générale des colonnes sensibles
        columns_to_character <- c("prov_name", "prov_date", "observation_date", "bbch_stage", "plot_id")
        
        for (col in columns_to_character) {
          if (col %in% names(old_data)) {
            old_data[[col]] <- as.character(old_data[[col]])
          }
          if (col %in% names(combined)) {
            combined[[col]] <- as.character(combined[[col]])
          }
        }
        
        
        
        
        # 🔢 Conversion des colonnes *_PC en numeric
        pc_cols <- grep("_PC$", names(old_data), value = TRUE)
        old_data[pc_cols] <- lapply(old_data[pc_cols], function(col) as.numeric(as.character(col)))
        
        combined <- dplyr::bind_rows(old_data, combined)
        print(combined)
        
        wb$remove_worksheet("data")
      }
      
      # Ajouter une nouvelle feuille "data"
      wb$add_worksheet("data")
      
      # Écrire les données combinées sous forme de tableau structuré
      wb$add_data_table(sheet = "data", x = combined)
      
      # Définir la feuille active à l'ouverture
      wb$set_active_sheet("data")
      
      # Sauvegarder le fichier
      wb$save(self$excel_data_trial)
      message("Les données combinées ont été insérées dans la feuille 'data'.")
      
      # Ouvrir automatiquement le fichier Excel généré
      if (Sys.info()["sysname"] == "Windows") {
        shell.exec(self$excel_data_trial)
      } else if (Sys.info()["sysname"] == "Darwin") { # macOS
        system(paste("open", shQuote(self$excel_data_trial)))
      } else { # Linux
        system(paste("xdg-open", shQuote(self$excel_data_trial)))
      }
      
      self$combined_data <- combined
      
      return(combined)
    },
    
    get_modalite_description = function() {
      if (is.null(self$plot_desc) || is.null(self$moda_desc)) {
        message("❌ plot_desc ou moda_desc manquant.")
        return(NULL)
      }
      
      # Étape 1 — Jointure placette + modalité
      self$plot_desc$factor_level_code <- as.character(self$plot_desc$factor_level_code)
      self$moda_desc$xp_trt_code <- as.character(self$moda_desc$xp_trt_code)
      
      df_plot_moda <- dplyr::left_join(
        self$plot_desc,
        self$moda_desc,
        by = c("factor_level_code" = "xp_trt_code")
      )
      
      # Ajout explicite de xp_trt_code
      df_plot_moda$xp_trt_code <- df_plot_moda$factor_level_code
      
      # Étape 2 — Ajouter les données d'observation
      if (length(self$obs_data) == 0) {
        message("⚠️ Aucune donnée d'observation à joindre. Retour uniquement plot + modalité.")
        return(df_plot_moda)
      }
      
      if (is.null(self$combined_data)) {
        message("❌ Les données combinées ne sont pas encore prêtes. Appelle d'abord combine_data_obs().")
        return(NULL)
      }
      
      df_obs <- self$combined_data
      df_obs$plot_id <- as.character(df_obs$plot_id)
      df_plot_moda$plot_id <- as.character(df_plot_moda$plot_id)
      
      # Fusion finale des observations + placette + modalité
      df_final <- dplyr::left_join(df_obs, df_plot_moda, by = "plot_id")
      
      return(df_final)
    }
    
  )
)


### creation d'une instance de classe mydata
mydata <- user_data$new(excel_model="inst/extdata/template.xlsx")
mydata <- user_data$new(excel_data_trial = "C:/Users/hmaire.VIGNEVIN/OneDrive - IFV/Documents/App Rshiny (Hervé)/template_rempli.xlsx") #Le fichier avec déjà des données à l'intérieur
mydata$excel_model

### ajout d'un dataframe data observation
# lecture data frame
# ce sera effectue par une fonction shiny
myfilepath = "misc/teissonniere_dataF1_2024.csv"
data1 <- read.csv2(myfilepath)

# ajouter le data frame
mydata$add_obs(name=basename(myfilepath),df=data1)

myfilepath2 = "misc//teissonniere_dataF2_2024.csv"
data2 <- read.csv2(myfilepath2)

# ajouter le data frame
mydata$add_obs(name=basename(myfilepath2),df=data2)

myfilepath3 = "misc//teissonniere_dataF3_2024.csv"
data3 <- read.csv2(myfilepath3)
mydata$add_obs(name=basename(myfilepath3),df=data3)

mydata$show_obs_data()

mydata$excel_data_trial

### fonctions a travailler
# 1. combiner des observations dans un seul fichier
# 2. exporter dans un fichier exCel modele si excel_data_trial est null

# 3. ajout d'un fichier excel modele
# 4. s'il existe, et si la feuille placette existe, importer la feuille placette dans plot_desc


#---- Combiner des observations dans un seul fichier ----

mydata$combine_data_obs()

#---- 

mydata$plot_desc
mydata$moda_desc


df_complet <- mydata$get_modalite_description()
View(df_complet)



