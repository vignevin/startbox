## R6 tests

### R6 class definition
# see https://thinkr.fr/au-menu-du-jour-r6-partie-1/
# see https://linogaliana.gitlab.io/collaboratif/package.html


# create R6 class to store data
user_data <- R6::R6Class(
  "UserData",
  public = list(
    # excel template path
    excel_model = NULL,
    # data_trial excel
    excel_data_trial =NULL,
    plot_desc = NULL,
    moda_desc = NULL,
    # obs_data is the list of dataframes with observation data. each list item has the name of the source file
    # if the item is a sheet of an excel file, the name will be filename_sheetname
    obs_data = list(),
    # initialize function to load the model excel workbook
    initialize = function(excel_model){
      self$excel_model <- excel_model
    },
    # Methode pour ajouter ou mettre a jour un element
    add_obs = function(name, df) {
      if (name %in% names(self$obs_data)) {
        message(paste("Mise à jour de l'élément :", name))
      } else {
        message(paste("Ajout d'un nouvel élément :", name))
      }
      self$obs_data[[name]] <- df
    },

    # Méthode pour afficher les éléments
    show_obs_data = function() {
      print(self$obs_data)
    }
  )
)


### creation d'une instance de classe mydata
mydata <- user_data$new(excel_model="inst//extdata//template.xlsx")
mydata$excel_model

### ajout d'un dataframe data observation
# lecture data frame
# ce sera effectue par une fonction shiny
myfilepath = "misc//teissonniere_dataF1_2024.csv"
data1 <- read.csv2(myfilepath)

# ajouter le data frame
mydata$add_obs(name=basename(myfilepath),df=data1)

myfilepath2 = "misc//teissonniere_dataF2_2024.csv"
data2 <- read.csv2(myfilepath2)

# ajouter le data frame
mydata$add_obs(name=basename(myfilepath2),df=data2)


mydata$show_obs_data()


### fonctions a travailler
# 1. combiner des observations dans un seul fichier
# 2. exporter dans un fichier exel modele si excel_data_trial est null

# 3. ajout d'un fichier excel modele
# 4. s'il existe, et si la feuille placette existe, importer la feuille placette dans plot_desc



