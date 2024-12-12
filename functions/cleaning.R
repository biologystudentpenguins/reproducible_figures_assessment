     ## contains functions for cleaning the data ##


# A function to shorten the species names:

shorten_species <- function(penguins_data) {
  penguins_data %>%
    mutate(species = case_when(
      species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
      species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
      species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
    ))
}


#A function to clean the raw data:

cleaning_data <- function(data_raw){
  data_raw %>%
    clean_names() %>%   #applies consistent formating, readable by humans and computer, to column names
    shorten_species() %>%
    remove_empty(c("rows", "cols")) %>%  #removes completely empty rows and columns
    select(-comments) #remove the "comments" column
}


#A function that (i) removes rows where there's an "NA" value in the "flipper_length_mm" or "body_mass_g" column, then (ii) only keeps the "flipper_length_mm" and "body_mass_g" columns as part of the data frame:

remove_empty_flipper_mass <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm)) %>%
    filter(!is.na(body_mass_g)) %>%
    select(flipper_length_mm, body_mass_g)
}

