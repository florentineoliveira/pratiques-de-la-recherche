# ------------------------------------------------------------------------------------
#       Lecture 3 : Régression linéaire simple et Moindres Carrés Ordinaires
# ------------------------------------------------------------------------------------

# Ce code simule une base de données de 100 000 familles ayant les mêmes caractéristiques que l'échantillon dans Black et al. 2005
# https://watermark.silverchair.com/120-2-669.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA2YwggNiBgkqhkiG9w0BBwagggNTMIIDTwIBADCCA0gGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMRUFpt9yUxU8Wuu4FAgEQgIIDGVcdvdpNZkmD_Fbj4_wa9HaGQTAoM2tQsF2wuRUmWvvlH23Fhxp68TwZUK_P2MpI1e4pA0PyMp-QAURQMsN_P7FcQuEoYqMkBs_7l-8reeiCLHbQYIdRAyJ7Ud-hDxSJwuZ1iqH-7kMcMtNHtrlh5JydP2Ya8p9GRWtFVmtfq4bzIbKfbqh2dI4_dv_8mVc-h8CQQxE-J7ME9RH9pw-1ZQccud0e4RCqHuVa6tR3ByEceFgsmwl1l779gZjQGoBrLm5sBLxc47Ni6bVW8_Czit3EPJjaq6vh22L8XnfsF256FHmtDgmSFUTljZ8l37VwGA4Y_4q62_k4RKJZbJft3i7_XW2eJxcfWAFTje1jjUIHi81WitYT24sisKkRmqHak6_yQFwbayocifHA23T8PfXoKqkLlDO9ewUYL83HrGsOrPHSd8JbitwP400KOWotEZVDATVqQxJhnJRa6LsVTtiYuZVcC6-PLcVSTAHPDbRF7Q_WcB4QL-0MeEorgQ-ULn_9trRFCjrs3tXlCo_6qPEvDFtAm3YAcZVVEaCcMRcwY5cnLeIL7q2gsB06txdyzGgriCrc_gMZksCy3M_V32yX8pP4RE81BP6cggKZu7BwlLzh3nOozg0AJqvC00IKcJ7k5iMhjSARFURevVtIhWz3RQ6_CAQAU5M4PfSaAnYzDHLO2FDKfnTThgJmZEYMAqUUJOXgLn4jrcE4OLXxuPLHfxdiqoAiAfhzTsTu6Wvn5x0YTaRq27_2bhw2pMXRNDotTjHQW6StxWxhlCvynLBVONRE4YKiXLN_YCXTd0FNjEyGQQRjmLbTQK6vvGgReEP2aUdB4-FarpRYU0b3DMFn6eFCUx0hdFMc2BunByvGDB8f3h5aubWaEZM4E-H-a7fIA_FE9wAORKqHd7wM1rx2x3nYVGk9k0HqHbYCyO48lAS2L5DmKUXPhH81ugVBbWCgX9EpzyLgUlloaTWnseadBr1rZBsUOUBqZ0M9gzJjtzYbqH8xvJiFtZ4Za01xveOJAv4qBp3uib9MHFtJqK1JkpQRP214aEQ

# Charger les bibliothèques nécessaires
library(dplyr)

# Définir le nombre de familles
n_families <- 100000

# Définir les proportions des tailles de fratrie
sizes <- c(0.179, 0.409, 0.269, 0.099, 0.031, 0.01, 0.004, 0.001)
family_sizes <- rep(1:8, round(n_families * sizes))
family_sizes <- family_sizes[1:n_families]  # S'assurer que nous avons exactement n_families

# Calculer le nombre total d'individus
n_individuals <- sum(family_sizes)

# Générer les identifiants familiaux et individuels
set.seed(123)  # pour la reproductibilité
family_id <- rep(1:n_families, times = family_sizes)
individual_id <- 1:n_individuals

# Générer les âges des enfants en 2000 (16 à 74 ans)
age_2000 <- sample(16:74, n_individuals, replace = TRUE)

# Générer l'année de naissance
year_of_birth <- 2000 - age_2000

# Générer les âges des parents sans les restreindre initialement
mother_age_2000 <- sample(16:49, n_individuals, replace = TRUE) + age_2000
father_age_2000 <- sample(16:54, n_individuals, replace = TRUE) + age_2000

# Générer les niveaux d'éducation en fonction de la taille de la famille et de l'ordre de naissance
family_size_levels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
education_means <- c(12.0, 12.4, 12.3, 12.0, 11.7, 11.4, 11.2, 11.1, 11.0, 11.0)
education_sds <- rep(2.4, length(family_size_levels))  # Utiliser l'écart type global

birth_order_levels <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
birth_order_effects <- c(0, -0.294, -0.494, -0.632, -0.718, -0.782, -0.854, -0.753, -0.945, -1.131) # Effets négatifs et décroissants

# birth_order_means <- c(12.2, 12.2, 12.0, 11.9, 11.7, 11.6, 11.5, 11.6, 11.3, 11.3)
# birth_order_sds <- rep(2.4, length(birth_order_levels))  # Utiliser l'écart type global

# Fonction pour ajuster l'éducation en fonction de la taille de la famille et de l'ordre de naissance
#adjust_education <- function(family_size, birth_order) {
#  family_size_effect <- education_means[family_size]
#  birth_order_effect <- birth_order_means[birth_order]
#  mean_education <- (family_size_effect + birth_order_effect) / 2
#  rnorm(1, mean = mean_education, sd = 2.4)
#}

female <- rbinom(n_individuals, 1, 0.48)
female_effect <- 0.3

adjust_education <- function(family_size, birth_order) {
  family_size_effect <- education_means[family_size]
  birth_order_effect <- birth_order_effects[birth_order]
  gender_effect <- ifelse(female == 1, female_effect, 0)
  mean_education <- family_size_effect + birth_order_effect
  rnorm(1, mean = mean_education, sd = 2.4)
}

# Générer les autres variables en respectant les moyennes et écarts types de la Table 1
education <- mapply(adjust_education, family_sizes[family_id], ave(family_id, family_id, FUN = seq_along))
mother_education <- rnorm(n_individuals, mean = 9.5, sd = 2.4)
father_education <- rnorm(n_individuals, mean = 10.4, sd = 3.0)
twins_in_family <- rbinom(n_individuals, 1, 0.015)

# Créer le data frame initial
data <- data.frame(
  family_id = family_id,
  individual_id = individual_id,
  family_size = family_sizes[family_id],
  age_2000 = age_2000,
  year_of_birth = year_of_birth,
  female = female,
  education = education,
  mother_education = mother_education,
  father_education = father_education,
  mother_age_2000 = mother_age_2000,
  father_age_2000 = father_age_2000,
  twins_in_family = twins_in_family
)

# Ajouter la variable birth_order
data <- data %>%
  group_by(family_id) %>%
  mutate(birth_order = row_number()) %>%
  ungroup()

# Sauvegarder la base de données générée dans un fichier CSV
write.csv(data, "Lecture 3/data/simulated_data_black_et_al_2005.csv", row.names = FALSE)

# Appliquer les exclusions :
# 1. Enfants âgés de 25 ans ou plus en 2000
# data <- data %>% filter(age_2000 >= 25)
# 
# # 2. Supprimer les jumeaux
# data <- data %>% filter(twins_in_family == 0)
# 
# # 3. Exclure les familles dans lesquelles il y a un enfant plus jeune que 16 ans en 2000
# data <- data %>%
#   group_by(family_id) %>%
#   filter(all(age_2000 >= 16)) %>%
#   ungroup()
# 
# # 4. Exclure les familles où une naissance a eu lieu lorsque la mère est âgée de moins de 16 ans ou plus de 49 ans
# data <- data %>%
#   filter(mother_age_2000 - age_2000 <= 49 & mother_age_2000 - age_2000 >= 16)
# 
# # 5. Exclure les familles où le niveau de diplôme d'un enfant est manquant
# data <- data %>%
#   filter(!is.na(education))
# 
# # Afficher un aperçu des données filtrées
# head(data)
# 
# # Sauvegarder la base de données nettoyée dans un fichier CSV
# write.csv(data, "simulated_data_black_et_al_2005.csv", row.names = FALSE)
# 