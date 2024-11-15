# ------------------------------------------------------------------------------------
#       Lecture 3 : Régression linéaire simple et Moindres Carrés Ordinaires
# ------------------------------------------------------------------------------------

# Ce code simule une base de données de 100 000 familles ayant les mêmes caractéristiques que l'échantillon dans Black et al. 2005
# https://watermark.silverchair.com/120-2-669.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA2YwggNiBgkqhkiG9w0BBwagggNTMIIDTwIBADCCA0gGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMRUFpt9yUxU8Wuu4FAgEQgIIDGVcdvdpNZkmD_Fbj4_wa9HaGQTAoM2tQsF2wuRUmWvvlH23Fhxp68TwZUK_P2MpI1e4pA0PyMp-QAURQMsN_P7FcQuEoYqMkBs_7l-8reeiCLHbQYIdRAyJ7Ud-hDxSJwuZ1iqH-7kMcMtNHtrlh5JydP2Ya8p9GRWtFVmtfq4bzIbKfbqh2dI4_dv_8mVc-h8CQQxE-J7ME9RH9pw-1ZQccud0e4RCqHuVa6tR3ByEceFgsmwl1l779gZjQGoBrLm5sBLxc47Ni6bVW8_Czit3EPJjaq6vh22L8XnfsF256FHmtDgmSFUTljZ8l37VwGA4Y_4q62_k4RKJZbJft3i7_XW2eJxcfWAFTje1jjUIHi81WitYT24sisKkRmqHak6_yQFwbayocifHA23T8PfXoKqkLlDO9ewUYL83HrGsOrPHSd8JbitwP400KOWotEZVDATVqQxJhnJRa6LsVTtiYuZVcC6-PLcVSTAHPDbRF7Q_WcB4QL-0MeEorgQ-ULn_9trRFCjrs3tXlCo_6qPEvDFtAm3YAcZVVEaCcMRcwY5cnLeIL7q2gsB06txdyzGgriCrc_gMZksCy3M_V32yX8pP4RE81BP6cggKZu7BwlLzh3nOozg0AJqvC00IKcJ7k5iMhjSARFURevVtIhWz3RQ6_CAQAU5M4PfSaAnYzDHLO2FDKfnTThgJmZEYMAqUUJOXgLn4jrcE4OLXxuPLHfxdiqoAiAfhzTsTu6Wvn5x0YTaRq27_2bhw2pMXRNDotTjHQW6StxWxhlCvynLBVONRE4YKiXLN_YCXTd0FNjEyGQQRjmLbTQK6vvGgReEP2aUdB4-FarpRYU0b3DMFn6eFCUx0hdFMc2BunByvGDB8f3h5aubWaEZM4E-H-a7fIA_FE9wAORKqHd7wM1rx2x3nYVGk9k0HqHbYCyO48lAS2L5DmKUXPhH81ugVBbWCgX9EpzyLgUlloaTWnseadBr1rZBsUOUBqZ0M9gzJjtzYbqH8xvJiFtZ4Za01xveOJAv4qBp3uib9MHFtJqK1JkpQRP214aEQ


# Charger la bibliothèque dplyr pour la manipulation de données
library(dplyr)

# Définir le nombre de familles simulées
n_families <- 100000

# Définir les proportions pour chaque taille de fratrie (1 à 8 enfants par famille)
sizes <- c(0.179, 0.409, 0.269, 0.099, 0.031, 0.01, 0.002, 0.001)

# Créer un vecteur indiquant la taille des fratries en respectant les proportions
family_sizes <- rep(1:8, round(n_families * sizes))

# Ajuster le vecteur pour s'assurer qu'il y a exactement n_families éléments
family_sizes <- family_sizes[1:n_families]

# Calculer le nombre total d'individus en sommant les tailles de toutes les fratries
n_individuals <- sum(family_sizes)

# Créer un DataFrame initial avec un identifiant de famille pour chaque individu, en répétant chaque ID
# selon la taille de la fratrie
data = data.frame(family_id = rep(1:n_families, times = family_sizes)) %>% 
  group_by(family_id) %>% 
  mutate(
    family_size = n(),          # Calculer la taille de la famille pour chaque famille
    birth_order = row_number()   # Attribuer le rang de naissance de chaque enfant dans la famille
  ) %>% 
  ungroup()

# Créer un DataFrame pour les effets de la taille de la fratrie sur le niveau d'éducation
family_size_effects_with_control = data.frame(
  family_size = seq(1:8),                        # Taille de la fratrie de 1 à 8
  family_size_effect = c(0, 0.257, 0.270, 0.195, 0.115, 0.034, -0.018, -0.039) # Effets spécifiques pour chaque taille de fratrie
)

# Créer un DataFrame pour les effets du rang de naissance sur le niveau d'éducation
birth_order_effects = data.frame(
  birth_order = seq(1:8),                        # Rang de naissance de 1 à 8
  birth_order_effect = c(0, -0.342, -0.538, -0.621, -0.648, -0.661, -0.709, -0.800) # Effets spécifiques pour chaque rang de naissance
)

# Ajouter les effets de la taille de la fratrie et du rang de naissance dans le DataFrame principal
data = data %>% 
  left_join(family_size_effects_with_control, by = 'family_size') %>% 
  left_join(birth_order_effects, by = 'birth_order')

# Fixer la graine pour obtenir des résultats reproductibles
set.seed(123)

# Calculer le niveau d'éducation de chaque individu en ajoutant un bruit normal
data$education = 12 + data$family_size_effect + data$birth_order_effect + rnorm(n = nrow(data), sd = 2)

# Résumer le modèle de régression pour vérifier l'effet de la taille de la fratrie sur l'éducation
summary(lm(education ~ factor(family_size), data = data))

# Générer une année de naissance pour le premier enfant de chaque famille entre 1950 et 1980
birth_year_first_born = trunc(runif(n = n_families, min = 1950, max = 1981))

# Générer une année de naissance pour la mère avec une moyenne d'écart de 24 ans par rapport au premier enfant
mother_year = trunc(mapply(rnorm, n = 1, mean = birth_year_first_born - 24, sd = 5))

# Créer un DataFrame contenant l'année de naissance du premier enfant et de la mère pour chaque famille
fb_mother = data.frame(
  family_id = seq(1, n_families),
  fb_birth = birth_year_first_born,
  mother_birth = mother_year
)

# Fusionner les informations sur le premier enfant et la mère dans le DataFrame principal
data = data %>% 
  left_join(fb_mother, by = 'family_id') %>% 
  mutate(
    ecart = runif(n = nrow(data), min = 0, max = 6),        # Générer un écart aléatoire pour chaque enfant
    ecart = ifelse(birth_order == 1, 0, ecart)              # Mettre cet écart à 0 pour le premier enfant
  ) %>% 
  arrange(family_id, birth_order) %>% 
  group_by(family_id) %>% 
  mutate(birth_year = trunc(fb_birth + cumsum(ecart))) %>%             # Calculer l'année de naissance pour chaque enfant
  select(-birth_order_effect, - family_size_effect, -fb_birth, - ecart)

# Calculer l'âge en 2000 pour chaque enfant et pour la mère
data$age_2000 = 2000 - data$birth_year
data$mother_age_2000 = 2000 - data$mother_birth

# Sauvegarder le DataFrame en format RDS
saveRDS(data, here("Lecture 3/data", "simulated_data_black_et_al_2005.rds"))

