# ------------------------------------------------------------------------------------
#       Simulation de données de fratries avec instrument same_sex
#       VERSION MODIFIÉE : Distribution de taille basée sur les données observées par CSP
# ------------------------------------------------------------------------------------

# Charger les bibliothèques nécessaires
library(dplyr)
library(tidyr)

# Fixer la graine pour la reproductibilité
set.seed(123)

# ------------------------------------------------------------------------------------
# 1. CRÉATION DES FAMILLES DE BASE (AVEC AU MOINS 2 ENFANTS)
# ------------------------------------------------------------------------------------

# Distribution totale par cohorte (on va générer plus de familles puis ajuster)
# Facteur de multiplication pour atteindre environ 100 000 observations
multiplier <- 2.875

# Nombre total de familles par cohorte (somme sur toutes les tailles)
cohort_totals <- c(
  sum(c(516, 370, 224, 108, 76, 28, 28, 9, 4, 2, 2)),  # 1945-1949
  sum(c(663, 500, 248, 130, 58, 41, 22, 11, 6, 3, 0)), # 1950-1954
  sum(c(886, 585, 282, 124, 69, 36, 8, 9, 2, 1, 2)),   # 1955-1959
  sum(c(1015, 617, 284, 104, 41, 19, 12, 6, 3, 1, 2)), # 1960-1964
  sum(c(1046, 605, 196, 57, 27, 7, 7, 0, 2, 0, 0)),    # 1965-1969
  sum(c(1047, 419, 112, 22, 10, 1, 0, 0, 0, 0, 0)),    # 1970-1974
  sum(c(648, 249, 46, 9, 4, 1, 0, 1, 0, 0, 0)),        # 1975-1979
  sum(c(308, 67, 12, 0, 1, 0, 0, 0, 0, 0, 0))          # 1980-1984
)

cohort_names <- c("1945-1949", "1950-1954", "1955-1959", "1960-1964", 
                  "1965-1969", "1970-1974", "1975-1979", "1980-1984")

# Créer les familles de base avec cohorte
cohort_counts <- round(cohort_totals * multiplier)
total_families <- sum(cohort_counts)

families_base <- data.frame(
  family_id = 1:total_families,
  cohort = rep(cohort_names, cohort_counts)
)

# ------------------------------------------------------------------------------------
# 2. GÉNÉRATION DU SEXE DES DEUX PREMIERS ENFANTS ET SAME_SEX
# ------------------------------------------------------------------------------------

# Pour chaque famille, générer le sexe des 2 premiers enfants (aléatoire, prob 0.5)
families_base <- families_base %>%
  mutate(
    sexe_first = sample(1:2, n(), replace = TRUE, prob = c(0.5, 0.5)),
    sexe_second = sample(1:2, n(), replace = TRUE, prob = c(0.5, 0.5)),
    same_sex = ifelse(sexe_first == sexe_second, 1, 0)
  )

# ------------------------------------------------------------------------------------
# 3. GÉNÉRATION DES CSP DES PARENTS
# ------------------------------------------------------------------------------------

# Distribution marginale des CSP du père (calculée à partir de votre tableau)
father_cs_probs <- c(1937, 3187, 6672, 3751, 1486, 6252) / sum(c(1937, 3187, 6672, 3751, 1486, 6252))

# Distribution marginale des CSP de la mère
mother_cs_probs <- c(2305, 1973, 2591, 3968, 10643, 4721) / sum(c(2305, 1973, 2591, 3968, 10643, 4721))

# Générer les CSP des parents pour chaque famille
families_base <- families_base %>%
  mutate(
    father_cs = sample(1:6, n(), replace = TRUE, prob = father_cs_probs),
    mother_cs = sample(1:6, n(), replace = TRUE, prob = mother_cs_probs)
  )

# ------------------------------------------------------------------------------------
# 4. GÉNÉRATION DE LA TAILLE DE FRATRIE (CONDITIONNELLE À SAME_SEX ET CSP)
# ------------------------------------------------------------------------------------

# Distribution conditionnelle de la taille par CSP (basée sur les données observées)
# Taille:     2    3    4    5    6    7    8    9   10   11   12
# CSP 1:    471  287  155   53   23   10    5    5    1    1    0
# CSP 2:    820  381  115   53   20    8    7    0    0    0    0
# CSP 3:   1261  695  261   68   26    9    5    0    1    0    0
# CSP 4:    850  439  144   40   18    8    9    2    2    0    0
# CSP 5:    316  151   71   27   16    3    2    2    0    0    0
# CSP 6:    873  590  253  144   73   30   19   13    6    1    2

family_size_by_cs <- data.frame(
  father_cs = rep(1:6, each = 11),
  family_size = rep(2:12, 6),
  count = c(
    # CSP 1
    471, 287, 155, 53, 23, 10, 5, 5, 1, 1, 0,
    # CSP 2
    820, 381, 115, 53, 20, 8, 7, 0, 0, 0, 0,
    # CSP 3
    1261, 695, 261, 68, 26, 9, 5, 0, 1, 0, 0,
    # CSP 4
    850, 439, 144, 40, 18, 8, 9, 2, 2, 0, 0,
    # CSP 5
    316, 151, 71, 27, 16, 3, 2, 2, 0, 0, 0,
    # CSP 6
    873, 590, 253, 144, 73, 30, 19, 13, 6, 1, 2
  )
) %>%
  group_by(father_cs) %>%
  mutate(prob = count / sum(count)) %>%
  ungroup()

# Générer la taille de fratrie conditionnellement à same_sex ET à la CSP du père ET à la cohorte
# Ajustement de same_sex variant selon la cohorte :
# - Avant 1960-1964 : effet faible/non significatif
# - À partir de 1960-1964 : effet qui se creuse progressivement

# Ajustement de same_sex selon la cohorte (en points de pourcentage sur prob de 3+ enfants)
cohort_same_sex_adjustment <- data.frame(
  cohort = cohort_names,
  adjustment_ss = c(0.01, 0.01, 0.015, 0.05, 0.08, 0.10, 0.12, 0.12)
  # Avant 1960 : effet très faible (~1-1.5 pp, non significatif)
  # À partir de 1960-1964 : effet qui augmente (5 pp puis 8, 10, 12 pp)
)

# Facteur de réduction de la taille selon la cohorte (plus la cohorte est récente, plus petite la fratrie)
cohort_size_adjustment <- data.frame(
  cohort = cohort_names,
  size_reduction_factor = c(1.0, 0.95, 0.90, 0.85, 0.75, 0.60, 0.50, 0.45)
  # Réduction progressive : les cohortes récentes ont moins d'enfants
)

families_with_size <- list()

for (coh in cohort_names) {
  for (cs in 1:6) {
    for (ss in 0:1) {
      # Filtrer les familles pour cette combinaison cohorte × CSP × same_sex
      fam_subset <- families_base %>% 
        filter(cohort == coh, father_cs == cs, same_sex == ss)
      
      if (nrow(fam_subset) == 0) next
      
      # Distribution de base pour cette CSP
      size_probs <- family_size_by_cs %>% filter(father_cs == cs)
      probs <- size_probs$prob
      
      prob_2 <- probs[1]
      prob_3plus <- sum(probs[2:11])
      
      # Ajustement pour same_sex (variant selon la cohorte)
      adjustment_ss <- cohort_same_sex_adjustment$adjustment_ss[cohort_same_sex_adjustment$cohort == coh]
      
      if (ss == 1) {
        # Same sex : plus de 3+ enfants
        new_prob_2 <- max(0.001, prob_2 - adjustment_ss)
        new_prob_3plus <- prob_3plus + adjustment_ss
        probs[1] <- new_prob_2
        if (new_prob_3plus > 0 && prob_3plus > 0) {
          probs[2:11] <- probs[2:11] * (new_prob_3plus / prob_3plus)
        }
      } else {
        # Different sex : moins de 3+ enfants
        new_prob_2 <- min(0.999, prob_2 + adjustment_ss)
        new_prob_3plus <- max(0.001, prob_3plus - adjustment_ss)
        probs[1] <- new_prob_2
        if (new_prob_3plus > 0 && prob_3plus > 0) {
          probs[2:11] <- probs[2:11] * (new_prob_3plus / prob_3plus)
        }
      }
      
      # Ajustement pour la cohorte : réduction de la taille des fratries dans les cohortes récentes
      reduction_factor <- cohort_size_adjustment$size_reduction_factor[cohort_size_adjustment$cohort == coh]
      
      # On augmente la probabilité de 2 enfants et on réduit celle des fratries plus grandes
      # selon le facteur de réduction
      current_prob_2 <- probs[1]
      current_prob_3plus <- sum(probs[2:11])
      
      # Plus le facteur est faible, plus on transfère de probabilité vers 2 enfants
      transfer_amount <- current_prob_3plus * (1 - reduction_factor)
      
      probs[1] <- current_prob_2 + transfer_amount
      probs[2:11] <- probs[2:11] * reduction_factor
      
      # S'assurer que les probabilités sont positives et somment à 1
      probs <- pmax(probs, 0.0001)
      probs <- probs / sum(probs)
      
      # Assigner les tailles
      fam_subset$family_size <- sample(2:12, nrow(fam_subset), replace = TRUE, prob = probs)
      
      families_with_size[[paste(coh, cs, ss, sep = "_")]] <- fam_subset
    }
  }
}

families <- bind_rows(families_with_size)
n_families <- nrow(families)

# ------------------------------------------------------------------------------------
# 5. GÉNÉRATION DES INDIVIDUS (ENFANTS DANS CHAQUE FRATRIE)
# ------------------------------------------------------------------------------------

# Créer un individu par enfant dans chaque fratrie
siblings <- families %>%
  slice(rep(1:n(), family_size)) %>%
  group_by(family_id) %>%
  mutate(birth_order = row_number()) %>%
  ungroup()

# Assigner le sexe : pour les 2 premiers, utiliser sexe_first et sexe_second
# Pour les suivants, générer aléatoirement
siblings <- siblings %>%
  group_by(family_id) %>%
  mutate(
    sexe = case_when(
      birth_order == 1 ~ first(sexe_first),
      birth_order == 2 ~ first(sexe_second),
      TRUE ~ sample(1:2, 1, prob = c(0.5, 0.5))
    )
  ) %>%
  ungroup() %>%
  select(-sexe_first, -sexe_second)  # Nettoyer les variables temporaires

# ------------------------------------------------------------------------------------
# 6. GÉNÉRATION DES ANNÉES DE NAISSANCE INDIVIDUELLES
# ------------------------------------------------------------------------------------

# Pour chaque cohorte de fratrie, générer les années de naissance de l'aîné
# puis ajouter des écarts pour les cadets

siblings <- siblings %>%
  group_by(family_id) %>%
  mutate(
    # Année de naissance de l'aîné basée sur la cohorte de la fratrie
    birth_year_first = case_when(
      cohort == "1945-1949" ~ sample(1945:1949, 1),
      cohort == "1950-1954" ~ sample(1950:1954, 1),
      cohort == "1955-1959" ~ sample(1955:1959, 1),
      cohort == "1960-1964" ~ sample(1960:1964, 1),
      cohort == "1965-1969" ~ sample(1965:1969, 1),
      cohort == "1970-1974" ~ sample(1970:1974, 1),
      cohort == "1975-1979" ~ sample(1975:1979, 1),
      cohort == "1980-1984" ~ sample(1980:1984, 1)
    ),
    # Écart en années entre chaque enfant (0 pour le premier, puis entre 1 et 4 ans)
    year_gap = ifelse(birth_order == 1, 0, sample(1:4, n()-1, replace = TRUE)),
    year_gap = ifelse(birth_order == 1, 0, year_gap),
    # Année de naissance cumulative
    birth_year = birth_year_first + cumsum(year_gap)
  ) %>%
  select(-birth_year_first, -year_gap) %>%
  ungroup()

# Créer une variable de cohorte individuelle basée sur l'année de naissance
siblings <- siblings %>%
  mutate(
    individual_cohort = case_when(
      birth_year >= 1945 & birth_year <= 1949 ~ "1945-1949",
      birth_year >= 1950 & birth_year <= 1954 ~ "1950-1954",
      birth_year >= 1955 & birth_year <= 1959 ~ "1955-1959",
      birth_year >= 1960 & birth_year <= 1964 ~ "1960-1964",
      birth_year >= 1965 & birth_year <= 1969 ~ "1965-1969",
      birth_year >= 1970 & birth_year <= 1974 ~ "1970-1974",
      birth_year >= 1975 & birth_year <= 1979 ~ "1975-1979",
      birth_year >= 1980 & birth_year <= 1984 ~ "1980-1984",
      birth_year >= 1985 & birth_year <= 1989 ~ "1985-1989",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------------
# 7. GÉNÉRATION DU NIVEAU D'ÉDUCATION (VARIABLE DÉPENDANTE)
# ------------------------------------------------------------------------------------

# Effets de la cohorte (référence : 1945-1949)
# Ajustement pour éviter une baisse trop forte dans les dernières cohortes
cohort_effects <- data.frame(
  cohort = c("1945-1949", "1950-1954", "1955-1959", "1960-1964", 
             "1965-1969", "1970-1974", "1975-1979", "1980-1984", "1985-1989"),
  cohort_effect = c(0, 0.043367, 0.055849, 0.108011, 0.216406, 
                    0.452773, 0.656041, 0.700000, 0.720000)
  # Augmentation continue pour les dernières cohortes au lieu de baisse
)

# Effet du sexe variant selon la cohorte (référence : sexe 1 = homme)
# Avant 1955 : les hommes ont un avantage
# À partir de 1955 : les femmes commencent à dépasser
# L'écart se creuse dans les cohortes récentes
sexe_cohort_effects <- data.frame(
  cohort = c("1945-1949", "1950-1954", "1955-1959", "1960-1964", 
             "1965-1969", "1970-1974", "1975-1979", "1980-1984", "1985-1989"),
  sexe_effect = c(-0.15, -0.05, 0.10, 0.20, 0.25, 0.35, 0.45, 0.50, 0.50)
  # Négatif = avantage hommes, Positif = avantage femmes (sexe 2)
)

# Effets de la taille de la fratrie (référence : 2 enfants)
family_size_effects <- data.frame(
  family_size = 2:12,
  family_size_effect = c(0, -0.093949, -0.220540, -0.357928, -0.439703, 
                         -0.454736, -0.648244, -0.826205, -1.088609, 
                         0, -1.536187)  # Note : pas d'effet pour 11 (non estimé)
)

# Effets du rang de naissance (effet linéaire approximatif basé sur vos données)
# À partir des distributions diplôme × rang, on peut estimer un effet négatif du rang
birth_order_effect_coef <- -0.15  # Effet négatif du rang sur l'éducation

# Effets de la CSP de la mère (référence : mère CSP 1)
mother_cs_effects <- data.frame(
  mother_cs = 1:6,
  mother_cs_effect = c(0, 0.115617, 0.723752, 0.488383, -0.008262, -0.297138)
)

# Effets de la CSP du père (référence : père CSP 1)
father_cs_effects <- data.frame(
  father_cs = 1:6,
  father_cs_effect = c(0, 0.269670, 0.887846, 0.270772, -0.004891, -0.347712)
)

# Joindre tous les effets
siblings <- siblings %>%
  left_join(cohort_effects %>% rename(individual_cohort = cohort), 
            by = "individual_cohort") %>%
  left_join(sexe_cohort_effects %>% rename(individual_cohort = cohort), 
            by = "individual_cohort") %>%
  left_join(family_size_effects, by = "family_size") %>%
  left_join(mother_cs_effects, by = "mother_cs") %>%
  left_join(father_cs_effects, by = "father_cs")

# Calculer le niveau d'éducation
# Formule : intercept + effets des variables + erreur aléatoire
intercept <- 2.704932

siblings <- siblings %>%
  mutate(
    # Effet du sexe variant selon la cohorte
    sexe_eff = ifelse(sexe == 2, sexe_effect, 0),
    # Effet du rang de naissance
    birth_order_eff = birth_order_effect_coef * (birth_order - 1),
    # Niveau d'éducation
    dipl = intercept + 
      cohort_effect + 
      sexe_eff + 
      family_size_effect + 
      birth_order_eff +
      mother_cs_effect + 
      father_cs_effect + 
      rnorm(n(), mean = 0, sd = 1.0),  # Erreur aléatoire
    # Arrondir et contraindre entre 1 et 5
    dipl = pmax(1, pmin(5, round(dipl)))
  ) %>%
  select(-cohort_effect, -sexe_eff, -sexe_effect, -family_size_effect, 
         -birth_order_eff, -mother_cs_effect, -father_cs_effect)

# Créer la variable more_than_2
siblings <- siblings %>%
  mutate(more_than_2 = ifelse(family_size > 2, 1, 0))

# ------------------------------------------------------------------------------------
# 8. NETTOYAGE ET FINALISATION
# ------------------------------------------------------------------------------------

# Convertir les variables catégorielles en facteurs
siblings <- siblings %>%
  mutate(
    cohort = factor(cohort, levels = c("1945-1949", "1950-1954", "1955-1959", 
                                       "1960-1964", "1965-1969", "1970-1974", 
                                       "1975-1979", "1980-1984")),
    individual_cohort = factor(individual_cohort, 
                               levels = c("1945-1949", "1950-1954", "1955-1959", 
                                          "1960-1964", "1965-1969", "1970-1974", 
                                          "1975-1979", "1980-1984", "1985-1989")),
    sexe = factor(sexe),
    father_cs = factor(father_cs),
    mother_cs = factor(mother_cs),
    same_sex = factor(same_sex)
  )

# Sélectionner et ordonner les colonnes finales
siblings <- siblings %>%
  select(family_id, birth_order, family_size, cohort, individual_cohort, 
         birth_year, sexe, same_sex, more_than_2, 
         father_cs, mother_cs, dipl)

# ------------------------------------------------------------------------------------
# 9. VÉRIFICATIONS ET STATISTIQUES DESCRIPTIVES
# ------------------------------------------------------------------------------------

cat("\n\n=== VÉRIFICATIONS DES DISTRIBUTIONS ===\n\n")

# Distribution de la taille des fratries
cat("Distribution de la taille des fratries :\n")
print(table(siblings %>% distinct(family_id, family_size) %>% pull(family_size)))

# Taille moyenne des fratries par CSP du père
cat("\n\nTaille moyenne des fratries par CSP du père :\n")
taille_by_cs <- siblings %>%
  distinct(family_id, family_size, father_cs) %>%
  group_by(father_cs) %>%
  summarise(
    n_families = n(),
    mean_size = mean(family_size),
    .groups = 'drop'
  )
print(taille_by_cs)

# Taille moyenne des fratries par cohorte
cat("\n\nTaille moyenne des fratries par cohorte :\n")
taille_by_cohort <- siblings %>%
  distinct(family_id, family_size, cohort) %>%
  group_by(cohort) %>%
  summarise(
    n_families = n(),
    mean_size = mean(family_size),
    prop_2_children = mean(family_size == 2),
    prop_3plus_children = mean(family_size >= 3),
    .groups = 'drop'
  )
print(taille_by_cohort)

# Distribution de la taille par CSP (tableau complet)
cat("\n\nDistribution de la taille par CSP du père :\n")
size_by_cs_table <- siblings %>%
  distinct(family_id, family_size, father_cs) %>%
  group_by(father_cs, family_size) %>%
  summarise(n = n(), .groups = 'drop') %>%
  pivot_wider(names_from = family_size, values_from = n, values_fill = 0)
print(size_by_cs_table)

# Distribution du sexe
cat("\n\nDistribution du sexe :\n")
print(table(siblings$sexe))

# Distribution de same_sex
cat("\n\nDistribution de same_sex :\n")
print(table(siblings %>% distinct(family_id, same_sex) %>% pull(same_sex)))

# VÉRIFICATION CLEF : Relation entre same_sex et probabilité d'avoir 3+ enfants
cat("\n\n=== VÉRIFICATION DE L'EFFET DE SAME_SEX SUR LA TAILLE ===\n\n")
verification <- siblings %>%
  distinct(family_id, family_size, same_sex) %>%
  mutate(more_than_2 = ifelse(family_size > 2, 1, 0)) %>%
  group_by(same_sex) %>%
  summarise(
    n_families = n(),
    prop_more_than_2 = mean(more_than_2),
    .groups = 'drop'
  ) %>%
  mutate(
    same_sex_label = ifelse(same_sex == 1, "Same sex", "Different sex"),
    prop_pct = paste0(round(prop_more_than_2 * 100, 2), "%")
  )

print(verification)

diff_prop <- verification$prop_more_than_2[verification$same_sex == 1] - 
  verification$prop_more_than_2[verification$same_sex == 0]
cat("\nDifférence de probabilité d'avoir 3+ enfants (same_sex - diff_sex) :", 
    round(diff_prop * 100, 2), "points de pourcentage\n")

# VÉRIFICATION : Relation entre same_sex et taille par CSP
cat("\n\n=== VÉRIFICATION DE L'EFFET DE SAME_SEX PAR CSP ===\n\n")
verification_by_cs <- siblings %>%
  distinct(family_id, family_size, same_sex, father_cs) %>%
  mutate(more_than_2 = ifelse(family_size > 2, 1, 0)) %>%
  group_by(father_cs, same_sex) %>%
  summarise(
    n_families = n(),
    prop_more_than_2 = mean(more_than_2),
    mean_size = mean(family_size),
    .groups = 'drop'
  )
print(verification_by_cs)

# VÉRIFICATION : Effet de same_sex par COHORTE (clé pour l'instrument)
cat("\n\n=== ÉVOLUTION DE L'EFFET DE SAME_SEX PAR COHORTE ===\n\n")
verification_by_cohort <- siblings %>%
  distinct(family_id, family_size, same_sex, cohort) %>%
  mutate(more_than_2 = ifelse(family_size > 2, 1, 0)) %>%
  group_by(cohort, same_sex) %>%
  summarise(
    n_families = n(),
    prop_more_than_2 = mean(more_than_2),
    mean_size = mean(family_size),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = same_sex,
    values_from = c(n_families, prop_more_than_2, mean_size),
    names_sep = "_"
  ) %>%
  mutate(
    diff_prop = prop_more_than_2_1 - prop_more_than_2_0,
    diff_size = mean_size_1 - mean_size_0,
    diff_prop_pct = round(diff_prop * 100, 2)
  )

print(verification_by_cohort)

cat("\n\nÉvolution graphique de l'effet (texte) :\n")
for (i in 1:nrow(verification_by_cohort)) {
  coh <- verification_by_cohort$cohort[i]
  diff <- verification_by_cohort$diff_prop_pct[i]
  
  if (!is.na(diff)) {
    significance <- ifelse(abs(diff) < 2, "non significatif", 
                           ifelse(abs(diff) < 5, "faible", 
                                  ifelse(abs(diff) < 8, "modéré", "fort")))
    cat(sprintf("%s: diff = %+.2f pp (%s)\n", coh, diff, significance))
  }
}

# Test statistique par cohorte
cat("\n\n=== TESTS STATISTIQUES PAR COHORTE ===\n\n")
for (coh in cohort_names) {
  fam_coh <- siblings %>%
    filter(cohort == coh) %>%
    distinct(family_id, family_size, same_sex) %>%
    mutate(more_than_2 = ifelse(family_size > 2, 1, 0))
  
  if (nrow(fam_coh) > 0) {
    test_result <- t.test(more_than_2 ~ same_sex, data = fam_coh)
    cat(sprintf("%s: t = %.3f, p-value = %.4f %s\n", 
                coh, 
                test_result$statistic, 
                test_result$p.value,
                ifelse(test_result$p.value < 0.05, "***", 
                       ifelse(test_result$p.value < 0.10, "*", ""))))
  }
}

# Répartition des sexes des deux premiers enfants
cat("\n\nRépartition des sexes des deux premiers enfants :\n")
two_first <- siblings %>%
  filter(birth_order <= 2) %>%
  group_by(family_id) %>%
  filter(n() == 2) %>%
  summarise(
    sex_pattern = paste0(ifelse(sexe[1] == 1, "M", "F"), 
                         ifelse(sexe[2] == 1, "M", "F")),
    .groups = 'drop'
  )
print(table(two_first$sex_pattern))

# Distribution de l'éducation
cat("\n\nDistribution du niveau d'éducation :\n")
print(table(siblings$dipl))

# VÉRIFICATION : Niveau moyen d'éducation par sexe et cohorte
cat("\n\n=== ÉVOLUTION DU NIVEAU D'ÉDUCATION PAR SEXE ET COHORTE ===\n\n")
educ_by_sex_cohort <- siblings %>%
  group_by(individual_cohort, sexe) %>%
  summarise(
    n = n(),
    mean_dipl = mean(dipl, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = sexe,
    values_from = c(n, mean_dipl),
    names_sep = "_"
  ) %>%
  mutate(
    diff_F_M = mean_dipl_2 - mean_dipl_1,
    ratio_F_M = mean_dipl_2 / mean_dipl_1
  )

print(educ_by_sex_cohort)

cat("\n\nGraphique de l'évolution (texte) :\n")
for (i in 1:nrow(educ_by_sex_cohort)) {
  coh <- educ_by_sex_cohort$individual_cohort[i]
  diff <- educ_by_sex_cohort$diff_F_M[i]
  m_dipl <- educ_by_sex_cohort$mean_dipl_1[i]
  f_dipl <- educ_by_sex_cohort$mean_dipl_2[i]
  
  if (!is.na(diff)) {
    direction <- ifelse(diff > 0, "F > H", ifelse(diff < 0, "H > F", "égal"))
    cat(sprintf("%s: H=%.3f, F=%.3f, diff=%.3f (%s)\n", 
                coh, m_dipl, f_dipl, diff, direction))
  }
}

cat("\n\n=== RÉGRESSIONS DE VÉRIFICATION ===\n\n")

# Régression avec tous les contrôles
cat("Régression complète :\n")
model1 <- lm(dipl ~ individual_cohort + sexe + as.factor(family_size) + 
               as.factor(mother_cs) + as.factor(father_cs), data = siblings)
print(summary(model1))

cat("\n\nRégression avec family_size en continu :\n")
model2 <- lm(dipl ~ individual_cohort + sexe + family_size + 
               as.factor(mother_cs) + as.factor(father_cs), data = siblings)
print(summary(model2))

cat("\n\nRégression avec more_than_2 :\n")
model3 <- lm(dipl ~ individual_cohort + sexe + more_than_2 + 
               as.factor(mother_cs) + as.factor(father_cs), data = siblings)
print(summary(model3))

cat("\n\nFirst stage (same_sex sur family_size) :\n")
first_stage <- lm(family_size ~ same_sex + father_cs + mother_cs, data = siblings)
print(summary(first_stage))

cat("\n\nReduced form (same_sex sur dipl) :\n")
reduced_form <- lm(dipl ~ same_sex + father_cs + mother_cs, data = siblings)
print(summary(reduced_form))

cat("\n\nRégression avec rang de naissance :\n")
model_with_rank <- lm(dipl ~ individual_cohort + sexe + as.factor(family_size) + 
                        birth_order + as.factor(mother_cs) + as.factor(father_cs), 
                      data = siblings)
print(summary(model_with_rank))


# Suppression de la variable de rang pour laisser les élèves la créer
siblings = siblings %>% 
  select(-birth_order, -cohort, -individual_cohort, -family_size, -same_sex, -more_than_2) 

# ------------------------------------------------------------------------------------
# 10. SAUVEGARDE DES DONNÉES
# ------------------------------------------------------------------------------------


saveRDS(siblings, "Homework/data/simulated_sibling_data.rds")

