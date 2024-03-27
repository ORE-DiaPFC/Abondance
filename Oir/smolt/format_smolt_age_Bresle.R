source("Bresle/smolt/fct_smolt_age.R")

readr::write_tsv(
  formate_smolt_age(
    chemin = "Oir/smolt/data",
    annee_debut = 1985
  ),
  file = "Oir/smolt/data/smolt_age_formatte.csv"
)
