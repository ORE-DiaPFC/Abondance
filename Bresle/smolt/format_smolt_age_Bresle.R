source("Bresle/smolt/fct_smolt_age.R")

readr::write_tsv(
  formate_smolt_age(),
  file = "Bresle/smolt/data/smolt_age_formatte.csv"
)
