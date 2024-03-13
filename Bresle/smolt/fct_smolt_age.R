#' mise en forme des données d'âge
formate_smolt_age <- function(
  chemin = "Bresle/smolt/data",
  fichier = "smolt_age_brut.csv",
  annee_debut = 1982
) {

  donnees_brutes <- readr::read_tsv(
    file = paste0(chemin, "/", fichier)
  )

  donnees_brutes <- donnees_brutes |>
    dplyr::mutate(
      age_riv = dplyr::if_else(age_riv == ".", NA, age_riv)
    ) |>
    dplyr::group_by(annee, cap_lf, age_riv) |>
    dplyr::summarise(nb = sum(nb))

  annees_selectionnees <- annee_debut:max(donnees_brutes$annee)

  donnees <- donnees_brutes |>
    dplyr::filter(annee %in% annees_selectionnees)

  tailles_selectionnees <-
    min(donnees$cap_lf, na.rm = TRUE):max(donnees$cap_lf, na.rm = TRUE)

  age <- unique(donnees$age_riv)

  matrice_complete <- expand.grid(
    age = age,
    taille = tailles_selectionnees,
    annee = annees_selectionnees
  )  |>
    dplyr::as_tibble()

  donnees_completes <- matrice_complete |>
    dplyr::left_join(
      donnees,
      by = dplyr::join_by(
        age == age_riv,
        taille == cap_lf,
        annee == annee
      )
    ) |>
    dplyr::mutate(
      nb = tidyr::replace_na(nb, replace = 0)
    )

  donnees_formatees <- donnees_completes |>
    tidyr::pivot_wider(
      names_from = age,
      values_from = nb
    ) |>
    dplyr::select(annee, taille, `NA`, `1`, `2`, `3`)

  return(donnees_formatees)
}
