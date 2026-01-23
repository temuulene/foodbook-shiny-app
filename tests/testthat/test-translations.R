test_that("translation.json includes required about and summary keys", {
  translation_path <- file.path("..", "..", "translations", "translation.json")
  file_text <- paste(readLines(translation_path, encoding = "UTF-8"), collapse = "\n")

  expectations <- list(
    list(
      en = "classification_key",
      fr = "classification_key"
    ),
    list(
      en = "The Food Exposure Analysis Tool facilitates the comparison of case exposure data against population reference values from the Foodbook Report.",
      fr = "L'outil d'analyse de l'exposition alimentaire facilite la comparaison des donn\u00e9es d'exposition des cas avec les valeurs de r\u00e9f\u00e9rence de la population du rapport Foodbook."
    ),
    list(
      en = "Population reference values for Canada and Provinces/Territories.",
      fr = "Valeurs de r\u00e9f\u00e9rence de la population pour le Canada et les provinces/territoires."
    ),
    list(
      en = "Updated data where available.",
      fr = "Donn\u00e9es mises \u00e0 jour lorsque disponibles."
    ),
    list(
      en = "Results are classified based on statistical comparison (Binomial Exact Test):",
      fr = "Les r\u00e9sultats sont class\u00e9s selon une comparaison statistique (test binomial exact) :"
    ),
    list(
      en = "p-value \u2264 0.05. Observed proportion is significantly higher than reference.",
      fr = "valeur p \u2264 0,05. La proportion observ\u00e9e est significativement plus \u00e9lev\u00e9e que la r\u00e9f\u00e9rence."
    ),
    list(
      en = "p-value \u2264 0.10. Observed proportion is marginally higher than reference.",
      fr = "valeur p \u2264 0,10. La proportion observ\u00e9e est marginalement plus \u00e9lev\u00e9e que la r\u00e9f\u00e9rence."
    ),
    list(
      en = "Developed by Public Health Agency of Canada.",
      fr = "D\u00e9velopp\u00e9 par l'Agence de la sant\u00e9 publique du Canada."
    ),
    list(
      en = "Location:",
      fr = "Lieu :"
    ),
    list(
      en = "Age Groups:",
      fr = "Groupes d'\u00e2ge :"
    ),
    list(
      en = "Months:",
      fr = "Mois :"
    )
  )

  for (entry in expectations) {
    expect_true(
      grepl(paste0("\"en\": \"", entry$en, "\""), file_text, fixed = TRUE),
      info = paste("Missing en translation for:", entry$en)
    )
    expect_true(
      grepl(paste0("\"fr\": \"", entry$fr, "\""), file_text, fixed = TRUE),
      info = paste("Missing fr translation for:", entry$en)
    )
  }
})