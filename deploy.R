ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

ensure_pkg("rsconnect")

# Detect whether any accounts are registered on this machine
if (length(rsconnect::accounts()$name) == 0) {
  message("No rsconnect accounts registered on this R installation.\n",
          "Two ways to authenticate:\n",
          "  1) Get a token & secret from shinyapps.io > Account > Tokens, then run:\n",
          "     rsconnect::setAccountInfo(name='temuulen', token='<TOKEN>', secret='<SECRET>')\n",
          "  2) Or run rsconnect::connectUser(account='temuulen', server='shinyapps.io') ",
          "to complete auth in a browser.\n\n",
          "Once authenticated, re-run this script to deploy.")
  quit(save = "no")
}

# If we reach here, an account is registered; deploy using the existing record
rsconnect::deployApp(
  appName = "foodbook-shiny-app",
  account = "temuulen",
  forceUpdate = TRUE,
  lint = FALSE
)

