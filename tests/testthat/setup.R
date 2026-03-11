# Test setup for Foodbook Shiny App

source(file.path("..", "..", "src", "foodbook_backend.R"))
source(file.path("..", "..", "src", "i18n_helper.R"))
source(file.path("..", "..", "src", "common_server.R"))
source(file.path("..", "..", "src", "app_public_helpers.R"))

# Source modules so tests don't need to re-source individually
source(file.path("..", "..", "src", "modules", "exposure_module.R"))
source(file.path("..", "..", "src", "modules", "mod_ref_settings.R"))
source(file.path("..", "..", "src", "modules", "mod_results_table.R"))
source(file.path("..", "..", "src", "modules", "mod_visualization.R"))
source(file.path("..", "..", "src", "modules", "mod_about.R"))
source(file.path("..", "..", "src", "modules", "mod_data_info.R"))
source(file.path("..", "..", "src", "modules", "language_selector_module.R"))
