# Module: Results Table
# Renders the main results table with DataTables

mod_results_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    withSpinner(
      uiOutput(ns("table_container"), width = "100%"),
      type = 4
    )
  )
}

mod_results_table_server <- function(id, results_data_reactive, get_tr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$table_container <- renderUI({
      tr <- get_tr()
      res <- results_data_reactive()
      
      # Check for NULL (loading or empty)
      if (is.null(res)) {
        # Optional: could check a 'loading' flag, but often NULL implies just not ready
        return(NULL) # Or placeholder
      }
      
      tagList(
        DTOutput(ns("results_table"), width = "100%"),
        helpText(tr$t("* Exposures from Foodbook 1.0"), style = "font-size: 0.8rem; margin-top: 0.5rem; color: #6c757d;")
      )
    })
    
    output$results_table <- renderDT(server = FALSE, {
      res <- results_data_reactive()
      if (is.null(res)) return(NULL)
      tr <- get_tr()
      
      # Formatting logic (similar to original app)
      # Assume 'res' comes in with columns: Reference Scope, Exposure, Total Valid, Yes, Probably, No, DK, Observed %, Reference %, P-Value, Classification
      
      # Sort by P-Value
      res_formatted <- res %>%
         mutate(
           classification_key = tolower(trimws(ifelse(
             is.na(Classification),
             "",
             as.character(Classification)
           ))),
           classification_key = case_match(
              classification_key,
              "alerte" ~ "alert",
              "limite" ~ "borderline",
              .default = classification_key
            ),
           alert_rank = if_else(
             classification_key %in% c("alert", "borderline"),
             0L,
             1L
           ),
           p_value_sort = if_else(is.na(`P-Value`), 999, `P-Value`),
           `Observed %` = round(`Observed %` * 100, 2),
           `Reference %` = if_else(
             is.na(`Reference %`),
             "-",
             as.character(round(`Reference %`, 2))
           ),
           `P-Value` = if_else(is.na(`P-Value`), "-", as.character(round(`P-Value`, 4)))
         ) %>%
         arrange(alert_rank, p_value_sort) %>%
         select(-alert_rank, -p_value_sort)
      
      # Translate column names
      # These must exist in translation.json
      col_names <- names(res_formatted)
      translated_cols <- purrr::map_chr(col_names, ~ tr$t(.))
      names(res_formatted) <- translated_cols
      
      # Generate filename
      filename <- paste0("foodbook_results_", Sys.Date())
      
      # Find classification column index for hiding & styling
      hidden_cols <- which(col_names == "classification_key") - 1
      key_index <- hidden_cols
      
      # Visible columns for export (exclude hidden classification_key)
      visible_col_indices <- setdiff(seq_along(col_names) - 1, hidden_cols)

      datatable(
        res_formatted,
        options = list(
          pageLength = 50,
          dom = 'Bfrtip',
          order = list(),
          language = list(
             search = tr$t("Search:"),
             lengthMenu = paste0(tr$t("Show"), " _MENU_ ", tr$t("entries")),
             info = paste0(tr$t("Showing"), " _START_ ", tr$t("to"), " _END_ ", tr$t("of"), " _TOTAL_ ", tr$t("entries")),
             zeroRecords = tr$t("No data available"),
             paginate = list(
               previous = tr$t("Previous"),
               `next` = tr$t("Next")
             )
          ),
          rowCallback = JS(sprintf(
            "function(row, data) {
               var key = data[%1$d];
               var bgColor = null;
               var borderColor = null;
               
               if (key === 'alert') {
                 bgColor = '#f8d7da';
                 borderColor = '#dc3545';
               } else if (key === 'borderline') {
                 bgColor = '#fff3cd';
                 borderColor = '#fd7e14';
               }
               
               if (bgColor) {
                 // Target the row itself for the border and general bg
                 row.style.setProperty('background-color', bgColor, 'important');
                 row.style.setProperty('border-left', '5px solid ' + borderColor, 'important');
                 
                 // Target individual cells to override Bootstrap's cell-specific bg (e.g. from striping)
                 var cells = row.getElementsByTagName('td');
                 for (var i = 0; i < cells.length; i++) {
                   cells[i].style.setProperty('background-color', bgColor, 'important');
                 }
               }
             }",
             key_index
          )),
          columnDefs = list(list(visible = FALSE, targets = hidden_cols)),
          buttons = list(
            list(extend = 'csv', filename = filename, exportOptions = list(columns = visible_col_indices), text = tr$t("Export")),
            list(extend = 'copy', exportOptions = list(columns = visible_col_indices), text = tr$t("Copy")),
            list(extend = 'print', exportOptions = list(columns = visible_col_indices), text = tr$t("Print"))
          )
        ),
        extensions = 'Buttons',
        rownames = FALSE
      )
    })
  })
}
