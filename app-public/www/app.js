// Foodbook Public Analysis Tool - JavaScript
// Custom Shiny message handlers for dynamic UI updates

// Register custom message handlers safely
function registerHandlers() {
  if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
    setTimeout(registerHandlers, 100);
    return;
  }

  // Custom message handler for updating navbar title/brand
  Shiny.addCustomMessageHandler('update-navbar-title', function(title) {
    $('.navbar-brand').text(title);
  });

  // Custom message handler for updating button labels
  Shiny.addCustomMessageHandler('update-button-labels', function(labels) {
    $('#reset').text(labels.reset);
    $('button[id*=bookmark]').text(labels.bookmark);
    $('#download_plot').text(labels.download);
  });

  // Custom message handler for updating tab names
  Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
    $('#nav-analysis-label').text(labels.analysis);
    $('#nav-ref-data-label').text(labels.reference_data);
    $('#nav-data-info-label').text(labels.data_info);
    $('#nav-about-label').text(labels.about);
    $('#nav-viz-label').text(labels.visualization);
    $('#nav-results-nested-label').text(labels.results);
  });

  // Custom message handler for updating sidebar title
  Shiny.addCustomMessageHandler('update-sidebar-title', function(title) {
    $('#sidebar-analysis-title').text(title);
  });

  // Custom message handler for updating accordion titles
  Shiny.addCustomMessageHandler('update-accordion-titles', function(labels) {
    $('#acc-ref-settings-label').text(labels.reference_settings);
    $('#acc-upload-label').text(labels.upload_exposure);
    $('#acc-actions-label').text(labels.actions);
  });

  // Custom message handler for updating card headers
  Shiny.addCustomMessageHandler('update-card-headers', function(labels) {
    $('#card-exposure-input-label').text(labels.exposure_data_input);
    $('#card-ref-settings-label').text(labels.reference_settings);
    $('#card-pop-snapshot-label').text(labels.population_snapshot);
    $('#card-cov-pt-label').text(labels.microdata_pt);
    $('#card-cov-month-label').text(labels.microdata_month);
    $('#card-about-label').text(labels.about_tool);
    $('#card-ref-values-label').text(labels.reference_values);

    // Nested results tab in public app
    $('#nav-results-nested-label').text(labels.results);
  });

  // Custom message handler for updating misc labels (help text, file inputs, etc)
  Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
    // Update help text
    if (labels.enter_case_counts) {
      $('#help-enter-counts').text(labels.enter_case_counts);
    }
    if (labels.fb1_asterisk) {
      $('#footnote-fb1-label').text(labels.fb1_asterisk);
    }
    if (labels.fb1_only_asterisk) {
      $('#footnote-fb1-only-label').text(labels.fb1_only_asterisk);
    }
  });
}

// Ensure handlers are registered
if (document.readyState === 'complete') {
  registerHandlers();
} else {
  $(window).on('load', registerHandlers);
}
