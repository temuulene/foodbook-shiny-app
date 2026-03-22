// Foodbook Public Analysis Tool - JavaScript
// Custom Shiny message handlers for dynamic UI updates

// Register custom message handlers safely
function registerHandlers() {
  if (typeof Shiny === 'undefined' || !Shiny.addCustomMessageHandler) {
    setTimeout(registerHandlers, 100);
    return;
  }

  // Helper: safely set text on an element if it exists
  function setText(selector, value) {
    if (!value) return;
    var el = $(selector);
    if (el.length) el.text(value);
  }

  // Custom message handler for updating navbar title/brand
  Shiny.addCustomMessageHandler('update-navbar-title', function(title) {
    setText('.navbar-brand', title);
  });

  // Custom message handler for updating button labels
  Shiny.addCustomMessageHandler('update-button-labels', function(labels) {
    setText('#reset', labels.reset);
    if (labels.bookmark) $('button[id*=bookmark]').text(labels.bookmark);
    setText('#download_plot', labels.download);
  });

  // Custom message handler for updating tab names
  Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
    setText('#nav-analysis-label', labels.analysis);
    setText('#nav-ref-data-label', labels.reference_data);
    setText('#nav-data-info-label', labels.data_info);
    setText('#nav-about-label', labels.about);
    setText('#nav-viz-label', labels.visualization);
    setText('#nav-results-nested-label', labels.results);
  });

  // Custom message handler for updating sidebar title
  Shiny.addCustomMessageHandler('update-sidebar-title', function(title) {
    setText('#sidebar-analysis-title', title);
  });

  // Custom message handler for updating accordion titles
  Shiny.addCustomMessageHandler('update-accordion-titles', function(labels) {
    setText('#acc-ref-settings-label', labels.reference_settings);
    setText('#acc-upload-label', labels.upload_exposure);
    setText('#acc-actions-label', labels.actions);
  });

  // Custom message handler for updating card headers
  Shiny.addCustomMessageHandler('update-card-headers', function(labels) {
    setText('#card-exposure-input-label', labels.exposure_data_input);
    setText('#card-ref-settings-label', labels.reference_settings);
    setText('#card-pop-snapshot-label', labels.population_snapshot);
    setText('#card-cov-pt-label', labels.microdata_pt);
    setText('#card-cov-month-label', labels.microdata_month);
    setText('#card-about-label', labels.about_tool);
    setText('#card-ref-values-label', labels.reference_values);
    setText('#nav-results-nested-label', labels.results);
  });

  // Custom message handler for updating misc labels (help text, file inputs, etc)
  Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
    setText('#help-enter-counts', labels.enter_case_counts);
    setText('#footnote-fb1-label', labels.fb1_asterisk);
    setText('#footnote-fb1-only-label', labels.fb1_only_asterisk);
  });
}

// Ensure handlers are registered
if (document.readyState === 'complete') {
  registerHandlers();
} else {
  $(window).on('load', registerHandlers);
}
