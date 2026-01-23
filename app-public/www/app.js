// Foodbook Public Analysis Tool - JavaScript
// Extracted from app.R for maintainability

// Inject language selector into navbar on page load
$(document).ready(function() {
  setTimeout(function() {
    var langContainer = $('#lang_selector_container');

    if (langContainer.length > 0) {
      // Find the form-group div inside the container
      var formGroup = langContainer.find('.form-group').first();

      if (formGroup.length > 0) {
        // Create wrapper and append to navbar
        var wrapper = $('<div class="language-selector-wrapper"></div>');
        $('nav.navbar').first().css('position', 'relative').append(wrapper);

        // Move the entire form-group to the wrapper and make visible
        formGroup.appendTo(wrapper);
        formGroup.css('display', 'block');
        wrapper.css('display', 'block');
      }
    }
  }, 500);
});

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
    $('#acc-upload-label').text(labels.upload_exposure);
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

