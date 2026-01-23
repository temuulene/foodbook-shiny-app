// Foodbook Internal Analysis Tool - JavaScript
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
    // Helper to update button text while preserving icon
    function updateButtonText(btnId, newText) {
      if (!newText) return;
      var btn = $('#' + btnId);
      if (!btn.length) return;
      
      // Find the icon (i or span with FA classes)
      var icon = btn.find('i, .fa, .fas, .far').first().clone();
      btn.empty();
      if (icon.length) {
        btn.append(icon).append(' ' + newText);
      } else {
        btn.text(newText);
      }
    }

    // Note: Most buttons are now handled via renderUI for reliable translation
    if (labels.cedars_clear) updateButtonText('cedars_clear', labels.cedars_clear);
  });

  // Custom message handler for updating tab names
  Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
    $('#nav-cedars-label').text(labels.cedars);
    $('#nav-data-info-label').text(labels.data_info);
    $('#nav-about-label').text(labels.about);
  });

  // Custom message handler for updating accordion titles
  Shiny.addCustomMessageHandler('update-accordion-titles', function(labels) {
    $('#acc-upload-label').text(labels.upload_exposure);
  });

  // Custom message handler for updating card headers
  Shiny.addCustomMessageHandler('update-card-headers', function(labels) {
    $('#card-results-label').text(labels.results || 'Results');
    $('#card-ref-settings-label').text(labels.reference_settings);
    $('#card-pop-snapshot-label').text(labels.population_snapshot);
    $('#card-cov-pt-label').text(labels.microdata_pt);
    $('#card-cov-month-label').text(labels.microdata_month);
    $('#card-about-label').text(labels.about_tool);
  });

  // Custom message handler for updating misc labels (help text, etc)
  Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
    if (labels.auto_detect_help) {
      $('#help-auto-detect').text(labels.auto_detect_help);
    }
  });
}

// Ensure handlers are registered
if (document.readyState === 'complete') {
  registerHandlers();
} else {
  $(window).on('load', registerHandlers);
}

