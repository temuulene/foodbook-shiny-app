// Foodbook Internal Analysis Tool - JavaScript
// Extracted from app.R for maintainability

// Inject language selector into navbar on page load
$(document).ready(function() {
  setTimeout(function() {
    var langContainer = $('#lang_selector_container');
    console.log('Found lang container:', langContainer.length);

    if (langContainer.length > 0) {
      // Find the form-group div inside the container
      var formGroup = langContainer.find('.form-group').first();
      console.log('Found form group:', formGroup.length);

      if (formGroup.length > 0) {
        // Create wrapper and append to navbar
        var wrapper = $('<div class="language-selector-wrapper"></div>');
        $('nav.navbar').first().css('position', 'relative').append(wrapper);

        // Move the entire form-group to the wrapper and make visible
        formGroup.appendTo(wrapper);
        formGroup.css('display', 'block');
        wrapper.css('display', 'block');

        console.log('Language selector moved to navbar');
      }
    }
  }, 500);
});

// Custom message handler for updating button labels
Shiny.addCustomMessageHandler('update-button-labels', function(labels) {
  $('#download_plot').text(labels.download);
});

// Custom message handler for updating tab names
Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
  $('#nav-cedars-label').text(labels.cedars);
  $('#nav-data-info-label').text(labels.data_info);
  $('#nav-about-label').text(labels.about);
});

// Note: Sidebar titles are now handled via renderUI for reliable translation

// Custom message handler for updating misc labels (help text, etc)
// Note: File input labels and Browse button are now handled via renderUI
Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
  $('#help-auto-detect').text(labels.auto_detect_help);
});
