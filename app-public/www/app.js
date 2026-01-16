// Foodbook Public Analysis Tool - JavaScript
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
// bslib's page_navbar creates nav items without specific IDs, 
// so we find them by their position in the navbar
Shiny.addCustomMessageHandler('update-tab-names', function(labels) {
  // Helper function to update a nav link's text while preserving the icon
  function updateNavText(navLink, newText) {
    if (!navLink || !newText) return;
    
    // Find the icon element if it exists
    var icon = navLink.find('i, svg, .fa, .fas, .far, .fab').first().clone();
    
    // Clear the nav link and add back icon + text
    navLink.empty();
    if (icon.length) {
      navLink.append(icon);
      navLink.append(' ' + newText);
    } else {
      navLink.text(newText);
    }
  }
  
  // Main nav items (excluding dropdown items)
  var navItems = $('nav.navbar .nav-item > .nav-link').not('.dropdown-toggle');
  
  // Update by position: Analysis (0), Reference Data (1), Data Info (2), About (3)
  if (navItems.length >= 1) updateNavText(navItems.eq(0), labels.analysis);
  if (navItems.length >= 2) updateNavText(navItems.eq(1), labels.reference_data);
  if (navItems.length >= 3) updateNavText(navItems.eq(2), labels.data_info);
  if (navItems.length >= 4) updateNavText(navItems.eq(3), labels.about);
});

// Custom message handler for updating sidebar title
Shiny.addCustomMessageHandler('update-sidebar-title', function(title) {
  $('#sidebar-analysis-title').text(title);
});

// Custom message handler for updating accordion titles
Shiny.addCustomMessageHandler('update-accordion-titles', function(labels) {
  $('#accordion-upload-exposure-label').text(labels.upload_exposure);
});

// Custom message handler for updating card headers
Shiny.addCustomMessageHandler('update-card-headers', function(labels) {
  $('#card-exposure-data-header').text(labels.exposure_data_input);
  $('#card-reference-settings-header').text(labels.reference_settings);
  $('#card-population-snapshot-header').text(labels.population_snapshot);
  $('#card-microdata-pt-header').text(labels.microdata_pt);
  $('#card-microdata-month-header').text(labels.microdata_month);
  $('#card-about-header').text(labels.about_tool);
  $('#card-reference-values-header').text(labels.reference_values);
});

// Custom message handler for updating misc labels (help text, file inputs, etc)
Shiny.addCustomMessageHandler('update-misc-labels', function(labels) {
  // Update help text (but NOT file input controls - they're handled by renderUI)
  $('#help-enter-case-counts').text(labels.enter_case_counts);
});
