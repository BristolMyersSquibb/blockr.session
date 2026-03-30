// Project navbar JavaScript handlers

Shiny.addCustomMessageHandler('blockr-get-nav-type', function(inputId) {
  var nav = performance.getEntriesByType('navigation');
  var type = nav.length > 0 ? nav[0].type : 'navigate';
  Shiny.setInputValue(inputId, type, {priority: 'event'});
});

// Update navbar title from server
Shiny.addCustomMessageHandler('blockr-update-navbar-title', function(title) {
  document.querySelectorAll('.blockr-navbar-title').forEach(function(el) {
    el.textContent = title;
  });
  document.querySelectorAll('.blockr-navbar-title-input').forEach(function(el) {
    el.value = title;
  });
});
