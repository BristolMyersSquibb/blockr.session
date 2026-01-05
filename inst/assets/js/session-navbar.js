// Session navbar JavaScript handlers

// Update navbar title from server
Shiny.addCustomMessageHandler('blockr-update-navbar-title', function(title) {
  document.querySelectorAll('.blockr-navbar-title').forEach(function(el) {
    el.textContent = title;
  });
  document.querySelectorAll('.blockr-navbar-title-input').forEach(function(el) {
    el.value = title;
  });
});

// Update save status from server
Shiny.addCustomMessageHandler('blockr-update-save-status', function(status) {
  document.querySelectorAll('.blockr-navbar-meta').forEach(function(el) {
    el.textContent = status;
  });
});
