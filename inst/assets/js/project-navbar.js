// Project navbar JavaScript handlers

(function() {
  var nav = performance.getEntriesByType('navigation');
  if (nav.length > 0 && nav[0].type === 'reload') {
    history.replaceState({}, '', window.location.pathname);
  }
})();

// Update navbar title from server
Shiny.addCustomMessageHandler('blockr-update-navbar-title', function(title) {
  document.querySelectorAll('.blockr-navbar-title').forEach(function(el) {
    el.textContent = title;
  });
  document.querySelectorAll('.blockr-navbar-title-input').forEach(function(el) {
    el.value = title;
  });
});
