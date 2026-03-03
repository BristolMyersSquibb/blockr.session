// Project navbar JavaScript handlers

// Update navbar title from server
Shiny.addCustomMessageHandler('blockr-update-navbar-title', function(title) {
  document.querySelectorAll('.blockr-navbar-title').forEach(function(el) {
    el.textContent = title;
  });
  document.querySelectorAll('.blockr-navbar-title-input').forEach(function(el) {
    el.value = title;
  });
});

// Trigger browser file download from a temporary Shiny URL
Shiny.addCustomMessageHandler('blockr-download-file', function(url) {
  var a = document.createElement('a');
  a.href = url;
  a.style.display = 'none';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
});

