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

// --- Workflow list search (variant A) -------------------------------------
// Purely client-side: the server renders every workflow (data-name carries the
// lowercased title); typing hides non-matches, so filtering costs no round-trip.

function blockrWorkflowPanel(el) {
  return el.closest('.blockr-tab-panel');
}

function blockrFilterWorkflows(input) {
  var panel = blockrWorkflowPanel(input);
  if (!panel) return;

  var query = input.value.trim().toLowerCase();
  var items = panel.querySelectorAll('.blockr-workflow-item');
  var shown = 0;

  items.forEach(function(item) {
    var name = item.getAttribute('data-name') || '';
    var match = !query || name.indexOf(query) !== -1;
    item.style.display = match ? '' : 'none';
    item.classList.remove('blockr-hi');
    if (match) shown++;
  });

  var noresults = panel.querySelector('.blockr-workflow-noresults');
  if (noresults) {
    noresults.style.display = (shown === 0 && items.length > 0) ? 'block' : 'none';
  }

  var count = panel.querySelector('.blockr-workflow-count');
  if (count) {
    if (!items.length) {
      count.textContent = '';
    } else {
      count.textContent = query ? (shown + ' / ' + items.length)
                                : String(items.length);
    }
  }
}

function blockrWorkflowSearchKey(event, input) {
  if (['ArrowDown', 'ArrowUp', 'Enter'].indexOf(event.key) === -1) return;

  var panel = blockrWorkflowPanel(input);
  if (!panel) return;

  var visible = Array.prototype.filter.call(
    panel.querySelectorAll('.blockr-workflow-item'),
    function(item) { return item.style.display !== 'none'; }
  );
  if (!visible.length) return;

  var current = -1;
  for (var i = 0; i < visible.length; i++) {
    if (visible[i].classList.contains('blockr-hi')) { current = i; break; }
  }

  if (event.key === 'Enter') {
    event.preventDefault();
    (visible[current] || visible[0]).click();
    return;
  }

  event.preventDefault();
  if (current >= 0) visible[current].classList.remove('blockr-hi');

  var next;
  if (event.key === 'ArrowDown') {
    next = current < 0 ? 0 : Math.min(current + 1, visible.length - 1);
  } else {
    next = current <= 0 ? 0 : current - 1;
  }

  visible[next].classList.add('blockr-hi');
  visible[next].scrollIntoView({ block: 'nearest' });
}

// Focus the search when the dropdown opens (only if the Workflows panel is the
// visible one) and re-apply any active filter after the list re-renders.
document.addEventListener('shown.bs.dropdown', function(event) {
  // shown.bs.dropdown fires on the toggle button; the search input lives in the
  // sibling menu, so scope the lookup to the shared .dropdown container.
  var root = event.target.closest('.dropdown') || document;
  var input = root.querySelector(
    '.blockr-tab-panel:not(.blockr-tab-panel-hidden) .blockr-workflow-search-input'
  );
  if (!input) return;
  blockrFilterWorkflows(input);
  input.focus();
  input.select();
});
