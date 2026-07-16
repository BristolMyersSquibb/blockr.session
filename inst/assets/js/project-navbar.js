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

// --- Workflow list: keyboard nav + infinite scroll ------------------------
// The server renders a window of the (server-filtered) list plus a sentinel at
// the bottom; scrolling the sentinel into view asks the server for the next
// batch, which it appends. Search is a server-side Shiny input.

function blockrWorkflowPanel(el) {
  return el.closest('.blockr-tab-panel');
}

function blockrWorkflowSearchKey(event, input) {
  if (['ArrowDown', 'ArrowUp', 'Enter'].indexOf(event.key) === -1) return;

  var panel = blockrWorkflowPanel(input);
  if (!panel) return;

  var items = Array.prototype.slice.call(
    panel.querySelectorAll('.blockr-workflow-item')
  );
  if (!items.length) return;

  var current = -1;
  for (var i = 0; i < items.length; i++) {
    if (items[i].classList.contains('blockr-hi')) { current = i; break; }
  }

  if (event.key === 'Enter') {
    // Act only on an explicitly highlighted row, so a stray Enter in the search
    // box never navigates away from the current board.
    if (current >= 0) {
      event.preventDefault();
      items[current].click();
    }
    return;
  }

  event.preventDefault();
  if (current >= 0) items[current].classList.remove('blockr-hi');

  var next;
  if (event.key === 'ArrowDown') {
    next = current < 0 ? 0 : Math.min(current + 1, items.length - 1);
  } else {
    next = current <= 0 ? 0 : current - 1;
  }

  items[next].classList.add('blockr-hi');
  items[next].scrollIntoView({ block: 'nearest' });
}

// The IntersectionObserver on the sentinel (a fresh node each render). Root is
// the scrolling panel, so it fires near the bottom; a still-visible sentinel
// keeps materializing batches until the panel fills.
var blockrSentinelObserver = null;

function blockrArmWorkflowSentinel() {
  if (blockrSentinelObserver) blockrSentinelObserver.disconnect();

  var sentinel = document.querySelector('.blockr-workflow-sentinel');
  if (!sentinel) return;

  blockrSentinelObserver = new IntersectionObserver(
    function(entries) {
      entries.forEach(function(entry) {
        if (entry.isIntersecting) {
          Shiny.setInputValue(
            sentinel.getAttribute('data-input-id'),
            Date.now(),
            { priority: 'event' }
          );
        }
      });
    },
    { root: sentinel.closest('.blockr-tab-panel'), rootMargin: '120px' }
  );

  blockrSentinelObserver.observe(sentinel);
}

// Re-arm whenever the list re-renders. A MutationObserver on the static list
// container catches every server render (open, load-more, filter). We use it
// rather than shiny:value, which is a jQuery event that addEventListener misses.
function blockrWatchWorkflowLists() {
  document.querySelectorAll('.blockr-workflows-list').forEach(function(list) {
    if (list.dataset.blockrWatched) return;
    list.dataset.blockrWatched = '1';
    new MutationObserver(blockrArmWorkflowSentinel).observe(
      list,
      { childList: true, subtree: true }
    );
  });
}

if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', blockrWatchWorkflowLists);
} else {
  blockrWatchWorkflowLists();
}

// Focus the search when the dropdown opens, if the Workflows panel is visible.
document.addEventListener('shown.bs.dropdown', function(event) {
  blockrWatchWorkflowLists();

  var root = event.target.closest('.dropdown') || document;
  var input = root.querySelector(
    '.blockr-tab-panel:not(.blockr-tab-panel-hidden) .blockr-workflow-search-input'
  );
  if (!input) return;
  input.focus();
  input.select();
});
