// Project navbar JavaScript handlers

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

// --- Manage-workflows modal: server-windowed table --------------------------
// The modal renders a server-side window of the (server-filtered) list plus a
// sentinel row; scrolling it into view asks for the next batch. Selection is
// authoritative on the server, so select-all and delete act over the whole
// filtered set rather than only the loaded rows: each checkbox reports its
// toggle, select-all reports one event, and the server pushes back the count.

var blockrModalSentinelObserver = null;

function blockrArmModalSentinel(modal) {
  if (blockrModalSentinelObserver) blockrModalSentinelObserver.disconnect();

  var sentinel = modal.querySelector('.blockr-wf-modal-sentinel');
  if (!sentinel) return;

  blockrModalSentinelObserver = new IntersectionObserver(
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
    {
      root: modal.querySelector('.blockr-wf-table-container'),
      rootMargin: '120px'
    }
  );

  blockrModalSentinelObserver.observe(sentinel);
}

function blockrInitWorkflowsModal(root) {
  var modal = root.classList && root.classList.contains('blockr-wf-manage-modal')
    ? root
    : root.querySelector('.blockr-wf-manage-modal');
  if (!modal || modal.dataset.blockrInit) return;
  modal.dataset.blockrInit = '1';

  var toggleInput = modal.getAttribute('data-toggle-input');
  var selectAllInput = modal.getAttribute('data-select-all-input');
  var deleteInput = modal.getAttribute('data-delete-input');
  var container = modal.querySelector('.blockr-wf-table-container');

  // Clear any server-side filter left over from a previous open.
  Shiny.setInputValue(
    modal.getAttribute('data-filter-input'), '', { priority: 'event' }
  );

  // Delegated so rows materialized on scroll are covered too.
  container.addEventListener('change', function(e) {
    var cb = e.target;
    if (!cb.classList.contains('blockr-wf-select')) return;
    Shiny.setInputValue(
      toggleInput,
      { id: cb.getAttribute('data-id'), checked: cb.checked, nonce: Date.now() },
      { priority: 'event' }
    );
  });

  var selectAll = modal.querySelector('.blockr-wf-select-all');
  selectAll.addEventListener('change', function(e) {
    modal.querySelectorAll('.blockr-wf-select').forEach(function(cb) {
      cb.checked = e.target.checked;
    });
    Shiny.setInputValue(
      selectAllInput,
      { checked: e.target.checked, nonce: Date.now() },
      { priority: 'event' }
    );
  });

  var delBtn = document.getElementById(modal.getAttribute('data-delete-btn'));
  delBtn.addEventListener('click', function() {
    var n = parseInt(modal.dataset.selCount || '0', 10);
    if (n > 0 && confirm('Delete ' + n + ' workflow(s)?')) {
      Shiny.setInputValue(deleteInput, Date.now(), { priority: 'event' });
    }
  });

  blockrArmModalSentinel(modal);
  new MutationObserver(function() {
    blockrArmModalSentinel(modal);
  }).observe(container, { childList: true, subtree: true });
}

// The server pushes the authoritative selection count on every change; reflect
// it in the Delete/Download buttons and the select-all box.
Shiny.addCustomMessageHandler('blockr-modal-selection', function(msg) {
  var modal = document.querySelector('.blockr-wf-manage-modal');
  if (!modal) return;

  modal.dataset.selCount = msg.count;

  var delBtn = document.getElementById(modal.getAttribute('data-delete-btn'));
  if (delBtn) {
    delBtn.style.display = msg.count > 0 ? '' : 'none';
    delBtn.textContent = 'Delete (' + msg.count + ')';
  }

  var dlWrap = document.getElementById(modal.getAttribute('data-download-wrap'));
  if (dlWrap) {
    dlWrap.style.visibility = msg.count > 0 ? 'visible' : 'hidden';
    dlWrap.style.position = msg.count > 0 ? '' : 'absolute';
    var dlLink = dlWrap.querySelector('a');
    if (dlLink) dlLink.textContent = 'Download (' + msg.count + ')';
  }

  var selectAll = modal.querySelector('.blockr-wf-select-all');
  if (selectAll) {
    selectAll.checked = msg.count > 0 && msg.count === msg.total;
    selectAll.indeterminate = msg.count > 0 && msg.count < msg.total;
  }
});

document.addEventListener('shown.bs.modal', function(event) {
  blockrInitWorkflowsModal(event.target);
});

document.addEventListener('hidden.bs.modal', function(event) {
  var modal = event.target.querySelector('.blockr-wf-manage-modal');
  if (modal) {
    Shiny.setInputValue(
      modal.getAttribute('data-closed-input'), Date.now(), { priority: 'event' }
    );
  }
});
