// Project navbar JavaScript handlers

// --- Recent workflows ------------------------------------------------------
// The name menu lists the workflows this browser opened, the open one first
// in semibold, then the others newest first. The list is kept in localStorage under the app's path, so two apps on one server
// keep their own. The server never reads it: it only says which workflow
// loaded (add) and which one the URL named but no longer exists (drop). A row
// asks the server to open its workflow, as a row of the full list does.

var BLOCKR_RECENT_KEEP = 10;
var BLOCKR_RECENT_SHOWN = 8;

// the open workflow, listed first and not a pick
var blockrRecentCurrent = null;

function blockrRecentKey() {
  return 'blockr-recent:' + window.location.pathname;
}

function blockrRecentRead() {
  try {
    var list = JSON.parse(window.localStorage.getItem(blockrRecentKey()));
    return Array.isArray(list) ? list : [];
  } catch (e) {
    return [];
  }
}

function blockrRecentWrite(list) {
  try {
    window.localStorage.setItem(blockrRecentKey(), JSON.stringify(list));
  } catch (e) {
    // storage full or switched off: the menu just stays empty
  }
}

function blockrRecentSame(a, b) {
  return a.id === b.id && (a.user || '') === (b.user || '');
}

// The wording of format_time_ago() in R
function blockrTimeAgo(ms) {
  var secs = (Date.now() - ms) / 1000;
  var unit = function(n, word) {
    n = Math.floor(n);
    return n + ' ' + word + (n > 1 ? 's' : '') + ' ago';
  };
  if (secs < 60) return 'Just now';
  if (secs < 3600) return unit(secs / 60, 'min');
  if (secs < 86400) return unit(secs / 3600, 'hour');
  if (secs < 604800) return unit(secs / 86400, 'day');
  if (secs < 2592000) return unit(secs / 604800, 'week');
  return new Date(ms).toLocaleDateString(
    'en-US', { month: 'short', day: '2-digit', year: 'numeric' }
  );
}

function blockrRecentRender() {
  var isCurrent = function(e) {
    return blockrRecentCurrent !== null && blockrRecentSame(e, blockrRecentCurrent);
  };
  var all = blockrRecentRead();
  var entries = all.filter(isCurrent).concat(
    all.filter(function(e) { return !isCurrent(e); })
  ).slice(0, BLOCKR_RECENT_SHOWN);

  document.querySelectorAll('.blockr-recent-list').forEach(function(list) {
    var panel = list.closest('.blockr-tab-panel');
    var tpl = panel && panel.querySelector('.blockr-recent-row-template');
    var input = list.getAttribute('data-load-input');

    list.replaceChildren();

    if (!entries.length) {
      var empty = document.createElement('div');
      empty.className = 'blockr-recent-empty';
      empty.textContent = 'None yet';
      list.appendChild(empty);
      return;
    }

    entries.forEach(function(e) {
      var row = tpl.content.firstElementChild.cloneNode(true);
      row.querySelector('.blockr-workflow-name').textContent = e.name || e.id;
      row.querySelector('.blockr-workflow-meta').textContent =
        blockrTimeAgo(e.opened);
      var tab = row.querySelector('.blockr-open-newtab');
      tab.setAttribute('href', e.href);
      tab.addEventListener('click', function(ev) { ev.stopPropagation(); });
      if (isCurrent(e)) {
        row.classList.add('current');
        row.removeAttribute('tabindex');
        row.removeAttribute('role');
        list.appendChild(row);
        return;
      }
      row.addEventListener('click', function() {
        Shiny.setInputValue(
          input, { id: e.id, user: e.user || '' }, { priority: 'event' }
        );
      });
      row.addEventListener('keydown', function(ev) {
        if (ev.key === 'Enter' || ev.key === ' ') {
          ev.preventDefault();
          row.click();
        }
      });
      list.appendChild(row);
    });
  });
}

Shiny.addCustomMessageHandler('blockr-recent-add', function(msg) {
  blockrRecentCurrent = msg;
  var list = blockrRecentRead().filter(function(e) {
    return !blockrRecentSame(e, msg);
  });
  list.unshift({
    id: msg.id, user: msg.user, name: msg.name, href: msg.href,
    opened: Date.now()
  });
  blockrRecentWrite(list.slice(0, BLOCKR_RECENT_KEEP));
  blockrRecentRender();
});

Shiny.addCustomMessageHandler('blockr-recent-drop', function(msg) {
  blockrRecentWrite(blockrRecentRead().filter(function(e) {
    return !blockrRecentSame(e, msg);
  }));
  blockrRecentRender();
});

// Draw on every open, so the menu picks up what other tabs opened meanwhile
document.addEventListener('show.bs.dropdown', function(event) {
  var root = event.target.closest('.dropdown');
  if (root && root.querySelector('.blockr-recent-list')) blockrRecentRender();
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

// --- The save menu ----------------------------------------------------------
// Save, Save as and Download close the menu; Version history opens the recent
// versions beside it; Share swaps in the sharing panel. The menu opens on its
// first panel again next time.

document.addEventListener('click', function(event) {
  var row = event.target.closest('.blockr-save-actions .blockr-menu__item');
  if (!row || row.classList.contains('blockr-share-item')) return;
  // "Version history" opens its own menu, and stays
  if (row.classList.contains('blockr-versions-row')) {
    var box = row.closest('.blockr-save-versions');
    var open = !box.classList.contains('is-open');
    box.classList.toggle('is-open', open);
    row.setAttribute('aria-expanded', open ? 'true' : 'false');
    return;
  }
  var group = row.closest('.blockr-navbar-save-group');
  var toggle = group && group.querySelector('[data-bs-toggle="dropdown"]');
  if (toggle) bootstrap.Dropdown.getOrCreateInstance(toggle).hide();
});

document.addEventListener('hidden.bs.dropdown', function(event) {
  var group = event.target.closest('.blockr-navbar-save-group');
  if (!group) return;
  group.querySelectorAll('.blockr-save-versions').forEach(function(box) {
    box.classList.remove('is-open');
    var row = box.querySelector('.blockr-versions-row');
    if (row) row.setAttribute('aria-expanded', 'false');
  });
  group.querySelectorAll('.blockr-save-menu .blockr-tab-panel').forEach(
    function(p) {
      p.classList.toggle(
        'blockr-tab-panel-hidden', !p.classList.contains('blockr-save-panel')
      );
    }
  );
});

// Ctrl+S saves, Ctrl+Shift+S saves as a new workflow (Cmd on a Mac)
document.addEventListener('keydown', function(event) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey) return;
  if (event.key !== 's' && event.key !== 'S') return;

  var target = document.querySelector(
    event.shiftKey ? '.blockr-save-as-item' : '.blockr-save-item'
  );
  if (!target) return;

  event.preventDefault();
  target.click();
});
