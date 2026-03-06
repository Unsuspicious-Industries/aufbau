// margin.js — Anchor-matching margin note positioner
// Pairs .margin-anchor[data-margin] with .margin-note / .margin-interactive
// of the same data-margin value, positions them in a sidebar column,
// and draws a faint dashed connector line between anchor and note.
(function () {
  "use strict";

  var MARGIN_GAP   = 18;  // gap between content edge and column
  var MIN_VIEWPORT = 1100; // hide below this px, matches CSS media query

  // ── helpers ──────────────────────────────────────────────────────────────

  // Absolute top offset of an element relative to the document.
  // Uses getBoundingClientRect for accuracy (accounts for transforms, margins, etc.)
  function absTop(el) {
    var rect = el.getBoundingClientRect();
    var scrollTop = window.pageYOffset || document.documentElement.scrollTop;
    return rect.top + scrollTop;
  }

  // ── main layout function ──────────────────────────────────────────────────

  function layoutMargins() {
    if (window.innerWidth < MIN_VIEWPORT) return;

    var column = document.getElementById("margin-column");
    if (!column) return;

    var pageEl = document.querySelector(".page");
    if (!pageEl) return;

    var wrapper = document.getElementById("page-wrapper");
    if (!wrapper) return;

    // Position the column flush against the right edge of .page
    var pageRect = pageEl.getBoundingClientRect();
    var pageRight = pageRect.right + (window.pageXOffset || 0);
    var wrapperLeft = wrapper.getBoundingClientRect().left + (window.pageXOffset || 0);

    column.style.left = (pageRight - wrapperLeft + MARGIN_GAP) + "px";

    // Remove old connectors
    var oldSvgs = column.querySelectorAll(".margin-connector");
    for (var i = 0; i < oldSvgs.length; i++) oldSvgs[i].parentNode.removeChild(oldSvgs[i]);

    // Collect anchors
    var anchorEls = document.querySelectorAll(".margin-anchor[data-margin]");
    var anchorMap = {};
    for (var i = 0; i < anchorEls.length; i++) {
      anchorMap[anchorEls[i].getAttribute("data-margin")] = anchorEls[i];
    }

    // Collect notes (both kinds)
    var noteEls = document.querySelectorAll(
      ".margin-note[data-margin], .margin-interactive[data-margin]"
    );
    if (noteEls.length === 0) return;

    var notes = [];
    for (var i = 0; i < noteEls.length; i++) notes.push(noteEls[i]);

    // Sort notes by their anchor's vertical position
    notes.sort(function (a, b) {
      var aa = anchorMap[a.getAttribute("data-margin")];
      var ba = anchorMap[b.getAttribute("data-margin")];
      if (!aa || !ba) return 0;
      return absTop(aa) - absTop(ba);
    });

    // Move all notes into the column (if not already there)
    for (var i = 0; i < notes.length; i++) {
      if (notes[i].parentNode !== column) {
        column.appendChild(notes[i]);
      }
    }

    // The column is position:absolute top:0 inside #page-wrapper.
    // So note.style.top is relative to wrapper's top edge.
    // We compute: anchorAbsTop - wrapperAbsTop = anchor offset from wrapper top.
    var wrapperAbsTop = absTop(wrapper);

    // Place notes, stacking with minimum gap if they would overlap
    var cursor = 0;

    for (var i = 0; i < notes.length; i++) {
      var note = notes[i];
      var mid = note.getAttribute("data-margin");
      var anchor = anchorMap[mid];
      if (!anchor) continue;

      var anchorTop = absTop(anchor);
      var targetTop = anchorTop - wrapperAbsTop;
      var top       = Math.max(cursor + 6, targetTop);

      note.style.position = "absolute";
      note.style.top = top + "px";

      cursor = top + note.offsetHeight;

      drawConnector(column, anchor, note, wrapperAbsTop);
    }
  }

  // Draw a faint dashed connector between the anchor and its margin note.
  function drawConnector(column, anchor, note, wrapperAbsTop) {
    var noteTop    = parseFloat(note.style.top) || 0;
    var noteHeight = note.offsetHeight;

    // y coords in wrapper-relative space
    var y1 = absTop(anchor) - wrapperAbsTop + (anchor.offsetHeight || 4) / 2;
    var y2 = noteTop + noteHeight / 2;

    var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
    svg.setAttribute("class", "margin-connector");

    var minY = Math.min(y1, y2);
    var maxY = Math.max(y1, y2);
    var h    = maxY - minY || 1;

    svg.style.cssText =
      "position:absolute;" +
      "left:" + (-MARGIN_GAP) + "px;" +
      "top:" + minY + "px;" +
      "width:" + (MARGIN_GAP + 4) + "px;" +
      "height:" + (h + 4) + "px;" +
      "overflow:visible;" +
      "pointer-events:none;";

    var line = document.createElementNS("http://www.w3.org/2000/svg", "line");
    line.setAttribute("x1", "0");
    line.setAttribute("y1", String(y1 - minY));
    line.setAttribute("x2", String(MARGIN_GAP));
    line.setAttribute("y2", String(y2 - minY));

    svg.appendChild(line);
    column.appendChild(svg);
  }

  // ── column setup ──────────────────────────────────────────────────────────

  function setupColumn() {
    if (document.getElementById("margin-column")) return;

    var wrapper = document.getElementById("page-wrapper");
    if (!wrapper) return;

    var wStyle = window.getComputedStyle(wrapper);
    if (wStyle.position === "static") {
      wrapper.style.position = "relative";
    }

    var col = document.createElement("div");
    col.id = "margin-column";
    wrapper.appendChild(col);
  }

  // Debounced layout
  var layoutTimer = null;
  function scheduleLayout(delay) {
    clearTimeout(layoutTimer);
    layoutTimer = setTimeout(function () {
      requestAnimationFrame(layoutMargins);
    }, delay || 50);
  }

  // ── init ─────────────────────────────────────────────────────────────────

  function init() {
    setupColumn();

    // Do NOT layout immediately -- wait for window load so that
    // MathJax, fonts, and images are fully rendered.
    // This avoids the "notes positioned too high before MathJax reflows" issue.

    window.addEventListener("load", function () {
      scheduleLayout(200);
      // Extra pass in case MathJax does late rendering after load
      setTimeout(function () { scheduleLayout(100); }, 1000);
      setTimeout(function () { scheduleLayout(100); }, 2500);
    });

    // Re-layout on resize with debounce
    window.addEventListener("resize", function () {
      scheduleLayout(120);
    });

    // MathJax 3: the async script may not have loaded yet at DOMContentLoaded.
    var mjPollCount = 0;
    var mjPoll = setInterval(function () {
      mjPollCount++;
      if (mjPollCount > 50) { clearInterval(mjPoll); return; }
      if (window.MathJax && MathJax.startup && MathJax.startup.promise) {
        clearInterval(mjPoll);
        MathJax.startup.promise.then(function () {
          scheduleLayout(200);
          setTimeout(function () { scheduleLayout(100); }, 500);
        });
      }
    }, 100);

    // MutationObserver: catch late DOM changes (MathJax, lazy content)
    var contentEl = document.getElementById("mdbook-content") || document.querySelector(".content");
    if (contentEl && window.MutationObserver) {
      var mutTimer = null;
      var observer = new MutationObserver(function () {
        clearTimeout(mutTimer);
        mutTimer = setTimeout(function () {
          requestAnimationFrame(layoutMargins);
        }, 300);
      });
      observer.observe(contentEl, { childList: true, subtree: true });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init);
  } else {
    init();
  }
})();
