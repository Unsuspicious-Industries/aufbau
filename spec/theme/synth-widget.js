// synth-widget.js — Interactive synthesizer widget for margin notes.
//
// Each .margin-interactive[data-steps] element becomes a live token-stepping
// widget.  The user clicks tokens to append them; each click triggers a
// short synthesized audio blip (Web Audio API, no external files needed).
// The parse-state display updates inline — the "video" is the widget itself.
//
// data-steps JSON schema:
//   {
//     "label": "string",          // optional header label
//     "input": "string",          // initial input string (default "")
//     "tokens": ["t1","t2",...],  // available tokens at each step OR
//     "steps": [                  // scripted sequence (optional)
//       { "token": "x", "tokens": ["a","b","c"], "display": "..." },
//       ...
//     ]
//   }
//
// If "steps" is provided, each click advances through the scripted sequence.
// If only "tokens" is provided, any token can be clicked in any order (free mode).

(function () {
  "use strict";

  // ── Audio ─────────────────────────────────────────────────────────────────

  var _ctx = null;

  function audioCtx() {
    if (!_ctx) {
      try {
        _ctx = new (window.AudioContext || window.webkitAudioContext)();
      } catch (e) {
        return null;
      }
    }
    // Resume if suspended (browser autoplay policy)
    if (_ctx.state === "suspended") _ctx.resume();
    return _ctx;
  }

  // Play a short pitched blip when a token is clicked.
  // freq: fundamental Hz, type: oscillator type, duration: seconds
  function playBlip(freq, type, duration) {
    var ctx = audioCtx();
    if (!ctx) return;

    var osc  = ctx.createOscillator();
    var gain = ctx.createGain();

    osc.connect(gain);
    gain.connect(ctx.destination);

    osc.type      = type || "triangle";
    osc.frequency.setValueAtTime(freq || 440, ctx.currentTime);

    // Short pluck envelope
    gain.gain.setValueAtTime(0, ctx.currentTime);
    gain.gain.linearRampToValueAtTime(0.18, ctx.currentTime + 0.01);
    gain.gain.exponentialRampToValueAtTime(0.001, ctx.currentTime + (duration || 0.18));

    osc.start(ctx.currentTime);
    osc.stop(ctx.currentTime + (duration || 0.18) + 0.02);
  }

  // Map a token string to a pitch by hashing its chars
  function tokenFreq(token) {
    var base  = 220; // A3
    var ratio = 1;
    for (var i = 0; i < token.length; i++) {
      ratio += token.charCodeAt(i) * 0.003;
    }
    // Snap to a pentatonic scale (semitone offsets: 0,2,4,7,9)
    var pentatonic = [0, 2, 4, 7, 9, 12, 14, 16, 19, 21];
    var idx = Math.round(ratio * 7) % pentatonic.length;
    return base * Math.pow(2, pentatonic[idx] / 12);
  }

  // "Completion success" chord — three stacked blips
  function playComplete() {
    playBlip(523, "sine",     0.25);
    setTimeout(function () { playBlip(659, "sine", 0.22); }, 40);
    setTimeout(function () { playBlip(784, "sine", 0.20); }, 80);
  }

  // ── Widget builder ────────────────────────────────────────────────────────

  function buildWidget(el) {
    var rawSteps  = el.dataset.steps  || "{}";
    var rawLabel  = el.dataset.label  || "";

    var config;
    try {
      config = JSON.parse(rawSteps.replace(/&quot;/g, '"'));
    } catch (e) {
      config = {};
    }

    var label      = config.label  || rawLabel || "synthesizer";
    var initInput  = config.input  || "";
    var scripted   = Array.isArray(config.steps) && config.steps.length > 0;
    var freeTokens = config.tokens || [];

    // State
    var state = {
      input:  initInput,
      stepIdx: 0,
      done:   false,
    };

    // ── DOM ──────────────────────────────────────────────────────────────────

    // Label
    var labelEl = document.createElement("span");
    labelEl.className = "margin-widget-label";
    labelEl.textContent = label;

    // Input display (the "video" — changes on every click)
    var inputEl = document.createElement("div");
    inputEl.className = "margin-widget-input";
    inputEl.textContent = state.input || "\u00a0";

    // Token buttons container
    var tokensEl = document.createElement("div");
    tokensEl.className = "margin-widget-tokens";

    // Step counter
    var stepEl = document.createElement("span");
    stepEl.className = "margin-widget-step";

    // Reset
    var resetEl = document.createElement("span");
    resetEl.className = "margin-widget-reset";
    resetEl.textContent = "reset";
    resetEl.title = "start over";

    el.innerHTML = "";
    el.appendChild(labelEl);
    el.appendChild(inputEl);
    el.appendChild(tokensEl);
    el.appendChild(stepEl);
    el.appendChild(resetEl);

    // ── Render ───────────────────────────────────────────────────────────────

    function currentTokens() {
      if (scripted) {
        if (state.done) return [];
        var step = config.steps[state.stepIdx];
        return step ? (step.tokens || [step.token]) : [];
      }
      return freeTokens;
    }

    function currentDisplay() {
      if (scripted && !state.done) {
        var step = config.steps[state.stepIdx];
        if (step && step.display) return step.display;
      }
      return state.input || "\u00a0";
    }

    function render() {
      inputEl.textContent = currentDisplay();

      tokensEl.innerHTML = "";
      var tokens = currentTokens();
      tokens.forEach(function (tok) {
        var btn = document.createElement("span");
        btn.className = "margin-token";
        btn.textContent = tok;
        btn.addEventListener("click", function () {
          onToken(tok);
        });
        tokensEl.appendChild(btn);
      });

      if (scripted) {
        var total = config.steps.length;
        stepEl.textContent = state.done
          ? "complete \u2713"
          : (state.stepIdx + 1) + " / " + total;
      } else {
        stepEl.textContent = state.input.length > 0
          ? state.input.length + " chars"
          : "";
      }
    }

    function onToken(tok) {
      if (state.done) return;

      // Audio
      playBlip(tokenFreq(tok), "triangle", 0.15);

      if (scripted) {
        // Advance to next step (tok is the chosen/only token at this step)
        var step = config.steps[state.stepIdx];
        state.input += (state.input && tok ? " " : "") + tok;
        state.stepIdx++;
        if (state.stepIdx >= config.steps.length) {
          state.done = true;
          setTimeout(playComplete, 80);
        }
      } else {
        // Free mode: append token
        state.input += (state.input ? " " : "") + tok;
      }

      render();
    }

    function reset() {
      state.input   = initInput;
      state.stepIdx = 0;
      state.done    = false;
      playBlip(180, "sine", 0.12);
      render();
    }

    resetEl.addEventListener("click", reset);

    render();
  }

  // ── Init ─────────────────────────────────────────────────────────────────

  function initWidgets() {
    var widgets = document.querySelectorAll(".margin-interactive[data-steps]");
    widgets.forEach(buildWidget);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initWidgets);
  } else {
    initWidgets();
  }
})();
