// P7 Parser Debugger
// All trees in the response have matched the full input.
// Filter distinguishes between:
// - Complete AST: syntactically complete (is_complete = true)
// - Partial AST: can accept more input (is_complete = false)

const $ = (sel) => document.querySelector(sel);
const $$ = (sel) => Array.from(document.querySelectorAll(sel));

// Elements
const svg = $('#viz');
const btn = $('#render');
const input = $('#input');
const specLibrarySel = $('#specLibrary');
const statusEl = $('#parseStatus');
const completionsPanel = $('#completions');
const treeSelector = $('#treeSelector');
const reconstructedPanel = $('#reconstructedPanel');
const showAllBtn = $('#showAll');
const showValidBtn = $('#showValid');
const showCompleteBtn = $('#showComplete');
const showTypedBtn = $('#showTyped');
const autoUpdateCheckbox = $('#autoUpdate');
const debugLevelSel = $('#debugLevel');
const debugModulesInput = $('#debugModules');
const typingContextInput = $('#typingContext');
const typedAstEl = $('#typedAst');
const tokensPre = $('#tokensPanel');
const rawJsonPre = $('#rawJson');
const timingsEl = $('#timings');
const inspectorContent = $('#inspectorContent');
const inspectorNodeId = $('#inspectorNodeId');
const treeStats = $('#treeStats');
const completionStats = $('#completionStats');

// State
let currentResponse = null;
let hiddenTrees = new Set();
let filterMode = 'all'; // 'all' | 'valid' | 'complete' | 'typed'
let debounceTimer = null;
let selectedNodeId = null;
let selectedSpecPath = null;

function setStatus(msg) { if (statusEl) statusEl.textContent = msg || ''; }
function escapeHtml(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}
function clearSvg() { while (svg?.firstChild) svg.removeChild(svg.firstChild); }

// ---------------------------------------------------------------------------
// Tabs
// ---------------------------------------------------------------------------
function setupTabs() {
  $$('.tab').forEach(t => {
    t.addEventListener('click', () => {
      const id = t.dataset.tab;
      $$('.tab').forEach(x => x.classList.toggle('active', x === t));
      $$('.pane').forEach(p => p.classList.toggle('active', p.dataset.pane === id));
    });
  });
}

// ---------------------------------------------------------------------------
// Spec loading
// ---------------------------------------------------------------------------
async function loadSpecLibrary() {
  if (!specLibrarySel) return;
  specLibrarySel.innerHTML = '<option value="">Loading…</option>';
  try {
    const res = await fetch('/specs');
    const data = await res.json();
    if (!res.ok || !data?.ok) throw new Error(data?.error || `HTTP ${res.status}`);
    
    const specs = data.aufs || [];
    if (specs.length === 0) {
      specLibrarySel.innerHTML = '<option value="">(no specs found)</option>';
      return;
    }
    
    specLibrarySel.innerHTML = specs.map(s => 
      `<option value="${escapeHtml(s.path)}">${escapeHtml(s.name)}</option>`
    ).join('');
    
    // Auto-select first or clike if available
    const prefer = specs.find(s => /clike\.auf$/i.test(s.path)) || specs[0];
    selectedSpecPath = prefer.path;
    specLibrarySel.value = selectedSpecPath;
  } catch (e) {
    specLibrarySel.innerHTML = '<option value="">(failed to load)</option>';
    setStatus('Error loading specs');
  }
}

async function loadSpecText() {
  if (!selectedSpecPath) throw new Error('No spec selected');
  const file = selectedSpecPath.replace(/^examples\//, '');
  const res = await fetch('/examples/' + file);
  if (!res.ok) throw new Error(`Failed to load spec: ${res.status}`);
  return await res.text();
}

// ---------------------------------------------------------------------------
// API
// ---------------------------------------------------------------------------
function parseTypingContext(text) {
  return String(text || '').split(/\r?\n/)
    .map(line => line.trim())
    .filter(s => s && s.includes(':'))
    .map(s => {
      const m = s.match(/^([^:]+):(.+)$/);
      return m ? { name: m[1].trim(), ty: m[2].trim() } : null;
    })
    .filter(Boolean);
}

async function analyze(spec, code) {
  const debug_modules = (debugModulesInput?.value || '')
    .split(',').map(s => s.trim()).filter(Boolean);

  const res = await fetch('/analyze', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      spec,
      input: code,
      debug_level: debugLevelSel?.value || 'none',
      debug_modules,
      context: parseTypingContext(typingContextInput?.value || ''),
    })
  });

  const data = await res.json();
  if (!res.ok) throw new Error(data?.error || `HTTP ${res.status}`);
  if (data?.version !== 'v1') throw new Error('Unexpected response version');
  return data;
}

// ---------------------------------------------------------------------------
// Reconstructed Input Panel
// ---------------------------------------------------------------------------
function renderReconstructedPanel(resp) {
  if (!reconstructedPanel) return;
  
  const recons = resp.ast_graph?.reconstructed_inputs || [];
  if (recons.length === 0) {
    reconstructedPanel.innerHTML = '<span class="muted">No trees</span>';
    return;
  }
  
  reconstructedPanel.innerHTML = recons.map(r => {
    const badge = r.complete ? 'complete' : 'partial';
    const label = r.complete ? '✓' : '…';
    return `<div class="recon-item">
      <span class="recon-badge ${badge}">T${r.tree_index} ${label}</span>
      <span class="recon-text">${escapeHtml(r.text)}</span>
    </div>`;
  }).join('');
}

// ---------------------------------------------------------------------------
// Tree Selector
// ---------------------------------------------------------------------------
let clickCounts = new Map(); // Track clicks per tree for triple-click detection
let clickTimers = new Map();

function renderTreeSelector(trees) {
  if (!treeSelector) return;
  if (!trees?.length) {
    treeSelector.innerHTML = '<span class="muted">No trees</span>';
    if (treeStats) treeStats.textContent = '';
    return;
  }
  
  const complete = trees.filter(t => t.complete).length;
  const wellTyped = trees.filter(t => t.well_typed).length;
  const valid = trees.filter(t => t.complete && t.well_typed).length;
  if (treeStats) {
    treeStats.textContent = `(${valid} valid, ${wellTyped} typed, ${complete} complete)`;
  }
  
  treeSelector.innerHTML = trees.map(t => {
    // Determine primary status class
    let cls = '';
    let mark = '';
    if (t.complete && t.well_typed) {
      cls = 'valid';
      mark = '✓';
    } else if (t.well_typed && !t.complete) {
      cls = 'partial-typed';
      mark = '…';
    } else if (t.complete && !t.well_typed) {
      cls = 'complete-untyped';
      mark = '⚠';
    } else {
      cls = 'error';
      mark = '✗';
    }
    
    const hidden = hiddenTrees.has(t.id) ? 'hidden' : '';
    const typeInfo = `${t.type_status} | ${t.complete ? 'complete' : 'partial'} AST`;
    
    return `<span class="tree-chip ${cls} ${hidden}" data-tree-id="${t.id}" title="${typeInfo}">
      <span class="status-dot"></span>
      T${t.index} ${mark}
    </span>`;
  }).join('');
  
  treeSelector.querySelectorAll('.tree-chip').forEach(chip => {
    chip.addEventListener('click', () => {
      const id = chip.dataset.treeId;
      
      // Track clicks for triple-click detection
      const now = Date.now();
      const count = (clickCounts.get(id) || 0) + 1;
      clickCounts.set(id, count);
      
      // Reset click count after 500ms
      clearTimeout(clickTimers.get(id));
      clickTimers.set(id, setTimeout(() => clickCounts.set(id, 0), 500));
      
      if (count >= 3) {
        // Triple click: select ONLY this tree, hide all others
        clickCounts.set(id, 0);
        hiddenTrees.clear();
        trees.forEach(t => {
          if (t.id !== id) hiddenTrees.add(t.id);
        });
        // Update all chips visually
        treeSelector.querySelectorAll('.tree-chip').forEach(c => {
          c.classList.toggle('hidden', c.dataset.treeId !== id);
        });
      } else {
        // Normal click: toggle visibility
        if (hiddenTrees.has(id)) {
          hiddenTrees.delete(id);
          chip.classList.remove('hidden');
        } else {
          hiddenTrees.add(id);
          chip.classList.add('hidden');
        }
      }
      
      if (currentResponse) renderForest(currentResponse);
    });
  });
}

// ---------------------------------------------------------------------------
// Completions
// ---------------------------------------------------------------------------
function renderCompletions(completions, allCompletions) {
  if (!completionsPanel) return;
  
  const typedSet = new Set(completions || []);
  const all = allCompletions || [];
  
  if (completionStats) {
    completionStats.textContent = all.length ? `(${typedSet.size}/${all.length} typed)` : '';
  }
  
  if (!all.length) {
    completionsPanel.innerHTML = '<span class="muted">None</span>';
    return;
  }
  
  // Show well-typed first
  const sorted = [...all].sort((a, b) => (typedSet.has(a) ? 0 : 1) - (typedSet.has(b) ? 0 : 1));
  
  completionsPanel.innerHTML = sorted.map(c => {
    const isTyped = typedSet.has(c);
    const cls = isTyped ? 'completion-item' : 'completion-item rejected';
    return `<span class="${cls}" data-completion="${escapeHtml(c)}">${escapeHtml(c)}</span>`;
  }).join('');
  
  completionsPanel.querySelectorAll('.completion-item:not(.rejected)').forEach(item => {
    item.addEventListener('click', () => {
      input.value += item.dataset.completion;
      input.focus();
      if (autoUpdateCheckbox?.checked) triggerAnalyze();
    });
  });
}

// ---------------------------------------------------------------------------
// Timings
// ---------------------------------------------------------------------------
function renderTimings(resp) {
  if (!timingsEl) return;
  const t = resp.timings_ms;
  if (!t) { timingsEl.textContent = ''; return; }
  timingsEl.textContent = `${t.total}ms total · parse ${t.parse_partial}ms`;
}

// ---------------------------------------------------------------------------
// Tokens
// ---------------------------------------------------------------------------
function renderTokens(resp) {
  if (!tokensPre) return;
  const toks = resp.tokens || [];
  if (!toks.length) {
    tokensPre.textContent = 'No tokens';
    return;
  }
  tokensPre.textContent = toks.map(t => 
    `[${t.index}] ${JSON.stringify(t.text)} (${t.start}..${t.end})${t.is_partial_special ? ' partial' : ''}`
  ).join('\n');
}

// ---------------------------------------------------------------------------
// Typed AST
// ---------------------------------------------------------------------------
function renderTypedAst(resp) {
  if (!typedAstEl) return;
  const ta = resp.typed_ast;
  if (!ta) {
    typedAstEl.textContent = 'Unavailable';
    return;
  }
  
  const parts = [];
  
  // Per-tree status
  if (ta.trees?.length) {
    parts.push('<span class="ta-line"><span class="ta-kind">Trees</span></span>');
    for (const tr of ta.trees) {
      const mark = tr.type_status === 'valid' ? '✓' : (tr.type_status === 'partial' ? '…' : '✗');
      const cls = tr.type_status === 'valid' ? 'ta-valid' : (tr.type_status === 'partial' ? 'ta-partial' : 'ta-error');
      parts.push(`<span class="ta-line">  <span class="${cls}">${mark}</span> T${tr.index} ${tr.complete ? 'complete' : 'partial'} : <span class="ta-ty">${escapeHtml(tr.ty)}</span></span>`);
    }
    parts.push('<span class="ta-line"></span>');
  }
  
  function fmtNode(n, depth) {
    const indent = '  '.repeat(depth);
    if (n.kind === 'Term') {
      parts.push(`<span class="ta-line">${indent}<span class="ta-term">${escapeHtml(JSON.stringify(n.val))}</span> : <span class="ta-ty">${escapeHtml(n.ty)}</span></span>`);
    } else if (n.kind === 'Expr') {
      const mark = n.complete ? '' : ' (partial)';
      parts.push(`<span class="ta-line">${indent}<span class="ta-name">${escapeHtml(n.name)}</span>${mark} : <span class="ta-ty">${escapeHtml(n.ty)}</span></span>`);
      for (const ch of (n.children || [])) fmtNode(ch, depth + 1);
    }
  }
  
  parts.push('<span class="ta-line"><span class="ta-kind">AST</span></span>');
  for (const r of (ta.roots || [])) fmtNode(r, 1);
  
  typedAstEl.innerHTML = parts.join('');
}

// ---------------------------------------------------------------------------
// Node Inspector
// ---------------------------------------------------------------------------
function showNodeDetails(node) {
  if (!node || !inspectorContent) return;
  
  selectedNodeId = node.id;
  if (inspectorNodeId) inspectorNodeId.textContent = node.id;
  
  const sections = [];
  
  // Basic info with status indicator
  const statusClass = node.status === 'complete' || node.status === 'terminal' ? 'status-ok' 
    : node.status === 'partial' ? 'status-partial' 
    : 'status-error';
  
  sections.push(`<div class="inspector-section">
    <span class="inspector-key">Node:</span> <span class="inspector-value">${escapeHtml(node.label)}</span>
    <span class="inspector-status ${statusClass}">${escapeHtml(node.status)}</span>
  </div>`);
  
  // RECONSTRUCTED INPUT - prominent display
  if (node.reconstructed) {
    sections.push(`<div class="inspector-section">
      <span class="inspector-key">Reconstructed:</span>
      <div class="inspector-recon">${escapeHtml(node.reconstructed)}</div>
    </div>`);
  }
  
  // TYPE INFO - make this prominent for type debugging
  if (node.meta?.inferred_type) {
    sections.push(`<div class="inspector-section inspector-type-section">
      <span class="inspector-key">Inferred Type:</span>
      <div class="inspector-type">${escapeHtml(node.meta.inferred_type)}</div>
    </div>`);
  }
  
  // Typing rule - important for debugging
  if (node.meta?.typing_rule) {
    const tr = node.meta.typing_rule;
    sections.push(`<div class="inspector-section inspector-rule-section">
      <span class="inspector-key">Typing Rule:</span> <span class="inspector-rule-name">${escapeHtml(tr.name)}</span>
      ${tr.pretty ? `<div class="inspector-rule">${escapeHtml(tr.pretty)}</div>` : ''}
    </div>`);
  }
  
  // Context extensions (Γ changes)
  if (node.meta?.context?.length) {
    sections.push(`<div class="inspector-section">
      <span class="inspector-key">Γ extensions:</span>
      <div class="inspector-context">
        ${node.meta.context.map(e => 
          `<span class="ctx-entry"><span class="ctx-name">${escapeHtml(e.name)}</span> : <span class="ctx-type">${escapeHtml(e.ty)}</span></span>`
        ).join('')}
      </div>
    </div>`);
  }
  
  // Binding
  if (node.meta?.binding) {
    sections.push(`<div class="inspector-section">
      <span class="inspector-key">Binding:</span> <span class="inspector-binding">${escapeHtml(node.meta.binding)}</span>
    </div>`);
  }
  
  // Production info (less prominent)
  if (node.meta?.production) {
    const p = node.meta.production;
    const rhs = p.rhs?.join(' ') || '';
    const progress = `${p.cursor}/${p.rhs?.length || 0}`;
    sections.push(`<div class="inspector-section inspector-production">
      <span class="inspector-key">Production:</span> <span class="inspector-value">${escapeHtml(rhs)}</span>
      <span class="inspector-progress">[${progress}]</span>
    </div>`);
  }
  
  inspectorContent.innerHTML = sections.join('') || '<span class="muted">No details</span>';
}

// ---------------------------------------------------------------------------
// Forest Rendering
// ---------------------------------------------------------------------------
function getTreeIdForNode(nodeId) {
  if (nodeId === 'root') return null;
  const m = nodeId.match(/^(t\d+)/);
  return m ? m[1] : null;
}

function applyTreeFilter(data, trees) {
  const visibleTreeIds = new Set();
  
  for (const tree of trees) {
    // Apply filter mode
    if (filterMode === 'valid' && !(tree.complete && tree.well_typed)) continue;
    if (filterMode === 'complete' && !tree.complete) continue;
    if (filterMode === 'typed' && !tree.well_typed) continue;
    // Apply individual visibility
    if (hiddenTrees.has(tree.id)) continue;
    visibleTreeIds.add(tree.id);
  }
  
  const visibleNodeIds = new Set(['root']);
  for (const node of data.nodes) {
    const treeId = getTreeIdForNode(node.id);
    if (treeId && visibleTreeIds.has(treeId)) {
      visibleNodeIds.add(node.id);
    }
  }
  
  return {
    nodes: data.nodes.filter(n => visibleNodeIds.has(n.id)),
    edges: data.edges.filter(e => visibleNodeIds.has(e.from) && visibleNodeIds.has(e.to))
  };
}

function renderForest(resp) {
  clearSvg();
  
  const rawData = resp.ast_graph;
  if (!rawData?.nodes?.length) {
    setStatus('No parse trees');
    return;
  }
  
  renderTreeSelector(rawData.trees || []);
  renderCompletions(resp.completions, resp.all_completions);
  
  const data = applyTreeFilter(rawData, rawData.trees || []);
  
  if (data.nodes.length <= 1) {
    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', '50%');
    text.setAttribute('y', '50%');
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', 'var(--muted)');
    text.textContent = 'No trees visible (adjust filter)';
    svg.appendChild(text);
    return;
  }
  
  const trees = rawData.trees || [];
  const complete = trees.filter(t => t.complete && t.well_typed).length;
  setStatus(`${trees.length} trees · ${complete} valid`);
  
  const width = svg.clientWidth || 800;
  const height = svg.clientHeight || 600;
  const cx = width / 2;
  const cy = height / 2;
  const radiusStep = Math.min(width, height) / 10;
  
  // Pan & zoom
  let scale = 1, panX = 0, panY = 0;
  const root = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const content = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  root.appendChild(content);
  svg.appendChild(root);
  
  function applyTransform() {
    root.setAttribute('transform', `translate(${panX},${panY}) scale(${scale})`);
  }
  applyTransform();
  
  let isPanning = false, startX = 0, startY = 0, startPanX = 0, startPanY = 0;
  svg.addEventListener('mousedown', e => {
    if (e.button !== 0) return;
    isPanning = true;
    startX = e.clientX; startY = e.clientY;
    startPanX = panX; startPanY = panY;
  });
  window.addEventListener('mousemove', e => {
    if (!isPanning) return;
    panX = startPanX + (e.clientX - startX);
    panY = startPanY + (e.clientY - startY);
    applyTransform();
  });
  window.addEventListener('mouseup', () => isPanning = false);
  svg.addEventListener('wheel', e => {
    e.preventDefault();
    const delta = -Math.sign(e.deltaY) * 0.1;
    const newScale = Math.min(4, Math.max(0.2, scale * (1 + delta)));
    const rect = svg.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;
    const k = newScale / scale;
    panX = mx - k * (mx - panX);
    panY = my - k * (my - panY);
    scale = newScale;
    applyTransform();
  }, { passive: false });
  
  // Build tree structure
  const children = new Map();
  for (const e of data.edges) {
    if (!children.has(e.from)) children.set(e.from, []);
    children.get(e.from).push({ id: e.to, style: e.style });
  }
  
  function buildNode(id, level) {
    const ch = (children.get(id) || []).map(x => buildNode(x.id, level + 1));
    return { id, level, children: ch };
  }
  const tree = buildNode('root', 0);
  
  function computeSize(node) {
    if (!node.children.length) { node.size = 1; return 1; }
    let s = 0;
    for (const c of node.children) s += computeSize(c);
    node.size = Math.max(1, s);
    return node.size;
  }
  computeSize(tree);
  
  const TWO_PI = Math.PI * 2;
  const START_ANGLE = -Math.PI / 2;
  
  function assignAngles(node, start, end) {
    node.angle = (start + end) / 2;
    if (!node.children.length) return;
    const span = end - start;
    let cursor = start;
    for (const c of node.children) {
      const frac = c.size / node.size;
      const childSpan = frac * span;
      assignAngles(c, cursor, cursor + childSpan);
      cursor += childSpan;
    }
  }
  assignAngles(tree, START_ANGLE, START_ANGLE + TWO_PI);
  
  const pos = new Map();
  function place(node) {
    const r = (node.level + 1) * radiusStep;
    const a = node.angle;
    pos.set(node.id, { x: cx + r * Math.cos(a), y: cy + r * Math.sin(a) });
    for (const c of node.children) place(c);
  }
  place(tree);
  
  // Draw edges
  for (const e of data.edges) {
    const a = pos.get(e.from) || { x: cx, y: cy };
    const b = pos.get(e.to) || { x: cx, y: cy };
    const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    line.setAttribute('x1', a.x);
    line.setAttribute('y1', a.y);
    line.setAttribute('x2', b.x);
    line.setAttribute('y2', b.y);
    line.setAttribute('class', 'edge' + (e.style === 'dashed' ? ' dashed' : ''));
    content.appendChild(line);
  }
  
  // Draw nodes
  const byId = new Map(data.nodes.map(n => [n.id, n]));
  for (const [id, p] of pos) {
    const n = byId.get(id);
    if (!n) continue;
    
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    g.setAttribute('class', 'node' + (id === selectedNodeId ? ' selected' : ''));
    
    const c = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    c.setAttribute('cx', p.x);
    c.setAttribute('cy', p.y);
    c.setAttribute('r', 10);
    
    let color = 'var(--muted)';
    if (n.status === 'complete' || n.status === 'terminal') color = 'var(--green)';
    else if (n.status === 'partial') color = 'var(--yellow)';
    else if (n.status === 'error') color = 'var(--red)';
    c.setAttribute('fill', color);
    
    g.appendChild(c);
    
    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', p.x);
    text.setAttribute('y', p.y - 14);
    text.setAttribute('class', 'node-label');
    text.textContent = n.label.length > 20 ? n.label.slice(0, 18) + '…' : n.label;
    
    g.addEventListener('click', e => {
      e.stopPropagation();
      // Deselect previous
      svg.querySelectorAll('.node.selected').forEach(el => el.classList.remove('selected'));
      g.classList.add('selected');
      showNodeDetails(n);
    });
    
    content.appendChild(g);
    content.appendChild(text);
  }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------
async function triggerAnalyze() {
  if (!selectedSpecPath) {
    setStatus('Select a grammar');
    return;
  }
  
  try {
    setStatus('Parsing…');
    const spec = await loadSpecText();
    const resp = await analyze(spec, input?.value || '');
    currentResponse = resp;
    
    renderReconstructedPanel(resp);
    renderTimings(resp);
    renderTokens(resp);
    renderTypedAst(resp);
    if (rawJsonPre) rawJsonPre.textContent = JSON.stringify(resp, null, 2);
    
    renderForest(resp);
    setStatus(resp.ok ? 'Ready' : 'Error');
  } catch (e) {
    setStatus('Error: ' + e.message);
    console.error(e);
  }
}

function setFilterMode(mode) {
  filterMode = mode;
  showAllBtn?.classList.toggle('active', mode === 'all');
  showValidBtn?.classList.toggle('active', mode === 'valid');
  showCompleteBtn?.classList.toggle('active', mode === 'complete');
  showTypedBtn?.classList.toggle('active', mode === 'typed');
  if (currentResponse) renderForest(currentResponse);
}

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------
setupTabs();

btn?.addEventListener('click', triggerAnalyze);

showAllBtn?.addEventListener('click', () => setFilterMode('all'));
showValidBtn?.addEventListener('click', () => setFilterMode('valid'));
showCompleteBtn?.addEventListener('click', () => setFilterMode('complete'));
showTypedBtn?.addEventListener('click', () => setFilterMode('typed'));

input?.addEventListener('input', () => {
  if (!autoUpdateCheckbox?.checked) return;
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(triggerAnalyze, 200);
});

specLibrarySel?.addEventListener('change', () => {
  selectedSpecPath = specLibrarySel.value || null;
  if (autoUpdateCheckbox?.checked) triggerAnalyze();
});

debugLevelSel?.addEventListener('change', () => {
  if (autoUpdateCheckbox?.checked) triggerAnalyze();
});

debugModulesInput?.addEventListener('input', () => {
  if (autoUpdateCheckbox?.checked) {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(triggerAnalyze, 300);
  }
});

typingContextInput?.addEventListener('input', () => {
  if (autoUpdateCheckbox?.checked) {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(triggerAnalyze, 300);
  }
});

// Load specs on init
loadSpecLibrary();
