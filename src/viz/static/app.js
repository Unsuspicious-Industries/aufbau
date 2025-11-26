const svg = document.getElementById('viz');
const btn = document.getElementById('render');
const input = document.getElementById('input');
const specFile = document.getElementById('specFile');
const details = document.getElementById('details');
const statusEl = document.getElementById('parseStatus');
const completionsPanel = document.getElementById('completions');
const treeSelector = document.getElementById('treeSelector');
const showAllBtn = document.getElementById('showAll');
const showValidBtn = document.getElementById('showValid');
const showWellTypedBtn = document.getElementById('showWellTyped');
const showFullyValidBtn = document.getElementById('showFullyValid');
const autoUpdateCheckbox = document.getElementById('autoUpdate');
const contextPanel = document.getElementById('contextPanel');

// State
let currentResponse = null;
let currentSpec = null;
let hiddenTrees = new Set();
let filterMode = 'all'; // 'all' | 'complete' | 'welltyped' | 'fullyvalid'

function setStatus(msg) { if (statusEl) statusEl.textContent = msg || ''; }
function showError(msg) { if (details) details.textContent = msg || 'Error'; }
function clear() { while (svg.firstChild) svg.removeChild(svg.firstChild); }
function escapeHtml(s) {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function updateContextPanel(trees) {
  if (!contextPanel) return;
  
  // Collect all context entries from well-typed trees
  const allEntries = new Map();
  for (const tree of (trees || [])) {
    if (tree.well_typed && tree.context) {
      for (const entry of tree.context) {
        allEntries.set(entry.name, entry.ty);
      }
    }
  }
  
  if (allEntries.size === 0) {
    contextPanel.innerHTML = '<span class="muted">Empty context (Γ = ∅)</span>';
    return;
  }
  
  contextPanel.innerHTML = Array.from(allEntries.entries())
    .map(([name, ty]) => `<span class="context-entry">
      <span class="ctx-name">${escapeHtml(name)}</span>
      <span class="ctx-colon">:</span>
      <span class="ctx-type">${escapeHtml(ty)}</span>
    </span>`)
    .join('');
}

async function fetchGraph(spec, code) {
  const res = await fetch('/graph', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ spec, input: code })
  });
  if (!res.ok) {
    const text = await res.text();
    throw new Error(`Server ${res.status}: ${text}`);
  }
  const response = await res.json();
  if (!response.graph || !response.graph.nodes || !response.graph.edges) {
    throw new Error(`Invalid response structure: ${JSON.stringify(response).slice(0, 200)}`);
  }
  return response;
}

function updateTreeSelector(trees) {
  if (!treeSelector) return;
  
  if (!trees || trees.length === 0) {
    treeSelector.innerHTML = '<span class="muted">No trees</span>';
    return;
  }
  
  // Track context for display
  window.currentTrees = trees;
  
  treeSelector.innerHTML = trees.map(t => {
    // Status: well_typed + complete = valid (green), well_typed + partial = partial (yellow), !well_typed = error (red)
    let statusClass;
    let statusText;
    if (!t.well_typed) {
      statusClass = 'type-error';
      statusText = '✗';
    } else if (t.complete) {
      statusClass = 'complete';
      statusText = '✓';
    } else {
      statusClass = 'partial';
      statusText = '…';
    }
    const hiddenClass = hiddenTrees.has(t.id) ? 'hidden' : '';
    const ctxCount = (t.context || []).length;
    const ctxInfo = ctxCount > 0 ? ` Γ=${ctxCount}` : '';
    return `<span class="tree-chip ${statusClass} ${hiddenClass}" data-tree-id="${t.id}" title="${t.type_status}${ctxInfo}">
      <span class="dot"></span>
      T${t.index} ${statusText}
    </span>`;
  }).join('');
  
  // Add click handlers
  treeSelector.querySelectorAll('.tree-chip').forEach(chip => {
    chip.addEventListener('click', () => {
      const treeId = chip.dataset.treeId;
      if (hiddenTrees.has(treeId)) {
        hiddenTrees.delete(treeId);
        chip.classList.remove('hidden');
      } else {
        hiddenTrees.add(treeId);
        chip.classList.add('hidden');
      }
      if (currentResponse) renderRadial(currentResponse);
    });
  });
}

function updateCompletions(completions, allCompletions) {
  if (!completionsPanel) return;
  
  const typedSet = new Set(completions || []);
  const all = allCompletions || [];
  const typedCount = typedSet.size;
  const totalCount = all.length;
  
  // Update stats
  const statsEl = document.getElementById('completionStats');
  if (statsEl) {
    if (totalCount === 0) {
      statsEl.textContent = '';
    } else if (typedCount === totalCount) {
      statsEl.textContent = `(${typedCount} valid)`;
    } else {
      statsEl.textContent = `(${typedCount}/${totalCount} well-typed)`;
    }
  }
  
  if (all.length === 0) {
    completionsPanel.innerHTML = '<span class="muted">No completions available</span>';
    return;
  }
  
  // Show well-typed completions first, then rejected ones
  const sorted = [...all].sort((a, b) => {
    const aTyped = typedSet.has(a) ? 0 : 1;
    const bTyped = typedSet.has(b) ? 0 : 1;
    return aTyped - bTyped;
  });
  
  completionsPanel.innerHTML = sorted.map(c => {
    const isTyped = typedSet.has(c);
    const className = isTyped ? 'completion-item' : 'completion-item rejected';
    const title = isTyped ? 'Well-typed completion (click to insert)' : 'Rejected: would cause type error';
    return `<span class="${className}" data-completion="${escapeHtml(c)}" title="${title}">${escapeHtml(c)}</span>`;
  }).join('');
  
  // Add click handlers only for well-typed completions
  completionsPanel.querySelectorAll('.completion-item:not(.rejected)').forEach(item => {
    item.addEventListener('click', () => {
      const completion = item.dataset.completion;
      input.value += completion;
      input.focus();
      // Trigger re-parse if auto-update is on
      if (autoUpdateCheckbox && autoUpdateCheckbox.checked) {
        triggerParse();
      }
    });
  });
}

function applyTreeFilter(data, trees) {
  // Get visible tree IDs
  let visibleTreeIds = new Set();
  
  for (const tree of trees) {
    // Apply filter mode
    if (filterMode === 'complete' && !tree.complete) continue;
    if (filterMode === 'welltyped' && !tree.well_typed) continue;
    if (filterMode === 'fullyvalid' && !(tree.complete && tree.well_typed)) continue;
    // Apply individual tree visibility
    if (hiddenTrees.has(tree.id)) continue;
    visibleTreeIds.add(tree.id);
  }
  
  // Filter nodes and edges
  const visibleNodeIds = new Set(['root']);
  
  // First pass: collect all nodes belonging to visible trees
  for (const node of data.nodes) {
    const treeId = getTreeIdForNode(node.id);
    if (treeId && visibleTreeIds.has(treeId)) {
      visibleNodeIds.add(node.id);
    }
  }
  
  // Always include root
  visibleNodeIds.add('root');
  
  // Filter
  const filteredNodes = data.nodes.filter(n => visibleNodeIds.has(n.id));
  const filteredEdges = data.edges.filter(e => 
    visibleNodeIds.has(e.from) && visibleNodeIds.has(e.to)
  );
  
  return { nodes: filteredNodes, edges: filteredEdges };
}

function getTreeIdForNode(nodeId) {
  if (nodeId === 'root') return null;
  // Node IDs are like "t0", "t0_1", "t0_1_2", etc.
  const match = nodeId.match(/^(t\d+)/);
  return match ? match[1] : null;
}

function renderRadial(response) {
  clear();
  currentResponse = response;
  
  const rawData = response.graph;
  if (!rawData || !Array.isArray(rawData.nodes) || !Array.isArray(rawData.edges)) {
    showError('Invalid response: ' + JSON.stringify(response));
    return;
  }
  
  // Update tree selector
  updateTreeSelector(rawData.trees || []);
  
  // Update context panel
  updateContextPanel(rawData.trees || []);
  
  // Update completions (both typed and all syntactic)
  updateCompletions(response.completions, response.all_completions);
  
  // Apply tree filtering
  const data = applyTreeFilter(rawData, rawData.trees || []);
  
  if (data.nodes.length <= 1) {
    // Only root node, show message
    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', '50%');
    text.setAttribute('y', '50%');
    text.setAttribute('text-anchor', 'middle');
    text.setAttribute('fill', 'var(--muted)');
    text.textContent = 'No trees visible (adjust filter)';
    svg.appendChild(text);
    return;
  }
  
  // Update status with type info
  const validCount = (rawData.trees || []).filter(t => t.well_typed && t.complete).length;
  const errorCount = (rawData.trees || []).filter(t => !t.well_typed).length;
  setStatus(`${rawData.trees?.length || 0} trees · ${validCount} valid · ${errorCount} type errors`);
  
  const width = svg.clientWidth || 960;
  const height = svg.clientHeight || 700;
  const cx = width / 2; const cy = height / 2;
  const radiusStep = Math.min(width, height) / 10;

  // Pan & zoom setup
  let scale = 1;
  let panX = 0, panY = 0;
  const root = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  const content = document.createElementNS('http://www.w3.org/2000/svg', 'g');
  root.appendChild(content);
  svg.appendChild(root);
  function applyTransform() {
    root.setAttribute('transform', `translate(${panX},${panY}) scale(${scale})`);
  }
  applyTransform();

  let isPanning = false; let startX = 0; let startY = 0; let startPanX = 0; let startPanY = 0;
  svg.addEventListener('mousedown', (e) => {
    if (e.button !== 0) return;
    isPanning = true; startX = e.clientX; startY = e.clientY; startPanX = panX; startPanY = panY;
  });
  window.addEventListener('mousemove', (e) => {
    if (!isPanning) return;
    panX = startPanX + (e.clientX - startX);
    panY = startPanY + (e.clientY - startY);
    applyTransform();
  });
  window.addEventListener('mouseup', () => { isPanning = false; });
  svg.addEventListener('wheel', (e) => {
    e.preventDefault();
    const delta = -Math.sign(e.deltaY) * 0.1;
    const newScale = Math.min(4, Math.max(0.25, scale * (1 + delta)));
    const rect = svg.getBoundingClientRect();
    const cxp = e.clientX - rect.left; const cyp = e.clientY - rect.top;
    const k = newScale / scale;
    panX = cxp - k * (cxp - panX);
    panY = cyp - k * (cyp - panY);
    scale = newScale;
    applyTransform();
  }, { passive: false });

  // Build adjacency preserving input order
  const children = new Map();
  for (const e of data.edges) {
    if (!children.has(e.from)) children.set(e.from, []);
    children.get(e.from).push({ id: e.to, style: e.style });
  }

  // Build a rooted tree from 'root'
  const rootId = 'root';
  function buildNode(id, level) {
    const ch = (children.get(id) || []).map(x => buildNode(x.id, level + 1));
    return { id, level, children: ch };
  }
  const tree = buildNode(rootId, 0);

  // Compute subtree sizes
  function computeSize(node) {
    if (!node.children.length) { node.size = 1; return 1; }
    let s = 0; for (const c of node.children) s += computeSize(c); node.size = Math.max(1, s); return node.size;
  }
  computeSize(tree);

  // Assign angles
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

  // Collect positions
  const pos = new Map();
  function place(node) {
    const r = (node.level + 1) * radiusStep;
    const a = node.angle;
    pos.set(node.id, { x: cx + r * Math.cos(a), y: cy + r * Math.sin(a) });
    for (const c of node.children) place(c);
  }
  place(tree);

  // Edges first
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

  // Nodes
  const byId = new Map(data.nodes.map(n => [n.id, n]));
  for (const [id, p] of pos) {
    const n = byId.get(id);
    if (!n) continue;
    const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
    g.setAttribute('class', 'node');
    const c = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
    c.setAttribute('cx', p.x); c.setAttribute('cy', p.y); c.setAttribute('r', 12);
    let color = 'var(--gray)';
    if (n.status === 'complete') color = 'var(--green)';
    else if (n.status === 'warning') color = 'var(--yellow)';
    else if (n.status === 'partial') color = 'var(--red)';
    else if (n.status === 'error') color = 'var(--red)';
    c.setAttribute('fill', color);
    g.appendChild(c);
    const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    text.setAttribute('x', p.x); text.setAttribute('y', p.y - 16);
    text.setAttribute('class', 'label');
    text.textContent = n.label;
    
    // Add typing rule indicator if present
    if (n.meta && n.meta.typing_rule) {
      const ruleIndicator = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
      ruleIndicator.setAttribute('cx', p.x + 8);
      ruleIndicator.setAttribute('cy', p.y - 8);
      ruleIndicator.setAttribute('r', 3);
      ruleIndicator.setAttribute('fill', 'var(--accent)');
      ruleIndicator.setAttribute('class', 'rule-indicator');
      g.appendChild(ruleIndicator);
    }
    
    g.addEventListener('click', (e) => {
      e.stopPropagation();
      showNodeDetails(n);
    });
    content.appendChild(g);
    content.appendChild(text);
  }
}

function showNodeDetails(n) {
  if (!details) return;
  const meta = n.meta || {};
  let html = '';
  html += `Node: ${n.label}\n`;
  html += `Status: ${n.status}\n`;
  if (meta.kind) html += `Kind: ${meta.kind}\n`;
  if (meta.value) html += `Value: ${meta.value}\n`;
  if (meta.binding) html += `Binding: ${meta.binding}\n`;
  if (meta.production) {
    const p = meta.production;
    html += `Production: ${p.rhs.join(' ')}\n`;
    html += `Cursor: ${p.cursor}/${p.rhs.length}\n`;
    html += `Complete: ${p.complete}\n`;
  }
  if (meta.typing_rule) {
    const r = meta.typing_rule;
    html += `\n--- Typing Rule: ${r.name} ---\n`;
    if (r.premises.length > 0) {
      html += `Premises:\n`;
      r.premises.forEach(p => html += `  ${p}\n`);
    }
    html += `Conclusion: ${r.conclusion}\n`;
  }
  details.textContent = html;
}

async function triggerParse() {
  if (details) details.textContent = 'Click a node to inspect';
  try {
    const file = specFile.files && specFile.files[0];
    if (!file) throw new Error('Please choose a spec file');
    const spec = await file.text();
    currentSpec = spec;
    setStatus('Parsing…');
    const data = await fetchGraph(spec, input.value);
    setStatus('Rendering…');
    renderRadial(data);
    // Status is updated by renderRadial with type info
  } catch (e) {
    showError(String(e));
    setStatus('Error');
  }
}

// Event listeners
btn.addEventListener('click', triggerParse);

// Filter buttons
function setFilterMode(mode) {
  filterMode = mode;
  showAllBtn?.classList.toggle('active', mode === 'all');
  showValidBtn?.classList.toggle('active', mode === 'complete');
  showWellTypedBtn?.classList.toggle('active', mode === 'welltyped');
  showFullyValidBtn?.classList.toggle('active', mode === 'fullyvalid');
  if (currentResponse) renderRadial(currentResponse);
}

showAllBtn?.addEventListener('click', () => setFilterMode('all'));
showValidBtn?.addEventListener('click', () => setFilterMode('complete'));
showWellTypedBtn?.addEventListener('click', () => setFilterMode('welltyped'));
showFullyValidBtn?.addEventListener('click', () => setFilterMode('fullyvalid'));

// Auto-update on input change
let debounceTimer = null;
input.addEventListener('input', () => {
  if (!autoUpdateCheckbox || !autoUpdateCheckbox.checked) return;
  if (!specFile.files || !specFile.files[0]) return;
  
  clearTimeout(debounceTimer);
  debounceTimer = setTimeout(triggerParse, 300);
});
