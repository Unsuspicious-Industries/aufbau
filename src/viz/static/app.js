const svg = document.getElementById('viz');
const btn = document.getElementById('render');
const input = document.getElementById('input');
const specFile = document.getElementById('specFile');
const err = document.getElementById('err');
const statusEl = document.getElementById('status');

function setStatus(msg) { statusEl.textContent = msg || ''; }
function clear() { while (svg.firstChild) svg.removeChild(svg.firstChild); }

async function fetchGraph(spec, code) {
  const res = await fetch('/graph', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ spec, input: code })
  });
  if (!res.ok) throw new Error(`Server ${res.status}`);
  return await res.json();
}

function renderRadial(data) {
  clear();
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
    const delta = -Math.sign(e.deltaY) * 0.1; // zoom step
    const newScale = Math.min(4, Math.max(0.25, scale * (1 + delta)));
    // Zoom towards cursor
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

  // Build a rooted tree from 'root'; ignore nodes not connected to root
  const rootId = 'root';
  const nodesSet = new Set(data.nodes.map(n => n.id));
  function buildNode(id, level) {
    const ch = (children.get(id) || []).map(x => buildNode(x.id, level + 1));
    return { id, level, children: ch };
  }
  const tree = buildNode(rootId, 0);

  // Compute subtree sizes (number of leaves) to allocate angular spans
  function computeSize(node) {
    if (!node.children.length) { node.size = 1; return 1; }
    let s = 0; for (const c of node.children) s += computeSize(c); node.size = Math.max(1, s); return node.size;
  }
  computeSize(tree);

  // Assign angles within [start, end] recursively without overlap
  const TWO_PI = Math.PI * 2;
  const START_ANGLE = -Math.PI / 2; // start at top
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

  // Collect positions by level and assigned angle
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
  const info = document.getElementById('status');
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
    g.addEventListener('click', (e) => {
      e.stopPropagation();
      showNodeDetails(n);
    });
    content.appendChild(g);
    content.appendChild(text);
  }
}

function showNodeDetails(n) {
  const panel = document.getElementById('err');
  const meta = n.meta || {};
  let html = '';
  html += `Node: ${n.label}\n`;
  html += `Status: ${n.status}\n`;
  if (meta.kind) html += `Kind: ${meta.kind}\n`;
  if (meta.value) html += `Value: ${meta.value}\n`;
  if (meta.binding) html += `Binding: ${meta.binding}\n`;
  if (meta.span) html += `Span: [${meta.span.start}, ${meta.span.end}]\n`;
  if (meta.production) {
    const p = meta.production;
    html += `Production: ${p.rhs.join(' ')}\n`;
    html += `Cursor: ${p.cursor}/${p.rhs_len}\n`;
    html += `Complete: ${p.complete}  Partial-In-Progress: ${p.has_partial}\n`;
  }
  panel.textContent = html;
}

btn.addEventListener('click', async () => {
  err.textContent = '';
  try {
    const file = specFile.files && specFile.files[0];
    if (!file) throw new Error('Please choose a spec file');
    const spec = await file.text();
    setStatus('Parsing…');
    const data = await fetchGraph(spec, input.value);
    setStatus('Rendering…');
    renderRadial(data);
    setStatus('Done');
  } catch (e) {
    err.textContent = String(e);
    setStatus('');
  }
});


