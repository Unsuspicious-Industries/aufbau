const svg = document.getElementById('viz');
const synthesizeBtn = document.getElementById('synthesize');
const stepBtn = document.getElementById('stepBtn');
const autoBtn = document.getElementById('autoBtn');
const input = document.getElementById('input');
const specFile = document.getElementById('specFile');
const rankerSelect = document.getElementById('ranker');
const kInput = document.getElementById('k');
const maxStepsInput = document.getElementById('maxSteps');
const stepSlider = document.getElementById('stepSlider');
const stepCounter = document.getElementById('stepCounter');
const currentCodeEl = document.getElementById('currentCode');
const completionsEl = document.getElementById('completions');
const err = document.getElementById('err');
const statusEl = document.getElementById('status');

let synthesisData = null;
let currentStepIndex = 0;
let autoPlayInterval = null;

function setStatus(msg) { statusEl.textContent = msg || ''; }
function clear() { while (svg.firstChild) svg.removeChild(svg.firstChild); }

async function runSynthesis() {
	err.textContent = '';
	try {
		const file = specFile.files && specFile.files[0];
		if (!file) throw new Error('Please choose a spec file');
		const spec = await file.text();
		const ranker = rankerSelect.value;
		const k = parseInt(kInput.value);
		const maxSteps = parseInt(maxStepsInput.value);
		
		setStatus('Running synthesis...');
		const res = await fetch('/synth', {
			method: 'POST',
			headers: { 'Content-Type': 'application/json' },
			body: JSON.stringify({ 
				spec, 
				input: input.value,
				ranker,
				k,
				max_steps: maxSteps
			})
		});
		if (!res.ok) {
			const text = await res.text();
			throw new Error(`Server ${res.status}: ${text}`);
		}
		synthesisData = await res.json();
		currentStepIndex = 0;
		
		// Enable controls
		stepBtn.disabled = false;
		autoBtn.disabled = false;
		stepSlider.disabled = false;
		stepSlider.max = synthesisData.steps.length - 1;
		
		renderStep(currentStepIndex);
		setStatus('Synthesis complete');
	} catch (e) {
		err.textContent = String(e);
		setStatus('');
	}
}

function renderStep(index) {
	if (!synthesisData || index < 0 || index >= synthesisData.steps.length) return;
	
	const step = synthesisData.steps[index];
	currentStepIndex = index;
	stepSlider.value = index;
	stepCounter.textContent = `${index}/${synthesisData.steps.length - 1}`;
	currentCodeEl.value = step.code;
	
	// Render completions
	renderCompletions(step.completions);
	
	// Render AST graph
	if (step.ast) {
		renderRadial(step.ast);
	} else {
		clear();
	}
}

function renderCompletions(completions) {
	if (!completions || completions.length === 0) {
		completionsEl.innerHTML = '<div style="color: var(--muted); font-size: 12px;">No completions</div>';
		return;
	}
	
	let html = '<div style="font-family: monospace; font-size: 12px;">';
	for (const comp of completions) {
		const tokenText = comp.token_type === 'Literal' 
			? `"${comp.token_value}"` 
			: `/${comp.token_value}/`;
		const color = comp.token_type === 'Literal' ? 'var(--green)' : 'var(--accent)';
		html += `<div style="padding: 2px 0; border-bottom: 1px solid var(--border);">`;
		html += `<span style="color: ${color};">${tokenText}</span>`;
		if (comp.metadata.origin) {
			html += ` <span style="color: var(--muted); font-size: 10px;">[${comp.metadata.origin}]</span>`;
		}
		if (comp.metadata.type_hint) {
			html += ` <span style="color: var(--yellow); font-size: 10px;">: ${comp.metadata.type_hint}</span>`;
		}
		html += '</div>';
	}
	html += '</div>';
	completionsEl.innerHTML = html;
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

	// Build adjacency
	const children = new Map();
	for (const e of data.edges) {
		if (!children.has(e.from)) children.set(e.from, []);
		children.get(e.from).push({ id: e.to, style: e.style });
	}

	// Build tree
	const rootId = 'root';
	function buildNode(id, level) {
		const ch = (children.get(id) || []).map(x => buildNode(x.id, level + 1));
		return { id, level, children: ch };
	}
	const tree = buildNode(rootId, 0);

	// Compute sizes
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

	// Position nodes
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
		html += `Complete: ${p.complete}  Partial: ${p.has_partial}\n`;
	}
	if (meta.typing_rule) {
		html += `\nTyping Rule:\n${meta.typing_rule}\n`;
	}
	panel.textContent = html;
}

function nextStep() {
	if (!synthesisData) return;
	if (currentStepIndex < synthesisData.steps.length - 1) {
		renderStep(currentStepIndex + 1);
	}
}

function prevStep() {
	if (!synthesisData) return;
	if (currentStepIndex > 0) {
		renderStep(currentStepIndex - 1);
	}
}

function toggleAutoPlay() {
	if (autoPlayInterval) {
		clearInterval(autoPlayInterval);
		autoPlayInterval = null;
		autoBtn.textContent = 'Auto';
	} else {
		autoBtn.textContent = 'Pause';
		autoPlayInterval = setInterval(() => {
			if (currentStepIndex >= synthesisData.steps.length - 1) {
				clearInterval(autoPlayInterval);
				autoPlayInterval = null;
				autoBtn.textContent = 'Auto';
			} else {
				nextStep();
			}
		}, 500);
	}
}

// Event listeners
synthesizeBtn.addEventListener('click', runSynthesis);
stepBtn.addEventListener('click', nextStep);
autoBtn.addEventListener('click', toggleAutoPlay);
stepSlider.addEventListener('input', (e) => {
	renderStep(parseInt(e.target.value));
});

// Keyboard shortcuts
document.addEventListener('keydown', (e) => {
	if (!synthesisData) return;
	if (e.key === 'ArrowRight') {
		e.preventDefault();
		nextStep();
	} else if (e.key === 'ArrowLeft') {
		e.preventDefault();
		prevStep();
	} else if (e.key === ' ') {
		e.preventDefault();
		toggleAutoPlay();
	}
});
