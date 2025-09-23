async function postParse(code, spec) {
  const res = await fetch('/api/parse', {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ code, spec })
  });
  return await res.json();
}

function statusBadge(node) {
  if (node.kind === 'terminal') return '<span class="badge complete">terminal</span>';
  if (node.is_complete === true) return '<span class="badge complete">complete</span>';
  if (node.is_dead_end) return '<span class="badge dead">dead end</span>';
  return '<span class="badge progress">partial</span>';
}

function renderNode(node) {
  const kindClass = node.kind === 'terminal' ? 'term' : (node.kind === 'mismatch' ? 'mismatch' : 'nt');
  const spanStr = node.span ? ` [${node.span.start}, ${node.span.end}]` : '';
  const metaParts = [];
  if (node.production_len != null) metaParts.push(`${node.parsed_symbols}/${node.production_len}`);
  if (node.binding) metaParts.push(`:${node.binding}`);
  const meta = metaParts.length ? `<span class="meta">(${metaParts.join(' · ')})</span>` : '';
  const header = `<div class="node ${kindClass}"><strong>${node.label}</strong>${meta} ${statusBadge(node)}</div>`;
  if (!node.children || node.children.length === 0) return header;
  const children = node.children.map(renderNode).map(s => `<div class="edge">${s}</div>`).join('');
  return header + children;
}

async function main() {
  const code = document.getElementById('code');
  const specInput = document.getElementById('spec');
  const clearSpecBtn = document.getElementById('clear-spec');
  const btn = document.getElementById('parse');
  const status = document.getElementById('status');
  const tree = document.getElementById('tree');

  let specText = null;

  specInput.addEventListener('change', async () => {
    const file = specInput.files && specInput.files[0];
    if (!file) { specText = null; return; }
    specText = await file.text();
    status.textContent = `Loaded spec: ${file.name}`;
  });

  clearSpecBtn.addEventListener('click', () => {
    specInput.value = '';
    specText = null;
    status.textContent = 'Cleared spec';
  });

  async function doParse() {
    status.textContent = 'Parsing...';
    try {
      const resp = await postParse(code.value, specText);
      if (!resp.ok) {
        status.textContent = 'Parse error: ' + (resp.error || 'unknown');
        tree.innerHTML = '';
        return;
      }
      status.textContent = 'OK';
      tree.innerHTML = renderNode(resp.root);
    } catch (e) {
      console.error(e);
      status.textContent = 'Request failed';
      tree.innerHTML = '';
    }
  }

  btn.addEventListener('click', doParse);
  code.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      doParse();
    }
  });
}

main();


