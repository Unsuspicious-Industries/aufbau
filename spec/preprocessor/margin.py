"""
Process margin annotation blocks in markdown.

Syntax:
    >N Optional title
    content here — shown as a handwritten margin note
    <

    >I widget-id
    {"steps": [...], "label": "..."}   <- JSON on one line, or multiline body used as label
    <

Where:
    N - margin Note  (handwritten sticky, static text)
    I - Interactive  (synthesizer widget; body is JSON config)

The preprocessor emits:
    - A <span class="margin-anchor" data-margin="ID"> at the point of the
      block in the document flow.
    - A <div class="margin-note" data-margin="ID"> (or margin-interactive)
      that margin.js will pick up and position.

Both are placed adjacent in the HTML so margin.js can match them by the
shared data-margin attribute and align the note to the anchor's vertical
position.
"""

import re
import json as _json

# Counter for unique IDs within a single document processing run.
# Reset per-document by using a mutable container so it works cleanly
# across multiple calls in the same process.
_COUNTER = [0]

PATTERN = r'>([NI])\s*(.*?)\n(.*?)\n<$'


def _next_id(prefix: str) -> str:
    _COUNTER[0] += 1
    return f"margin-{prefix}-{_COUNTER[0]}"


def _replace_block(match):
    symbol = match.group(1)       # 'N' or 'I'
    title  = match.group(2).strip()
    body   = match.group(3).strip()

    mid = _next_id(symbol.lower())

    anchor = f'<span class="margin-anchor" data-margin="{mid}"></span>'

    if symbol == 'N':
        title_html = f'<span class="margin-note-title">{title}</span>\n' if title else ''
        note = (
            f'<div class="margin-note" data-margin="{mid}">'
            f'{title_html}'
            f'{body}'
            f'</div>'
        )
    else:
        # Interactive: body must be valid JSON or we pass it through as-is.
        # title is used as the widget label / grammar name.
        # Validate JSON loosely — if it fails we still emit the div.
        try:
            _json.loads(body)
            data_attr = body.replace('"', '&quot;')
        except ValueError:
            data_attr = '{}'

        label_attr = title.replace('"', '&quot;')
        note = (
            f'<div class="margin-interactive" data-margin="{mid}" '
            f'data-label="{label_attr}" '
            f'data-steps="{data_attr}">'
            f'</div>'
        )

    # Anchor first (inline), then note (positioned by JS).
    return f'{anchor}\n\n{note}'


def process(content: str) -> str:
    """Transform margin note/interactive blocks into HTML."""
    _COUNTER[0] = 0  # reset per document
    return re.sub(PATTERN, _replace_block, content, flags=re.MULTILINE | re.DOTALL)
