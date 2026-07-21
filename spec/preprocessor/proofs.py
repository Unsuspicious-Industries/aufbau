"""
Process theorem-like blocks in markdown.

Syntax:
    >X Title
    content here
    <

Where X is one of:
    T - Theorem
    D - Definition
    P - Proof
    L - Lemma
    E - Example
    R - Remark
    A - Assumption
"""

import re

TYPE_MAP = {
    'T': ('theorem', 'Theorem'),
    'D': ('definition', 'Definition'),
    'P': ('proof', 'Proof'),
    'L': ('lemma', 'Lemma'),
    'E': ('example', 'Example'),
    'R': ('remark', 'Remark'),
    'A': ('assumption', 'Assumption'),
}

PATTERN = r'>([TDPLERA])\s*(.*?)\n(.*?)\n<$'


def _replace_block(match):
    symbol = match.group(1)
    title = match.group(2).strip()
    body = match.group(3)

    block_type, display_name = TYPE_MAP.get(symbol, ('note', 'Note'))
    title_text = f"{display_name}" + (f": {title}" if title else "")

    return f'''<div class="{block_type}">
<div class="{block_type}-title">{title_text}</div>

{body}
</div>'''


def process(content: str) -> str:
    """Transform theorem blocks into styled HTML divs."""
    return re.sub(PATTERN, _replace_block, content, flags=re.MULTILINE | re.DOTALL)
