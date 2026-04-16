import re

MERMAID_BLOCK_RE = re.compile(r"```mermaid\n(.*?)\n```", re.DOTALL)


def process(content: str) -> str:
    """Convert mermaid code blocks to HTML divs for client-side rendering."""

    def replace_mermaid(match):
        diagram = match.group(1).strip()
        return f'<div class="mermaid">\n{diagram}\n</div>'

    return MERMAID_BLOCK_RE.sub(replace_mermaid, content)
