# when we have a title in an md documnt, it can have (#+)[W] is work in progress* after it
# (#+)[D] is done
# (#+)[U] is uncertain
# (#+) is for the unspecified header level
# Optional link syntax: (#+)[D](link_url)

import re

TYPE_MAP = {
    'W': ('wip', 'WIP'),
    'D': ('done', 'DONE'),
    'U': ('uncertain', 'U'),
}
    
PATTERN = r'(#+)\[\s*([WDU])\s*(.*?)\s*\](?:\(([^)]+)\))?'
def _replace_status(match):
    hashes = match.group(1)
    status_symbol = match.group(2)
    extra_text = match.group(3).strip()
    link_url = match.group(4)
    
    status_class, status_text = TYPE_MAP.get(status_symbol, ('unknown', '?'))
    
    # Build the badge (with optional link)
    if link_url:
        badge = f'<a href="{link_url}" class="status-badge {status_class}">{status_text}</a>'
    else:
        badge = f'<span class="status-badge {status_class}">{status_text}</span>'
    
    # Extra text comes first, badge is postfix
    if extra_text:
        result = f'{hashes} <span class="status-note {status_class}">{extra_text}</span> {badge}'
    else:
        result = f'{hashes} {badge}'
    
    return result
def process(content: str) -> str:
    """Transform (#+)[X] status markers in headers into styled spans."""
    return re.sub(PATTERN, _replace_status, content)