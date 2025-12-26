from proofs import process as process_proofs
from status import process as process_status
from paths import process as process_paths

import json
import sys

def process(content: str) -> str:
    """Apply all preprocessors to the content."""
    content = process_proofs(content)
    content = process_status(content)
    content = process_paths(content)
    return content

def process_chapter(chapter):
    """Recursively process a chapter and its sub-items."""
    if 'Chapter' in chapter:
        ch = chapter['Chapter']
        ch['content'] = process(ch['content'])
        for sub_item in ch.get('sub_items', []):
            process_chapter(sub_item)

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'supports':
        # mdbook is checking if we support the renderer
        sys.exit(0)

    # Read the [context, book] JSON from stdin
    context, book = json.load(sys.stdin)

    # Process each section in the book
    for section in book['sections']:
        process_chapter(section)

    # Output the modified book as JSON
    json.dump(book, sys.stdout)
