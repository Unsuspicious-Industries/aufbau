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
    """Recursively process a chapter and its sub-items.

    Handle both mdBook shapes: some versions wrap chapter data under a
    `"Chapter"` key, others present the chapter dict directly.
    """
    # Normalize to the inner chapter dict
    if isinstance(chapter, dict) and 'Chapter' in chapter:
        ch = chapter['Chapter']
    else:
        ch = chapter

    # If there's content, process it (be tolerant of missing fields)
    if isinstance(ch, dict) and 'content' in ch and ch['content'] is not None:
        ch['content'] = process(ch.get('content', ''))

    # Recurse into any sub-items / children
    for sub_item in ch.get('sub_items', []) if isinstance(ch, dict) else []:
        process_chapter(sub_item)

if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] == 'supports':
        # mdbook is checking if we support the renderer
        sys.exit(0)

    # Read the [context, book] JSON from stdin
    context, book = json.load(sys.stdin)

    # mdBook's JSON shape has changed across versions; be defensive and accept
    # multiple shapes. Prefer the usual `sections`, but fall back to other
    # locations so the preprocessor won't crash on newer mdBook releases.
    if isinstance(book, dict) and 'sections' in book:
        sections = book['sections']
    elif isinstance(book, dict) and 'book' in book and isinstance(book['book'], dict) and 'sections' in book['book']:
        sections = book['book']['sections']
    elif isinstance(book, dict) and 'items' in book:
        sections = book['items']
    else:
        # Unexpected shape: log keys for debugging and return the book unchanged
        print(f"preprocessor: unexpected book keys: {list(book.keys())}", file=sys.stderr)
        json.dump(book, sys.stdout)
        sys.exit(0)

    # Process each section in the book
    for section in sections:
        process_chapter(section)

    # Output the modified book as JSON
    json.dump(book, sys.stdout)
