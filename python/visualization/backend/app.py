"""
P7 Visualization Platform - Flask Backend API
Provides endpoints for grammar validation, debugging, and constrained generation.
"""

from __future__ import annotations

import os
import sys
import json
import traceback
from pathlib import Path
from dataclasses import dataclass, asdict
from typing import Optional, Dict, Any, List
from flask import Flask, request, jsonify, Response
from flask_cors import CORS

# Add parent directory to path to import proposition_7
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import proposition_7 as p7
from proposition_7 import Grammar, ConstrainedGenerator

app = Flask(__name__)
CORS(app)

# Store active models and generators
_models: Dict[str, Any] = {}
_generators: Dict[str, ConstrainedGenerator] = {}


@dataclass
class GrammarValidationResult:
    valid: bool
    errors: List[str]
    start_nonterminal: Optional[str] = None


@dataclass
class DebugInfo:
    current_text: str
    is_complete: bool
    completions: Dict[str, List[str]]
    valid_token_count: int
    well_typed_tree_count: int


@dataclass
class GenerationResult:
    text: str
    is_complete: bool
    tokens_generated: int
    stopped_reason: str
    constrained_tokens: List[str]
    unconstrained_tokens: List[str]


def validate_grammar(spec: str) -> GrammarValidationResult:
    """Validate a grammar spec and return detailed error information."""
    errors = []
    start_nt = None
    
    try:
        grammar = Grammar(spec)
        start_nt = grammar.start_nonterminal()
        return GrammarValidationResult(
            valid=True,
            errors=[],
            start_nonterminal=start_nt
        )
    except Exception as e:
        error_msg = str(e)
        errors.append(error_msg)
        
        # Try to provide more helpful error messages
        if "Grammar parse error" in error_msg:
            # Extract line number if available
            if "line" in error_msg.lower():
                errors.append("Check syntax around the indicated line number")
            else:
                errors.append("Common issues:")
                errors.append("  - Missing '::=' in production rules")
                errors.append("  - Unmatched parentheses or quotes")
                errors.append("  - Invalid regex patterns")
                errors.append("  - Typing rules must be separated from grammar by blank lines")
        
        return GrammarValidationResult(
            valid=False,
            errors=errors,
            start_nonterminal=None
        )


@app.route('/api/health', methods=['GET'])
def health_check():
    """Health check endpoint."""
    return jsonify({"status": "ok", "version": p7.__version__})


@app.route('/api/grammars', methods=['GET'])
def list_grammars():
    """List all available built-in grammars."""
    grammars = []
    for name in p7.list_grammars():
        info = p7.get_grammar_info(name)
        grammars.append({
            "name": name,
            "display_name": info.get("name", name),
            "description": info.get("description", ""),
            "short": info.get("short", name),
        })
    return jsonify({"grammars": grammars})


@app.route('/api/grammars/<name>', methods=['GET'])
def get_grammar(name: str):
    """Get a built-in grammar spec by name."""
    try:
        spec = p7.get_grammar(name)
        info = p7.get_grammar_info(name)
        return jsonify({
            "name": name,
            "spec": spec,
            "info": info
        })
    except ValueError as e:
        return jsonify({"error": str(e)}), 404


@app.route('/api/validate-grammar', methods=['POST'])
def validate_grammar_endpoint():
    """Validate a grammar spec."""
    data = request.get_json()
    if not data or 'spec' not in data:
        return jsonify({"error": "Missing 'spec' field"}), 400
    
    spec = data['spec']
    result = validate_grammar(spec)
    return jsonify(asdict(result))


@app.route('/api/debug-grammar', methods=['POST'])
def debug_grammar():
    """Debug a grammar at a given input state."""
    data = request.get_json()
    if not data or 'spec' not in data:
        return jsonify({"error": "Missing 'spec' field"}), 400
    
    spec = data['spec']
    input_text = data.get('input', '')
    
    # Validate grammar first
    validation = validate_grammar(spec)
    if not validation.valid:
        return jsonify({
            "valid": False,
            "errors": validation.errors
        })
    
    try:
        grammar = Grammar(spec)
        generator = ConstrainedGenerator(grammar)
        
        # Feed input if provided
        if input_text:
            try:
                generator.feed_raw(input_text)
            except TypeError as e:
                return jsonify({
                    "valid": True,
                    "type_error": str(e),
                    "current_text": input_text,
                    "is_complete": False,
                    "completions": {"patterns": [], "examples": []},
                    "well_typed_tree_count": 0
                })
        
        # Get debug info
        debug_info = generator.debug_completions()
        well_typed_count = generator.well_typed_tree_count()
        
        return jsonify({
            "valid": True,
            "current_text": generator.current_text(),
            "is_complete": generator.is_complete(),
            "completions": debug_info,
            "well_typed_tree_count": well_typed_count,
            "type_error": None
        })
    except Exception as e:
        return jsonify({
            "valid": False,
            "errors": [str(e), traceback.format_exc()]
        }), 500


@app.route('/api/get-completions', methods=['POST'])
def get_completions():
    """Get valid completions for a given input state."""
    data = request.get_json()
    if not data or 'spec' not in data:
        return jsonify({"error": "Missing 'spec' field"}), 400
    
    spec = data['spec']
    input_text = data.get('input', '')
    
    try:
        grammar = Grammar(spec)
        generator = ConstrainedGenerator(grammar)
        
        if input_text:
            generator.feed_raw(input_text)
        
        completions = generator.debug_completions()
        
        return jsonify({
            "current_text": generator.current_text(),
            "completions": completions,
            "is_complete": generator.is_complete()
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500


def generate_stream(
    spec: str,
    prompt: str,
    initial: str,
    model_name: str,
    max_tokens: int = 50,
    compare_unconstrained: bool = True
):
    """Stream generation results for Server-Sent Events."""
    try:
        # Import here to handle optional dependencies gracefully
        from transformers import AutoModelForCausalLM, AutoTokenizer
        import torch
        
        # Load model and tokenizer
        yield f"data: {json.dumps({'type': 'status', 'message': f'Loading model {model_name}...'})}\n\n"
        
        tokenizer = AutoTokenizer.from_pretrained(model_name)
        model = AutoModelForCausalLM.from_pretrained(model_name)
        model.eval()
        
        if torch.cuda.is_available():
            model = model.cuda()
        
        vocab = [tokenizer.decode([i]) for i in range(tokenizer.vocab_size)]
        
        yield f"data: {json.dumps({'type': 'status', 'message': 'Starting generation...'})}\n\n"
        
        # Create constrained sampler
        sampler = p7.TypedSampler(
            grammar=spec,
            vocab=vocab,
            logit_fn=lambda: model(
                tokenizer.encode(prompt + initial, return_tensors="pt")
            ).logits[0, -1, :].tolist()
        )
        
        constrained_text = initial
        unconstrained_text = initial
        
        if initial:
            sampler.feed(initial)
        
        for step in range(max_tokens):
            # Get logits
            input_ids = tokenizer.encode(prompt + constrained_text, return_tensors="pt")
            if torch.cuda.is_available():
                input_ids = input_ids.cuda()
            
            with torch.no_grad():
                logits = model(input_ids).logits[0, -1, :].cpu().tolist()
            
            # Update sampler logits
            sampler.logit_fn = lambda: logits
            
            # Get constrained token
            constrained_token = sampler.infer_greedy(k=1, pre_top_k=100)
            
            # Get unconstrained token
            unconstrained_idx = max(range(len(logits)), key=lambda i: logits[i])
            unconstrained_token = tokenizer.decode([unconstrained_idx])
            
            if constrained_token is None:
                yield f"data: {json.dumps({'type': 'done', 'reason': 'no_valid_tokens'})}\n\n"
                break
            
            # Feed constrained token
            try:
                sampler.feed(constrained_token)
                constrained_text += constrained_token
            except TypeError as e:
                yield f"data: {json.dumps({'type': 'error', 'message': f'Type error: {e}'})}\n\n"
                break
            
            unconstrained_text += unconstrained_token
            
            token_data = {
                'type': 'token',
                'step': step,
                'constrained_token': constrained_token,
                'unconstrained_token': unconstrained_token,
                'constrained_text': constrained_text,
                'unconstrained_text': unconstrained_text
            }
            yield f"data: {json.dumps(token_data)}\n\n"
            
            if sampler.is_complete():
                done_data = {'type': 'done', 'reason': 'complete', 'is_complete': True}
                yield f"data: {json.dumps(done_data)}\n\n"
                break
        else:
            done_data = {'type': 'done', 'reason': 'max_tokens', 'is_complete': False}
            yield f"data: {json.dumps(done_data)}\n\n"
            
    except ImportError as e:
        yield f"data: {json.dumps({'type': 'error', 'message': f'Missing dependency: {e}'})}\n\n"
    except Exception as e:
        yield f"data: {json.dumps({'type': 'error', 'message': str(e), 'traceback': traceback.format_exc()})}\n\n"


@app.route('/api/generate', methods=['POST'])
def generate():
    """Generate text using constrained decoding (non-streaming)."""
    data = request.get_json()
    if not data:
        return jsonify({"error": "No data provided"}), 400
    
    spec = data.get('spec')
    prompt = data.get('prompt', '')
    initial = data.get('initial', '')
    model_name = data.get('model', 'gpt2')
    max_tokens = data.get('max_tokens', 50)
    stream = data.get('stream', False)
    
    if not spec:
        return jsonify({"error": "Missing 'spec' field"}), 400
    
    if stream:
        return Response(
            generate_stream(spec, prompt, initial, model_name, max_tokens),
            mimetype='text/event-stream'
        )
    
    # Non-streaming generation
    try:
        from transformers import AutoModelForCausalLM, AutoTokenizer
        import torch
        
        tokenizer = AutoTokenizer.from_pretrained(model_name)
        model = AutoModelForCausalLM.from_pretrained(model_name)
        model.eval()
        
        if torch.cuda.is_available():
            model = model.cuda()
        
        vocab = [tokenizer.decode([i]) for i in range(tokenizer.vocab_size)]
        
        sampler = p7.TypedSampler(
            grammar=spec,
            vocab=vocab,
            logit_fn=lambda: []
        )
        
        constrained_text = initial
        unconstrained_text = initial
        constrained_tokens = []
        unconstrained_tokens = []
        
        if initial:
            sampler.feed(initial)
        
        stopped_reason = "max_tokens"
        
        for step in range(max_tokens):
            input_ids = tokenizer.encode(prompt + constrained_text, return_tensors="pt")
            if torch.cuda.is_available():
                input_ids = input_ids.cuda()
            
            with torch.no_grad():
                logits = model(input_ids).logits[0, -1, :].cpu().tolist()
            
            sampler.logit_fn = lambda: logits
            
            constrained_token = sampler.infer_greedy(k=1, pre_top_k=100)
            
            unconstrained_idx = max(range(len(logits)), key=lambda i: logits[i])
            unconstrained_token = tokenizer.decode([unconstrained_idx])
            
            if constrained_token is None:
                stopped_reason = "no_valid"
                break
            
            try:
                sampler.feed(constrained_token)
                constrained_text += constrained_token
                constrained_tokens.append(constrained_token)
            except TypeError as e:
                stopped_reason = f"type_error: {e}"
                break
            
            unconstrained_text += unconstrained_token
            unconstrained_tokens.append(unconstrained_token)
            
            if sampler.is_complete():
                stopped_reason = "complete"
                break
        
        return jsonify({
            "success": True,
            "constrained": {
                "text": constrained_text,
                "tokens": constrained_tokens,
                "is_complete": sampler.is_complete()
            },
            "unconstrained": {
                "text": unconstrained_text,
                "tokens": unconstrained_tokens
            },
            "stopped_reason": stopped_reason,
            "tokens_generated": len(constrained_tokens)
        })
        
    except ImportError as e:
        return jsonify({"error": f"Missing dependency: {e}. Install with: pip install transformers torch"}), 500
    except Exception as e:
        return jsonify({"error": str(e), "traceback": traceback.format_exc()}), 500


@app.route('/api/parse-to-ast', methods=['POST'])
def parse_to_ast():
    """Parse input and return AST representation."""
    data = request.get_json()
    if not data or 'spec' not in data:
        return jsonify({"error": "Missing 'spec' field"}), 400
    
    spec = data['spec']
    input_text = data.get('input', '')
    
    try:
        grammar = Grammar(spec)
        generator = ConstrainedGenerator(grammar)
        
        if input_text:
            generator.feed_raw(input_text)
        
        try:
            sexpr = generator.to_sexpr()
            return jsonify({
                "success": True,
                "sexpr": sexpr,
                "current_text": generator.current_text(),
                "is_complete": generator.is_complete()
            })
        except Exception as e:
            return jsonify({
                "success": False,
                "error": str(e),
                "current_text": generator.current_text(),
                "is_complete": generator.is_complete()
            })
    except Exception as e:
        return jsonify({"error": str(e)}), 500


if __name__ == '__main__':
    app.run(debug=True, host='0.0.0.0', port=5001)
