import React from 'react';
import Editor from 'react-simple-code-editor';
import styled from 'styled-components';

const EditorContainer = styled.div`
  flex: 1;
  overflow: auto;
  background-color: #1e1e1e;
  
  & textarea {
    font-family: 'Consolas', 'Monaco', 'Courier New', monospace !important;
    font-size: 14px !important;
    line-height: 1.5 !important;
  }
`;

const ErrorList = styled.div`
  max-height: 150px;
  overflow: auto;
  background-color: #2d1f1f;
  border-top: 1px solid #5c1e1e;
  padding: 10px;
`;

const ErrorItem = styled.div`
  color: #f44747;
  font-size: 12px;
  margin-bottom: 5px;
  padding: 5px;
  background-color: #3c1e1e;
  border-radius: 3px;
`;

// Simple syntax highlighting for .spec files
const highlight = (code) => {
  // Split into lines for line-by-line processing
  const lines = code.split('\n');
  
  return lines.map((line, i) => {
    let highlighted = line
      // Productions: Name ::= ...
      .replace(/^(\w+)(\s*\([^)]*\))?\s*(::=)/g, '<span style="color:#4ec9b0;font-weight:bold;">$1$2</span> <span style="color:#c586c0;">$3</span>')
      // Typing rule names: (var), (lambda), (app)
      .replace(/\((\w+)\)/g, '(<span style="color:#dcdcaa;">$1</span>)')
      // Terminals in quotes
      .replace(/('[^']*')/g, '<span style="color:#ce9178;">$1</span>')
      // Regex patterns
      .replace(/(\/[^\/]+\/)/g, '<span style="color:#9cdcfe;">$1</span>')
      // Bindings [x], [τ]
      .replace(/\[([a-zτA-Z0-9_]+)\]/g, '[<span style="color:#9cdcfe;">$1</span>]')
      // Keywords in rules
      .replace(/\b(Γ|⊢|:|→|\->|ε|∈)\b/g, '<span style="color:#569cd6;">$1</span>')
      // Rule separator
      .replace(/^(---+)$/gm, '<span style="color:#808080;">$1</span>');
    
    return `<div>${highlighted || ' '}</div>`;
  }).join('');
};

function GrammarEditor({ value, onChange, errors }) {
  return (
    <>
      <EditorContainer>
        <Editor
          value={value}
          onValueChange={onChange}
          highlight={highlight}
          padding={20}
          style={{
            fontFamily: '"Consolas", "Monaco", "Courier New", monospace',
            fontSize: 14,
            backgroundColor: '#1e1e1e',
            minHeight: '100%',
          }}
          textareaClassName="grammar-editor"
        />
      </EditorContainer>
      
      {errors && errors.length > 0 && (
        <ErrorList>
          {errors.map((error, idx) => (
            <ErrorItem key={idx}>{error}</ErrorItem>
          ))}
        </ErrorList>
      )}
    </>
  );
}

export default GrammarEditor;
