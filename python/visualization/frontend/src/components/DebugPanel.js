import React, { useState, useEffect, useCallback } from 'react';
import styled from 'styled-components';
import { API_BASE_URL } from '../config';

const PanelContainer = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
  overflow: hidden;
  background-color: #252526;
`;

const Section = styled.div`
  padding: 15px;
  border-bottom: 1px solid #333;
`;

const SectionTitle = styled.h4`
  margin: 0 0 10px 0;
  font-size: 13px;
  color: #4ec9b0;
  text-transform: uppercase;
  letter-spacing: 0.5px;
`;

const InputRow = styled.div`
  display: flex;
  gap: 10px;
  margin-bottom: 10px;
`;

const Input = styled.input`
  flex: 1;
  padding: 8px 12px;
  background-color: #3c3c3c;
  border: 1px solid #555;
  color: #d4d4d4;
  border-radius: 3px;
  font-family: 'Consolas', monospace;
  font-size: 13px;
  
  &:focus {
    outline: none;
    border-color: #0e639c;
  }
`;

const Button = styled.button`
  padding: 8px 15px;
  background-color: #0e639c;
  border: none;
  color: white;
  border-radius: 3px;
  cursor: pointer;
  font-size: 12px;
  font-weight: 600;
  
  &:hover {
    background-color: #1177bb;
  }
  
  &:disabled {
    background-color: #555;
    cursor: not-allowed;
  }
`;

const InfoGrid = styled.div`
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 10px;
`;

const InfoItem = styled.div`
  background-color: #1e1e1e;
  padding: 10px;
  border-radius: 3px;
  border: 1px solid #333;
`;

const InfoLabel = styled.div`
  font-size: 11px;
  color: #808080;
  margin-bottom: 5px;
`;

const InfoValue = styled.div`
  font-size: 14px;
  color: #d4d4d4;
  font-family: 'Consolas', monospace;
`;

const CompletionsList = styled.div`
  max-height: 200px;
  overflow: auto;
  background-color: #1e1e1e;
  border: 1px solid #333;
  border-radius: 3px;
`;

const CompletionItem = styled.div`
  padding: 8px 12px;
  border-bottom: 1px solid #2d2d2d;
  font-family: 'Consolas', monospace;
  font-size: 12px;
  color: #9cdcfe;
  cursor: pointer;
  
  &:hover {
    background-color: #2d2d2d;
  }
  
  &:last-child {
    border-bottom: none;
  }
`;

const TypeError = styled.div`
  padding: 10px;
  background-color: #3c1e1e;
  border: 1px solid #5c1e1e;
  border-radius: 3px;
  color: #f44747;
  font-size: 12px;
`;

const ASTSection = styled.div`
  flex: 1;
  overflow: auto;
  background-color: #1e1e1e;
  border: 1px solid #333;
  border-radius: 3px;
  padding: 10px;
  font-family: 'Consolas', monospace;
  font-size: 12px;
  white-space: pre-wrap;
  word-break: break-all;
`;

function DebugPanel({ grammar, grammarValid, debugInfo }) {
  const [input, setInput] = useState('');
  const [localDebug, setLocalDebug] = useState(null);
  const [loading, setLoading] = useState(false);
  const [ast, setAst] = useState(null);

  const debugGrammar = useCallback(async () => {
    if (!grammarValid) return;
    
    setLoading(true);
    try {
      const response = await fetch(`${API_BASE_URL}/api/debug-grammar`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ spec: grammar, input })
      });
      const data = await response.json();
      setLocalDebug(data);
    } catch (err) {
      console.error('Debug failed:', err);
    } finally {
      setLoading(false);
    }
  }, [grammar, grammarValid, input]);

  const viewAST = useCallback(async () => {
    if (!grammarValid) return;
    
    try {
      const response = await fetch(`${API_BASE_URL}/api/parse-to-ast`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ spec: grammar, input })
      });
      const data = await response.json();
      setAst(data);
    } catch (err) {
      console.error('AST parse failed:', err);
    }
  }, [grammar, grammarValid, input]);

  // Auto-debug on input change (debounced)
  useEffect(() => {
    const timeout = setTimeout(() => {
      if (grammarValid && input !== undefined) {
        debugGrammar();
      }
    }, 300);
    return () => clearTimeout(timeout);
  }, [input, grammarValid, debugGrammar]);

  const displayInfo = debugInfo || localDebug;

  return (
    <PanelContainer>
      <Section>
        <SectionTitle>Test Input</SectionTitle>
        <InputRow>
          <Input
            type="text"
            value={input}
            onChange={(e) => setInput(e.target.value)}
            placeholder="Type to test grammar parsing..."
            disabled={!grammarValid}
          />
          <Button onClick={debugGrammar} disabled={!grammarValid || loading}>
            {loading ? 'Checking...' : 'Debug'}
          </Button>
        </InputRow>
        <InputRow>
          <Button onClick={viewAST} disabled={!grammarValid} style={{ backgroundColor: '#4ec9b0', color: '#1e1e1e' }}>
            View AST
          </Button>
        </InputRow>
      </Section>

      {displayInfo && (
        <>
          <Section>
            <SectionTitle>Parse Status</SectionTitle>
            <InfoGrid>
              <InfoItem>
                <InfoLabel>Current Text</InfoLabel>
                <InfoValue>{displayInfo.current_text || '(empty)'}</InfoValue>
              </InfoItem>
              <InfoItem>
                <InfoLabel>Complete</InfoLabel>
                <InfoValue style={{ color: displayInfo.is_complete ? '#4ec9b0' : '#dcdcaa' }}>
                  {displayInfo.is_complete ? 'Yes' : 'No'}
                </InfoValue>
              </InfoItem>
              <InfoItem>
                <InfoLabel>Well-Typed Trees</InfoLabel>
                <InfoValue style={{ color: displayInfo.well_typed_tree_count > 0 ? '#4ec9b0' : '#f44747' }}>
                  {displayInfo.well_typed_tree_count}
                </InfoValue>
              </InfoItem>
              <InfoItem>
                <InfoLabel>Status</InfoLabel>
                <InfoValue style={{ color: displayInfo.type_error ? '#f44747' : '#4ec9b0' }}>
                  {displayInfo.type_error ? 'Type Error' : 'Valid'}
                </InfoValue>
              </InfoItem>
            </InfoGrid>
          </Section>

          {displayInfo.type_error && (
            <Section>
              <SectionTitle>Type Error</SectionTitle>
              <TypeError>{displayInfo.type_error}</TypeError>
            </Section>
          )}

          {displayInfo.completions && (
            <Section>
              <SectionTitle>Valid Completions ({displayInfo.completions.patterns?.length || 0})</SectionTitle>
              <CompletionsList>
                {displayInfo.completions.patterns?.map((pattern, idx) => (
                  <CompletionItem 
                    key={idx}
                    onClick={() => setInput(displayInfo.current_text + pattern)}
                  >
                    {pattern}
                    {displayInfo.completions.examples?.[idx] && (
                      <span style={{ color: '#6a9955', marginLeft: '10px' }}>
                        // {displayInfo.completions.examples[idx]}
                      </span>
                    )}
                  </CompletionItem>
                ))}
                {(!displayInfo.completions.patterns || displayInfo.completions.patterns.length === 0) && (
                  <CompletionItem style={{ color: '#808080', fontStyle: 'italic' }}>
                    No completions available
                  </CompletionItem>
                )}
              </CompletionsList>
            </Section>
          )}
        </>
      )}

      {ast && (
        <Section style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
          <SectionTitle>AST (S-Expression)</SectionTitle>
          <ASTSection>
            {ast.success ? ast.sexpr : <span style={{ color: '#f44747' }}>{ast.error}</span>}
          </ASTSection>
        </Section>
      )}

      {!displayInfo && grammarValid && (
        <Section style={{ textAlign: 'center', color: '#808080', paddingTop: '40px' }}>
          Type in the test input above to see grammar debugging info
        </Section>
      )}

      {!grammarValid && (
        <Section style={{ textAlign: 'center', color: '#f44747', paddingTop: '40px' }}>
          Fix grammar errors to enable debugging
        </Section>
      )}
    </PanelContainer>
  );
}

export default DebugPanel;
