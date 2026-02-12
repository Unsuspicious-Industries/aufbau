import React, { useState, useEffect, useCallback } from 'react';
import styled from 'styled-components';
import GrammarEditor from './components/GrammarEditor';
import DebugPanel from './components/DebugPanel';
import GenerationPanel from './components/GenerationPanel';
import Header from './components/Header';
import { API_BASE_URL } from './config';

const AppContainer = styled.div`
  display: flex;
  flex-direction: column;
  height: 100vh;
  background-color: #1e1e1e;
  color: #d4d4d4;
`;

const MainContent = styled.div`
  display: flex;
  flex: 1;
  overflow: hidden;
`;

const LeftPanel = styled.div`
  width: 50%;
  display: flex;
  flex-direction: column;
  border-right: 1px solid #333;
`;

const RightPanel = styled.div`
  width: 50%;
  display: flex;
  flex-direction: column;
`;

const PanelHeader = styled.div`
  padding: 10px 15px;
  background-color: #2d2d2d;
  border-bottom: 1px solid #333;
  font-weight: 600;
  font-size: 14px;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const TabContainer = styled.div`
  display: flex;
  gap: 5px;
`;

const Tab = styled.button`
  padding: 5px 12px;
  background-color: ${props => props.active ? '#0e639c' : 'transparent'};
  border: 1px solid ${props => props.active ? '#0e639c' : '#555'};
  color: ${props => props.active ? '#fff' : '#ccc'};
  border-radius: 3px;
  cursor: pointer;
  font-size: 12px;
  
  &:hover {
    background-color: ${props => props.active ? '#0e639c' : '#3c3c3c'};
  }
`;

// Default STLC grammar
const DEFAULT_GRAMMAR = `Identifier ::= /[A-Za-z_][A-Za-z0-9_]*
Variable(var) ::= Identifier[x]
TypeName ::= /[A-Za-z0-9_]+/
BaseType ::= TypeName | '(' Type ')'
FunctionType ::= BaseType '->' Type
Type ::= BaseType | FunctionType
Lambda(lambda) ::= 'λ' Identifier[a] ':' Type[τ] '.' Expression[e]
AtomicExpression ::= Variable | '(' Expression ')' | Lambda
Application(app) ::= AtomicExpression[l] Expression[r]
Expression ::= AtomicExpression | Application

x ∈ Γ
----- (var)
Γ(x)

Γ[a:τ] ⊢ e : ?B
--------------- (lambda)
τ → ?B

Γ ⊢ r : ?A → ?B, Γ ⊢ l : ?A
----------------------------- (app)
?B`;

function App() {
  const [grammar, setGrammar] = useState(DEFAULT_GRAMMAR);
  const [grammarStatus, setGrammarStatus] = useState({ valid: true, errors: [] });
  const [debugInfo, setDebugInfo] = useState(null);
  const [activeTab, setActiveTab] = useState('debug'); // 'debug' or 'examples'
  const [availableGrammars, setAvailableGrammars] = useState([]);
  const [isLoading, setIsLoading] = useState(false);

  // Load available grammars on mount
  useEffect(() => {
    fetch(`${API_BASE_URL}/api/grammars`)
      .then(res => res.json())
      .then(data => {
        setAvailableGrammars(data.grammars);
      })
      .catch(err => console.error('Failed to load grammars:', err));
  }, []);

  // Validate grammar when it changes (with debounce)
  useEffect(() => {
    const timeoutId = setTimeout(() => {
      validateGrammar(grammar);
    }, 500);

    return () => clearTimeout(timeoutId);
  }, [grammar]);

  const validateGrammar = useCallback(async (spec) => {
    try {
      const response = await fetch(`${API_BASE_URL}/api/validate-grammar`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ spec })
      });
      const data = await response.json();
      setGrammarStatus(data);
    } catch (err) {
      setGrammarStatus({ valid: false, errors: [err.message] });
    }
  }, []);

  const loadGrammarExample = useCallback(async (name) => {
    setIsLoading(true);
    try {
      const response = await fetch(`${API_BASE_URL}/api/grammars/${name}`);
      const data = await response.json();
      if (data.spec) {
        setGrammar(data.spec);
      }
    } catch (err) {
      console.error('Failed to load grammar:', err);
    } finally {
      setIsLoading(false);
    }
  }, []);

  const updateDebugInfo = useCallback((info) => {
    setDebugInfo(info);
  }, []);

  return (
    <AppContainer>
      <Header 
        grammarStatus={grammarStatus}
        availableGrammars={availableGrammars}
        onLoadExample={loadGrammarExample}
        isLoading={isLoading}
      />
      <MainContent>
        <LeftPanel>
          <PanelHeader>
            <span>Grammar Editor (.spec)</span>
            <TabContainer>
              {grammarStatus.valid ? (
                <span style={{ color: '#4ec9b0', fontSize: '12px' }}>✓ Valid</span>
              ) : (
                <span style={{ color: '#f44747', fontSize: '12px' }}>✗ Invalid</span>
              )}
            </TabContainer>
          </PanelHeader>
          <GrammarEditor 
            value={grammar}
            onChange={setGrammar}
            errors={grammarStatus.errors}
          />
        </LeftPanel>
        
        <RightPanel>
          <PanelHeader>
            <TabContainer>
              <Tab 
                active={activeTab === 'debug'} 
                onClick={() => setActiveTab('debug')}
              >
                Grammar Debug
              </Tab>
              <Tab 
                active={activeTab === 'examples'} 
                onClick={() => setActiveTab('examples')}
              >
                Examples
              </Tab>
            </TabContainer>
          </PanelHeader>
          
          {activeTab === 'debug' && (
            <DebugPanel 
              grammar={grammar}
              grammarValid={grammarStatus.valid}
              debugInfo={debugInfo}
            />
          )}
          
          {activeTab === 'examples' && (
            <div style={{ padding: '20px', overflow: 'auto' }}>
              <h3>Available Grammar Examples</h3>
              {availableGrammars.map(g => (
                <div 
                  key={g.name}
                  style={{ 
                    padding: '15px', 
                    marginBottom: '10px', 
                    backgroundColor: '#252526',
                    borderRadius: '5px',
                    cursor: 'pointer'
                  }}
                  onClick={() => loadGrammarExample(g.name)}
                >
                  <strong style={{ color: '#4ec9b0' }}>{g.display_name}</strong>
                  <p style={{ margin: '5px 0 0 0', fontSize: '13px', color: '#9cdcfe' }}>
                    {g.description}
                  </p>
                </div>
              ))}
            </div>
          )}
        </RightPanel>
      </MainContent>
      
      <GenerationPanel 
        grammar={grammar}
        grammarValid={grammarStatus.valid}
        onDebugUpdate={updateDebugInfo}
      />
    </AppContainer>
  );
}

export default App;
