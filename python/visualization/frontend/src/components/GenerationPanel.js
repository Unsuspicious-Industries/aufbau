import React, { useState, useRef, useCallback } from 'react';
import styled from 'styled-components';
import { API_BASE_URL } from '../config';

const PanelContainer = styled.div`
  height: 300px;
  display: flex;
  flex-direction: column;
  border-top: 1px solid #333;
  background-color: #252526;
`;

const PanelHeader = styled.div`
  padding: 10px 15px;
  background-color: #2d2d2d;
  border-bottom: 1px solid #333;
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const PanelTitle = styled.span`
  font-weight: 600;
  font-size: 14px;
  color: #d4d4d4;
`;

const ConfigRow = styled.div`
  display: flex;
  gap: 15px;
  align-items: center;
`;

const ConfigItem = styled.div`
  display: flex;
  align-items: center;
  gap: 5px;
`;

const ConfigLabel = styled.label`
  font-size: 11px;
  color: #808080;
  text-transform: uppercase;
`;

const Input = styled.input`
  padding: 5px 8px;
  background-color: #3c3c3c;
  border: 1px solid #555;
  color: #d4d4d4;
  border-radius: 3px;
  font-size: 12px;
  
  &:focus {
    outline: none;
    border-color: #0e639c;
  }
`;

const Select = styled.select`
  padding: 5px 8px;
  background-color: #3c3c3c;
  border: 1px solid #555;
  color: #d4d4d4;
  border-radius: 3px;
  font-size: 12px;
`;

const Button = styled.button`
  padding: 6px 15px;
  background-color: ${props => props.variant === 'secondary' ? '#3c3c3c' : '#0e639c'};
  border: 1px solid ${props => props.variant === 'secondary' ? '#555' : '#0e639c'};
  color: white;
  border-radius: 3px;
  cursor: pointer;
  font-size: 12px;
  font-weight: 600;
  
  &:hover {
    background-color: ${props => props.variant === 'secondary' ? '#4c4c4c' : '#1177bb'};
  }
  
  &:disabled {
    background-color: #555;
    border-color: #555;
    cursor: not-allowed;
  }
`;

const ContentArea = styled.div`
  flex: 1;
  display: flex;
  overflow: hidden;
`;

const PromptSection = styled.div`
  width: 35%;
  padding: 15px;
  border-right: 1px solid #333;
  display: flex;
  flex-direction: column;
  gap: 10px;
`;

const SectionTitle = styled.label`
  font-size: 11px;
  color: #808080;
  text-transform: uppercase;
  font-weight: 600;
`;

const TextArea = styled.textarea`
  flex: 1;
  padding: 10px;
  background-color: #1e1e1e;
  border: 1px solid #333;
  color: #d4d4d4;
  border-radius: 3px;
  font-family: 'Consolas', monospace;
  font-size: 13px;
  resize: none;
  
  &:focus {
    outline: none;
    border-color: #0e639c;
  }
  
  &::placeholder {
    color: #555;
  }
`;

const OutputSection = styled.div`
  width: 65%;
  display: flex;
`;

const OutputPane = styled.div`
  flex: 1;
  padding: 15px;
  border-right: ${props => props.border ? '1px solid #333' : 'none'};
  display: flex;
  flex-direction: column;
  gap: 10px;
  background-color: ${props => props.highlight ? '#1e2d1e' : '#252526'};
`;

const OutputLabel = styled.div`
  font-size: 11px;
  font-weight: 600;
  text-transform: uppercase;
  color: ${props => props.color || '#808080'};
  display: flex;
  justify-content: space-between;
  align-items: center;
`;

const OutputBox = styled.div`
  flex: 1;
  padding: 12px;
  background-color: #1e1e1e;
  border: 1px solid #333;
  border-radius: 3px;
  font-family: 'Consolas', monospace;
  font-size: 13px;
  overflow: auto;
  white-space: pre-wrap;
  word-break: break-all;
  color: #d4d4d4;
  line-height: 1.5;
`;

const StatusBadge = styled.span`
  padding: 2px 8px;
  border-radius: 3px;
  font-size: 10px;
  font-weight: 600;
  background-color: ${props => {
    switch(props.status) {
      case 'complete': return '#1e4d2b';
      case 'generating': return '#1e3a5f';
      case 'error': return '#5c1e1e';
      default: return '#3c3c3c';
    }
  }};
  color: ${props => {
    switch(props.status) {
      case 'complete': return '#4ec9b0';
      case 'generating': return '#569cd6';
      case 'error': return '#f44747';
      default: return '#808080';
    }
  }};
`;

const TokenStream = styled.div`
  font-size: 11px;
  color: #6a9955;
  margin-top: 5px;
  font-family: 'Consolas', monospace;
`;

function GenerationPanel({ grammar, grammarValid, onDebugUpdate }) {
  const [prompt, setPrompt] = useState('Complete this expression:\n');
  const [initial, setInitial] = useState('λx:Int.');
  const [model, setModel] = useState('gpt2');
  const [maxTokens, setMaxTokens] = useState(30);
  const [isGenerating, setIsGenerating] = useState(false);
  
  const [constrainedOutput, setConstrainedOutput] = useState('');
  const [unconstrainedOutput, setUnconstrainedOutput] = useState('');
  const [constrainedTokens, setConstrainedTokens] = useState([]);
  const [unconstrainedTokens, setUnconstrainedTokens] = useState([]);
  const [status, setStatus] = useState('idle');
  const [stoppedReason, setStoppedReason] = useState('');
  const [isComplete, setIsComplete] = useState(false);

  const abortControllerRef = useRef(null);

  const generate = useCallback(async () => {
    if (!grammarValid || isGenerating) return;
    
    setIsGenerating(true);
    setConstrainedOutput(initial);
    setUnconstrainedOutput(initial);
    setConstrainedTokens([]);
    setUnconstrainedTokens([]);
    setStatus('generating');
    setStoppedReason('');
    setIsComplete(false);
    
    try {
      const response = await fetch(`${API_BASE_URL}/api/generate`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          spec: grammar,
          prompt,
          initial,
          model,
          max_tokens: parseInt(maxTokens),
          stream: false
        })
      });
      
      const data = await response.json();
      
      if (data.error) {
        setStatus('error');
        setConstrainedOutput(`Error: ${data.error}`);
        setUnconstrainedOutput(`Error: ${data.error}`);
      } else {
        setConstrainedOutput(data.constrained.text);
        setUnconstrainedOutput(data.unconstrained.text);
        setConstrainedTokens(data.constrained.tokens);
        setUnconstrainedTokens(data.unconstrained.tokens);
        setIsComplete(data.constrained.is_complete);
        setStoppedReason(data.stopped_reason);
        setStatus(data.constrained.is_complete ? 'complete' : 'stopped');
        
        // Update debug info with final result
        onDebugUpdate({
          current_text: data.constrained.text,
          is_complete: data.constrained.is_complete,
          completions: { patterns: [], examples: [] },
          well_typed_tree_count: data.constrained.is_complete ? 1 : 0,
          type_error: null
        });
      }
    } catch (err) {
      setStatus('error');
      setConstrainedOutput(`Error: ${err.message}`);
      setUnconstrainedOutput(`Error: ${err.message}`);
    } finally {
      setIsGenerating(false);
    }
  }, [grammar, grammarValid, prompt, initial, model, maxTokens, isGenerating, onDebugUpdate]);

  const clear = useCallback(() => {
    setConstrainedOutput('');
    setUnconstrainedOutput('');
    setConstrainedTokens([]);
    setUnconstrainedTokens([]);
    setStatus('idle');
    setStoppedReason('');
    setIsComplete(false);
  }, []);

  const formatTokenStream = (tokens) => {
    if (tokens.length === 0) return '';
    return tokens.slice(-10).map(t => t.replace(/\s/g, '·')).join(' ');
  };

  return (
    <PanelContainer>
      <PanelHeader>
        <PanelTitle>Constrained Generation</PanelTitle>
        <ConfigRow>
          <ConfigItem>
            <ConfigLabel>Model</ConfigLabel>
            <Select value={model} onChange={(e) => setModel(e.target.value)}>
              <option value="gpt2">GPT-2</option>
              <option value="gpt2-medium">GPT-2 Medium</option>
              <option value="EleutherAI/pythia-160m">Pythia-160M</option>
            </Select>
          </ConfigItem>
          
          <ConfigItem>
            <ConfigLabel>Max Tokens</ConfigLabel>
            <Input
              type="number"
              value={maxTokens}
              onChange={(e) => setMaxTokens(e.target.value)}
              style={{ width: '60px' }}
            />
          </ConfigItem>
          
          <Button 
            onClick={generate} 
            disabled={!grammarValid || isGenerating}
          >
            {isGenerating ? 'Generating...' : 'Generate'}
          </Button>
          
          <Button 
            variant="secondary" 
            onClick={clear}
            disabled={isGenerating}
          >
            Clear
          </Button>
        </ConfigRow>
      </PanelHeader>
      
      <ContentArea>
        <PromptSection>
          <SectionTitle>Prompt</SectionTitle>
          <TextArea
            value={prompt}
            onChange={(e) => setPrompt(e.target.value)}
            placeholder="Enter prompt for the model..."
          />
          
          <SectionTitle>Initial Seed</SectionTitle>
          <Input
            type="text"
            value={initial}
            onChange={(e) => setInitial(e.target.value)}
            placeholder="Starting text for generation..."
            style={{ width: '100%' }}
          />
        </PromptSection>
        
        <OutputSection>
          <OutputPane border highlight={isComplete}>
            <OutputLabel color="#4ec9b0">
              Constrained (Type-Safe)
              <StatusBadge status={status === 'generating' ? 'generating' : isComplete ? 'complete' : status}>
                {isComplete ? 'Complete' : status === 'generating' ? 'Generating...' : stoppedReason || 'Ready'}
              </StatusBadge>
            </OutputLabel>
            <OutputBox>{constrainedOutput}</OutputBox>
            {constrainedTokens.length > 0 && (
              <TokenStream>
                Last tokens: {formatTokenStream(constrainedTokens)}
              </TokenStream>
            )}
          </OutputPane>
          
          <OutputPane>
            <OutputLabel color="#dcdcaa">
              Unconstrained (Raw Model)
              <StatusBadge status={status === 'generating' ? 'generating' : status === 'idle' ? 'idle' : 'complete'}>
                {status === 'idle' ? 'Ready' : status === 'generating' ? 'Generating...' : 'Complete'}
              </StatusBadge>
            </OutputLabel>
            <OutputBox>{unconstrainedOutput}</OutputBox>
            {unconstrainedTokens.length > 0 && (
              <TokenStream>
                Last tokens: {formatTokenStream(unconstrainedTokens)}
              </TokenStream>
            )}
          </OutputPane>
        </OutputSection>
      </ContentArea>
    </PanelContainer>
  );
}

export default GenerationPanel;
