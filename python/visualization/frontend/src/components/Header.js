import React from 'react';
import styled from 'styled-components';

const HeaderContainer = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 10px 20px;
  background-color: #2d2d2d;
  border-bottom: 1px solid #333;
`;

const Title = styled.h1`
  margin: 0;
  font-size: 18px;
  color: #4ec9b0;
`;

const Subtitle = styled.span`
  font-size: 12px;
  color: #808080;
  margin-left: 10px;
`;

const Controls = styled.div`
  display: flex;
  gap: 10px;
  align-items: center;
`;

const Select = styled.select`
  padding: 5px 10px;
  background-color: #3c3c3c;
  border: 1px solid #555;
  color: #d4d4d4;
  border-radius: 3px;
  font-size: 12px;
`;

const StatusBadge = styled.span`
  padding: 4px 10px;
  border-radius: 3px;
  font-size: 12px;
  font-weight: 600;
  background-color: ${props => props.valid ? '#1e4d2b' : '#5c1e1e'};
  color: ${props => props.valid ? '#4ec9b0' : '#f44747'};
`;

function Header({ grammarStatus, availableGrammars, onLoadExample, isLoading }) {
  return (
    <HeaderContainer>
      <div>
        <Title>P7 Visualization Platform</Title>
        <Subtitle>Grammar Editor & Constrained Generation Workflows</Subtitle>
      </div>
      
      <Controls>
        <span style={{ fontSize: '12px', color: '#808080' }}>Load Example:</span>
        <Select 
          onChange={(e) => e.target.value && onLoadExample(e.target.value)}
          value=""
          disabled={isLoading}
        >
          <option value="">{isLoading ? 'Loading...' : 'Select...'}</option>
          {availableGrammars.map(g => (
            <option key={g.name} value={g.name}>
              {g.display_name}
            </option>
          ))}
        </Select>
        
        <StatusBadge valid={grammarStatus.valid}>
          {grammarStatus.valid ? 'Grammar Valid' : 'Grammar Invalid'}
        </StatusBadge>
      </Controls>
    </HeaderContainer>
  );
}

export default Header;
