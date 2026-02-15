#!/bin/bash

# P7 Visualization Platform Launcher
# This script starts both the backend and frontend servers

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=================================${NC}"
echo -e "${BLUE}P7 Visualization Platform${NC}"
echo -e "${BLUE}=================================${NC}"
echo ""

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

# Check if P7 is installed
echo -e "${YELLOW}Checking P7 installation...${NC}"
if python -c "import proposition_7" 2>/dev/null; then
    echo -e "${GREEN}✓ P7 library is installed${NC}"
else
    echo -e "${RED}✗ P7 library not found${NC}"
    echo "Please install P7 first:"
    echo "  cd /path/to/p7/python && pip install -e ."
    exit 1
fi

# Check for required Python packages
echo -e "${YELLOW}Checking Python dependencies...${NC}"
python -c "import flask, flask_cors" 2>/dev/null || {
    echo -e "${RED}✗ Flask dependencies missing${NC}"
    echo "Install with: pip install flask flask-cors"
    exit 1
}
echo -e "${GREEN}✓ Flask dependencies OK${NC}"

# Check for Node.js
echo -e "${YELLOW}Checking Node.js...${NC}"
if command -v node &> /dev/null; then
    NODE_VERSION=$(node --version)
    echo -e "${GREEN}✓ Node.js ${NODE_VERSION} found${NC}"
else
    echo -e "${RED}✗ Node.js not found${NC}"
    echo "Please install Node.js 16+ to run the frontend"
    exit 1
fi

# Install frontend dependencies if needed
if [ ! -d "frontend/node_modules" ]; then
    echo -e "${YELLOW}Installing frontend dependencies...${NC}"
    cd frontend
    if [ -f package-lock.json ]; then
        npm ci --legacy-peer-deps || npm install --legacy-peer-deps
    else
        npm install --legacy-peer-deps
        npm install --package-lock-only --legacy-peer-deps || true
    fi
    cd ..
fi

echo ""
echo -e "${GREEN}Starting servers...${NC}"
echo ""

# Function to cleanup on exit
cleanup() {
    echo ""
    echo -e "${YELLOW}Shutting down servers...${NC}"
    kill $BACKEND_PID $FRONTEND_PID 2>/dev/null || true
    wait $BACKEND_PID $FRONTEND_PID 2>/dev/null || true
    echo -e "${GREEN}Servers stopped${NC}"
    exit 0
}

trap cleanup INT TERM

# Start backend
echo -e "${BLUE}Starting backend server on http://localhost:5001${NC}"
cd backend
python app.py &
BACKEND_PID=$!
cd ..

# Wait for backend to start
sleep 2

# Check if backend started successfully
if ! kill -0 $BACKEND_PID 2>/dev/null; then
    echo -e "${RED}✗ Backend failed to start${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Backend running (PID: $BACKEND_PID)${NC}"
echo ""

# Start frontend
echo -e "${BLUE}Starting frontend server on http://localhost:3000${NC}"
cd frontend
npm start &
FRONTEND_PID=$!
cd ..

echo -e "${GREEN}✓ Frontend starting...${NC}"
echo ""

echo -e "${GREEN}=================================${NC}"
echo -e "${GREEN}Platform is running!${NC}"
echo -e "${GREEN}=================================${NC}"
echo ""
echo -e "Backend API:  ${BLUE}http://localhost:5001${NC}"
echo -e "Frontend UI:  ${BLUE}http://localhost:3000${NC}"
echo ""
echo -e "Press ${YELLOW}Ctrl+C${NC} to stop both servers"
echo ""

# Wait for both processes
wait $BACKEND_PID $FRONTEND_PID
