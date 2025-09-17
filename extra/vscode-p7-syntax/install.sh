#!/bin/bash

# P7 Syntax Extension Installation Script

echo "Building P7 Syntax Extension..."

# Check if vsce is installed locally
if [ ! -f "node_modules/.bin/vsce" ]; then
    echo "Installing vsce locally..."
    npm install --save-dev vsce
fi

# Compile the extension
echo "Compiling TypeScript..."
npm run compile

# Package the extension
echo "Packaging extension..."
npx vsce package

echo "Extension packaged successfully!"
echo "To install manually:"
echo "1. Open VS Code"
echo "2. Go to Extensions (Ctrl+Shift+X)"
echo "3. Click the '...' menu and select 'Install from VSIX...'"
echo "4. Select the generated .vsix file"
echo ""
echo "Or install from command line:"
echo "code --install-extension $(ls *.vsix | head -1)"
