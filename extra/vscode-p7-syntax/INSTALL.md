## P7 Syntax Extension Installation & Usage

### Installation Options

#### Option 1: Install from VSIX file
```bash
code --install-extension p7-syntax-0.1.0.vsix
```

#### Option 2: Manual Installation
1. Open VS Code
2. Go to Extensions (Ctrl+Shift+X)
3. Click the "..." menu and select "Install from VSIX..."
4. Select the `p7-syntax-0.1.0.vsix` file

#### Option 3: Use the install script
```bash
./install.sh
```

### Testing the Extension

1. Open the `example.spec` file to see syntax highlighting in action
2. Create new `.spec` or `.p7` files to test with your own grammar
3. The extension should automatically activate when opening files with these extensions

### Syntax Highlighting Features

- **Grammar Productions**: `Variable(var) ::= Identifier[x]`
- **Typing Rules**: Multi-line inference rules with premises and conclusions
- **Type Expressions**: Unicode type variables, arrows, logical operators
- **Context Operations**: Context lookups and transformations
- **Comments**: Line comments starting with `//`

### File Extensions Supported

- `.spec` - Primary file extension for P7 specifications
- `.p7` - Alternative file extension

### Development

To modify the extension:
1. Edit `syntaxes/p7-spec.tmLanguage.json` for syntax highlighting rules
2. Run `npm run compile` to rebuild
3. Run `npx vsce package` to create a new VSIX file
4. Reload VS Code window to test changes

### Uninstalling

```bash
code --uninstall-extension p7-project.p7-syntax
```
