# Technology Stack

## Core Technologies
- **TypeScript 5.0+** - Primary language with strict type checking
- **Vanilla HTML/CSS/JS** - No frameworks, pure web APIs
- **Intersection Observer API** - Core browser API being demonstrated

## Build System
- **TypeScript Compiler (tsc)** - Direct compilation, no bundlers
- **Output**: Compiled JS goes to `dist/` directory with source maps

## TypeScript Configuration
- **Target**: ES2020
- **Strict mode**: Enabled with comprehensive type checking
- **Source maps**: Generated for debugging
- **Declaration files**: Generated for type definitions

## Development Dependencies
- `typescript ^5.0.0`
- `@types/node ^18.0.0`

## Common Commands

### Build & Development
```bash
# Compile TypeScript once
npm run build

# Watch mode for development
npm run watch

# Development with live server
npm run dev

# Clean compiled files
npm run clean
```

### Manual Commands
```bash
# Direct TypeScript compilation
tsc

# Watch mode
tsc --watch
```

## Code Quality Standards
- Strict TypeScript configuration with no implicit any
- Comprehensive null/undefined checking
- Exact optional property types
- No unchecked indexed access