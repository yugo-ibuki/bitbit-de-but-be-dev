# Project Structure

## Root Directory Layout
```
intersection-observer/
├── index.html              # Main HTML entry point
├── styles.css              # Global styles and component styling
├── main.ts                 # Main TypeScript implementation
├── dist/                   # Compiled JavaScript output
│   ├── main.js            # Compiled from main.ts
│   └── main.js.map        # Source map for debugging
├── package.json           # Project configuration and scripts
├── tsconfig.json          # TypeScript compiler configuration
└── node_modules/          # Dependencies (generated)
```

## File Organization Patterns

### HTML Structure
- Single `index.html` file with semantic HTML5 structure
- Uses `data-id` attributes for element identification
- CSS classes for styling and behavior (`observed-section`, `fade-in`, `lazy-load`)

### TypeScript Architecture
- **Single main class**: `IntersectionObserverPlayground`
- **Interface definitions**: `ObservedElement` for type safety
- **Private methods**: Internal implementation details
- **Public API**: Methods for external interaction (`addElement`, `removeElement`, etc.)

### CSS Organization
- **Reset styles**: Universal box-sizing and margin/padding reset
- **Component-based**: Styles grouped by functionality
- **State classes**: `.visible` for intersection states
- **Responsive design**: Mobile-first approach with media queries

## Naming Conventions

### TypeScript
- **Classes**: PascalCase (`IntersectionObserverPlayground`)
- **Interfaces**: PascalCase (`ObservedElement`)
- **Methods**: camelCase (`handleIntersection`)
- **Properties**: camelCase (`observedElements`)

### HTML/CSS
- **CSS classes**: kebab-case (`observed-section`, `fade-in`)
- **IDs**: kebab-case (`status-panel`, `status-list`)
- **Data attributes**: kebab-case (`data-id`)

### Files
- **TypeScript**: `.ts` extension
- **Compiled output**: `.js` with corresponding `.js.map`
- **Styles**: Single `styles.css` file

## Key Architectural Patterns
- **Observer pattern**: Core Intersection Observer implementation
- **Event-driven**: Custom events for visibility changes
- **Map-based storage**: Element tracking with `Map<string, ObservedElement>`
- **DOM manipulation**: Direct DOM API usage, no virtual DOM