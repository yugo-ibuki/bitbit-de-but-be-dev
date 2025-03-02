---
outline: deep
---

# API Reference Layout

This page demonstrates how to structure an API reference documentation page.

## Introduction

The API reference section provides detailed information about functions, classes, and other elements of your library or framework. This layout is optimized for technical documentation with code examples.

## Installation

```bash
npm install your-package
```

## Usage

```js
import { createApp } from 'your-package'

const app = createApp()
app.mount('#app')
```

## API

### createApp()

Creates an application instance.

**Type:**

```ts
function createApp(options?: AppOptions): App
```

**Parameters:**

- `options` - Optional configuration object

**Returns:**

- `App` - The application instance

**Example:**

```js
import { createApp } from 'your-package'

const app = createApp({
  debug: true,
  plugins: []
})
```

### App

The main application object.

#### app.mount()

Mounts the application to a DOM element.

**Type:**

```ts
interface App {
  mount(selector: string | Element): void
}
```

**Parameters:**

- `selector` - CSS selector or DOM element

**Example:**

```js
app.mount('#app')
```

#### app.unmount()

Unmounts the application.

**Type:**

```ts
interface App {
  unmount(): void
}
```

**Example:**

```js
app.unmount()
```

### Hooks

#### onMount()

Called when the component is mounted.

**Type:**

```ts
function onMount(callback: () => void): void
```

**Parameters:**

- `callback` - Function to execute on mount

**Example:**

```js
import { onMount } from 'your-package'

onMount(() => {
  console.log('Component mounted')
})
```

## Type Definitions

```ts
interface AppOptions {
  debug?: boolean
  plugins?: Plugin[]
}

interface Plugin {
  name: string
  install: (app: App) => void
}

type App = {
  mount: (selector: string | Element) => void
  unmount: () => void
  use: (plugin: Plugin) => App
}
```

## Error Codes

| Code | Description |
|------|-------------|
| E001 | Invalid configuration |
| E002 | Missing required parameter |
| E003 | Component not found |
| E004 | Invalid plugin format |