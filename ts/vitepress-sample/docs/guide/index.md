# Getting Started

## Introduction

VitePress is a static site generator designed for creating documentation websites. It's built on top of Vite and Vue, offering a fast development experience and optimized production builds.

## Installation

You can create a new VitePress site using npm:

```bash
npm init vitepress
```

Or with yarn:

```bash
yarn create vitepress
```

## Project Structure

A basic VitePress project structure looks like this:

```
.
├── docs
│   ├── .vitepress
│   │   └── config.js
│   ├── index.md
│   └── guide
│       └── index.md
└── package.json
```

## Basic Configuration

The `.vitepress/config.js` file is where you configure your site:

```js
export default {
  title: 'My Documentation',
  description: 'A VitePress site',
  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Guide', link: '/guide/' }
    ],
    sidebar: {
      '/guide/': [
        { text: 'Introduction', link: '/guide/' }
      ]
    }
  }
}
```

## Writing Content

VitePress uses Markdown for content. You can use all standard Markdown syntax, plus Vue components directly in your Markdown files.

```md
# Hello VitePress

This is a markdown file with a Vue component:

<MyComponent />
```