# Configuration

VitePress comes with a powerful configuration system that allows you to customize almost every aspect of your site.

## Site Configuration

The site configuration is defined in the `.vitepress/config.js` file. Here's an example of a basic configuration:

```js
export default {
  // Site-level options
  title: 'My Documentation',
  description: 'A VitePress site',
  lang: 'en-US',
  base: '/',
  head: [
    ['link', { rel: 'icon', href: '/favicon.ico' }]
  ],
  
  // Theme-related options
  themeConfig: {
    logo: '/logo.svg',
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Guide', link: '/guide/' }
    ],
    sidebar: {
      '/guide/': [
        {
          text: 'Guide',
          items: [
            { text: 'Introduction', link: '/guide/' },
            { text: 'Configuration', link: '/guide/configuration' }
          ]
        }
      ]
    },
    footer: {
      message: 'Released under the MIT License.',
      copyright: 'Copyright © 2023-present'
    }
  }
}
```

## Customizing the Theme

VitePress allows you to customize the default theme or create your own theme:

```js
// .vitepress/theme/index.js
import DefaultTheme from 'vitepress/theme'
import MyLayout from './MyLayout.vue'
import './custom.css'

export default {
  ...DefaultTheme,
  Layout: MyLayout,
  enhanceApp({ app, router, siteData }) {
    // Register global components or add Vue plugins
  }
}
```

## Markdown Configuration

You can customize the Markdown parser behavior:

```js
export default {
  markdown: {
    lineNumbers: true,
    // Configure Markdown-it options
    config: (md) => {
      // Use Markdown-it plugins
      md.use(require('markdown-it-plugin'))
    }
  }
}
```