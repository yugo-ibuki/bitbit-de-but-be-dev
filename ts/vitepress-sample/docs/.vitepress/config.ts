import { defineConfig } from 'vitepress';

// refer https://vitepress.dev/reference/site-config for details
export default defineConfig({
  lang: 'en-US',
  title: 'VitePress Sample',
  description: 'Vite & Vue powered static site generator.',

  themeConfig: {
    nav: [
      { text: 'Home', link: '/' },
      { text: 'Guide', link: '/guide/' },
      {
        text: 'Layouts',
        items: [
          { text: 'Basic Page', link: '/layouts/basic' },
          { text: 'API Reference', link: '/layouts/api' },
          { text: 'Feature Page', link: '/layouts/feature' },
          { text: 'Team Page', link: '/layouts/team' },
        ],
      },
      { text: 'About', link: '/about' },
    ],

    sidebar: {
      '/guide/': [
        {
          text: 'Introduction',
          items: [
            { text: 'Getting Started', link: '/guide/' },
            { text: 'Configuration', link: '/guide/configuration' },
            { text: 'Deployment', link: '/guide/deployment' },
          ],
        },
      ],
      '/layouts/': [
        {
          text: 'Layout Examples',
          items: [
            { text: 'Basic Page', link: '/layouts/basic' },
            { text: 'API Reference', link: '/layouts/api' },
            { text: 'Feature Page', link: '/layouts/feature' },
            { text: 'Team Page', link: '/layouts/team' },
          ],
        },
      ],
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/vuejs/vitepress' },
    ],

    footer: {
      message: 'Released under the MIT License.',
      copyright: 'Copyright © 2023-present',
    },
  },
});