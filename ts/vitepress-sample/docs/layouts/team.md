---
layout: page
---

<script setup>
import { VPTeamMembers } from 'vitepress/theme'

const members = [
  {
    avatar: 'https://www.github.com/yyx990803.png',
    name: 'Evan You',
    title: 'Creator',
    links: [
      { icon: 'github', link: 'https://github.com/yyx990803' },
      { icon: 'twitter', link: 'https://twitter.com/youyuxi' }
    ]
  },
  {
    avatar: 'https://www.github.com/kiaking.png',
    name: 'Kia King Ishii',
    title: 'Developer',
    links: [
      { icon: 'github', link: 'https://github.com/kiaking' },
      { icon: 'twitter', link: 'https://twitter.com/KiaKing85' }
    ]
  },
  {
    avatar: 'https://www.github.com/bencodezen.png',
    name: 'Ben Hong',
    title: 'Developer',
    links: [
      { icon: 'github', link: 'https://github.com/bencodezen' },
      { icon: 'twitter', link: 'https://twitter.com/bencodezen' }
    ]
  },
  {
    avatar: 'https://www.github.com/antfu.png',
    name: 'Anthony Fu',
    title: 'Developer',
    links: [
      { icon: 'github', link: 'https://github.com/antfu' },
      { icon: 'twitter', link: 'https://twitter.com/antfu7' }
    ]
  },
  {
    avatar: 'https://www.github.com/patak-dev.png',
    name: 'Patak',
    title: 'Developer',
    links: [
      { icon: 'github', link: 'https://github.com/patak-dev' },
      { icon: 'twitter', link: 'https://twitter.com/patak_dev' }
    ]
  },
  {
    avatar: 'https://www.github.com/sodatea.png',
    name: 'Haoqun Jiang',
    title: 'Developer',
    links: [
      { icon: 'github', link: 'https://github.com/sodatea' },
      { icon: 'twitter', link: 'https://twitter.com/haoqunjiang' }
    ]
  }
]

const partners = [
  {
    avatar: 'https://avatars.githubusercontent.com/u/7147592',
    name: 'Pine Wu',
    title: 'VitePress Contributor',
    links: [
      { icon: 'github', link: 'https://github.com/octref' }
    ]
  },
  {
    avatar: 'https://avatars.githubusercontent.com/u/499550',
    name: 'Evan You',
    title: 'Vue.js Creator',
    links: [
      { icon: 'github', link: 'https://github.com/yyx990803' }
    ]
  },
  {
    avatar: 'https://avatars.githubusercontent.com/u/11247099',
    name: 'Anthony Fu',
    title: 'VitePress Contributor',
    links: [
      { icon: 'github', link: 'https://github.com/antfu' }
    ]
  },
  {
    avatar: 'https://avatars.githubusercontent.com/u/664177',
    name: 'Sebastien Chopin',
    title: 'Nuxt.js Creator',
    links: [
      { icon: 'github', link: 'https://github.com/Atinux' }
    ]
  }
]
</script>

# Our Team

This page demonstrates how to create a team page with member profiles using VitePress components.

## Core Team

The core team members who are actively maintaining and developing VitePress.

<VPTeamMembers size="medium" :members="members" />

## Partners & Sponsors

Organizations and individuals who support our project.

<VPTeamMembers size="small" :members="partners" />

## Join Our Team

We're always looking for talented individuals to join our team. If you're passionate about documentation and web development, we'd love to hear from you!

### Open Positions

- **Technical Writer** - Help us create clear and concise documentation.
- **Frontend Developer** - Improve the VitePress user interface and experience.
- **DevOps Engineer** - Optimize our build and deployment processes.

### How to Apply

1. Fork our repository on GitHub
2. Make a meaningful contribution
3. Reach out to one of our team members

## Community Contributors

We'd like to thank all the amazing people who have contributed to VitePress. Your efforts help make this project better for everyone.

<div class="contributors-wrapper">
  <img src="https://contrib.rocks/image?repo=vuejs/vitepress" alt="VitePress Contributors" />
</div>

<style>
.contributors-wrapper {
  margin: 2rem 0;
  text-align: center;
}

.contributors-wrapper img {
  max-width: 100%;
  height: auto;
  border-radius: 8px;
}
</style>