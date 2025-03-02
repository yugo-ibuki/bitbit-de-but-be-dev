# Deployment

VitePress generates static HTML files that can be deployed to any static hosting service.

## Building for Production

To build your VitePress site for production, run:

```bash
npm run docs:build
```

This will generate a `.vitepress/dist` directory with all the static assets.

## Deploying to GitHub Pages

1. Set the correct `base` in your config:

```js
export default {
  base: '/your-repo-name/'
}
```

2. Create a deployment script:

```bash
#!/usr/bin/env sh

# abort on errors
set -e

# build
npm run docs:build

# navigate to the build output directory
cd docs/.vitepress/dist

# if you're deploying to a custom domain
# echo 'www.example.com' > CNAME

git init
git add -A
git commit -m 'deploy'

# if you're deploying to https://<USERNAME>.github.io/<REPO>
git push -f git@github.com:<USERNAME>/<REPO>.git main:gh-pages

cd -
```

## Deploying to Netlify

1. Create a `netlify.toml` file in the root of your project:

```toml
[build]
  command = "npm run docs:build"
  publish = "docs/.vitepress/dist"
```

2. Push your code to a Git repository and connect it to Netlify.

## Deploying to Vercel

1. Create a `vercel.json` file in the root of your project:

```json
{
  "buildCommand": "npm run docs:build",
  "outputDirectory": "docs/.vitepress/dist",
  "framework": "vitepress"
}
```

2. Push your code to a Git repository and connect it to Vercel.