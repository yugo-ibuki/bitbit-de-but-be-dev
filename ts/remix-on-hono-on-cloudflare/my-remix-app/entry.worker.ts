import { Hono } from 'hono'
import { handle } from '@remix-run/server-runtime'
import { serveStatic } from '@hono/node-server/serve-static'
import * as build from '@remix-run/dev/server-build'

const app = new Hono()

// 静的ファイルの処理
app.use('/build/*', serveStatic({ root: './' }))

// Remixのハンドラー
app.all('*', async (c) => {
  const request = c.req.raw
  const loadContext = {
    env: c.env,
  }

  const response = await handle(request, loadContext, build)
  return response
})

export default app
