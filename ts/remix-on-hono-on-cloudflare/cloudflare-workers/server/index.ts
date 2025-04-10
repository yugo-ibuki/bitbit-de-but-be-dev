// server/index.ts
import { Hono } from 'hono'

const app = new Hono<{
  Bindings: {
    MY_VAR: string
  }
  Variables: {
    MY_VAR_IN_VARIABLES: string
  }
}>()

app.use(async (c, next) => {
  c.set('MY_VAR_IN_VARIABLES', 'My variable set in c.set')
  await next()
  c.header('X-Powered-By', 'React Router and Hono')
})

app.get('/api', (c) => {
  console.log(c.env.MY_VAR)
  return c.json({
    message: 'Hello',
    var: c.env.MY_VAR,
  })
})

export default app
