import { act, cleanup, render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, describe, expect, it } from 'vitest'
import { getRoute, useAppRoute } from './useAppRoute'

function RouteHarness() {
  const { route, navigate } = useAppRoute()
  return (
    <button
      type="button"
      onClick={() => navigate(route === 'game' ? 'ranking' : 'game')}
    >
      {route}
    </button>
  )
}

describe('app route', () => {
  afterEach(() => {
    cleanup()
    window.history.replaceState({}, '', '/')
  })

  it('maps only the ranking pathname to the ranking view', () => {
    expect(getRoute('/')).toBe('game')
    expect(getRoute('/something-else')).toBe('game')
    expect(getRoute('/ranking')).toBe('ranking')
  })

  it('pushes routes and responds to browser navigation', async () => {
    window.history.replaceState({}, '', '/')
    const user = userEvent.setup()
    render(<RouteHarness />)

    expect(screen.getByRole('button')).toHaveTextContent('game')
    await user.click(screen.getByRole('button'))
    expect(window.location.pathname).toBe('/ranking')
    expect(screen.getByRole('button')).toHaveTextContent('ranking')

    act(() => {
      window.history.pushState({}, '', '/')
      window.dispatchEvent(new PopStateEvent('popstate'))
    })
    expect(screen.getByRole('button')).toHaveTextContent('game')
  })

  it('reads a direct ranking pathname on first render', () => {
    window.history.replaceState({}, '', '/ranking')
    render(<RouteHarness />)
    expect(screen.getByRole('button')).toHaveTextContent('ranking')
  })
})
