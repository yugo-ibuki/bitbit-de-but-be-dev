import { render, screen } from '@testing-library/react'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { GameErrorBoundary } from './GameErrorBoundary'

function BrokenScene(): never {
  throw new Error('WebGL unavailable')
}

describe('GameErrorBoundary', () => {
  afterEach(() => vi.restoreAllMocks())

  it('shows a useful recovery message', () => {
    vi.spyOn(console, 'error').mockImplementation(() => undefined)
    render(
      <GameErrorBoundary>
        <BrokenScene />
      </GameErrorBoundary>,
    )
    expect(screen.getByRole('alert')).toHaveTextContent('ゲームを起動できませんでした')
  })
})
