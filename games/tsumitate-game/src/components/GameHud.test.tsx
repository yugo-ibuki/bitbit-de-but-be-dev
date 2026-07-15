import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { describe, expect, it, vi } from 'vitest'
import { GameHud } from './GameHud'

describe('GameHud', () => {
  it('shows state and sends player actions', async () => {
    const user = userEvent.setup()
    const onSelect = vi.fn()
    const onDestroy = vi.fn()
    const onReset = vi.fn()

    render(
      <GameHud
        selectedKind="box"
        count={12}
        max={100}
        notice="もっと積めます"
        onSelect={onSelect}
        onDestroy={onDestroy}
        onReset={onReset}
      />,
    )

    expect(
      screen.queryByRole('heading', { name: '積み立てクラッシュ' }),
    ).not.toBeInTheDocument()
    expect(screen.queryByText('STACK · WATCH · CRASH')).not.toBeInTheDocument()
    expect(screen.getByText('12 / 100')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'ボックス' })).toHaveAttribute(
      'aria-pressed',
      'true',
    )
    expect(screen.getByText('もっと積めます')).toBeInTheDocument()

    await user.click(screen.getByRole('button', { name: 'ボール' }))
    await user.click(screen.getByRole('button', { name: '破壊する' }))
    await user.click(screen.getByRole('button', { name: 'もう一度積む' }))

    expect(onSelect).toHaveBeenCalledWith('sphere')
    expect(onDestroy).toHaveBeenCalledOnce()
    expect(onReset).toHaveBeenCalledOnce()
  })
})
