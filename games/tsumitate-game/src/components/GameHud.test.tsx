import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { describe, expect, it, vi } from 'vitest'
import { GameHud } from './GameHud'

describe('GameHud', () => {
  it('shows state and sends player actions', async () => {
    const user = userEvent.setup()
    const onDestroy = vi.fn()
    const onReset = vi.fn()

    render(
      <GameHud
        challengeKey="2026-07-16"
        currentPiece={{
          kind: 'box',
          size: 'large',
          scale: 1.25,
          color: '#fff',
          rotation: [0, 0, 0],
        }}
        nextPieces={[
          { kind: 'sphere', size: 'small', scale: 0.78, color: '#fff', rotation: [0, 0, 0] },
          { kind: 'cylinder', size: 'medium', scale: 1, color: '#fff', rotation: [0, 0, 0] },
          { kind: 'box', size: 'small', scale: 0.78, color: '#fff', rotation: [0, 0, 0] },
        ]}
        usedCount={12}
        max={100}
        currentHeight={4.2}
        bestHeight={5.8}
        notice="もっと積めます"
        onDestroy={onDestroy}
        onReset={onReset}
      />,
    )

    expect(
      screen.queryByRole('heading', { name: '積み立てクラッシュ' }),
    ).not.toBeInTheDocument()
    expect(screen.queryByText('STACK · WATCH · CRASH')).not.toBeInTheDocument()
    expect(screen.getByText('4.20')).toBeInTheDocument()
    expect(screen.getByText('5.80 m')).toBeInTheDocument()
    expect(screen.getByText("TODAY'S DECK")).toBeInTheDocument()
    expect(screen.getByText('2026.07.16')).toBeInTheDocument()
    expect(screen.getByLabelText('現在の物体')).toHaveTextContent('ボックス')
    expect(screen.getByLabelText('現在の物体')).toHaveTextContent('L')
    expect(screen.getByLabelText('次の3個')).toHaveTextContent('ボール')
    expect(screen.getByText('12 / 100')).toBeInTheDocument()
    expect(screen.getByText('もっと積めます')).toBeInTheDocument()

    await user.click(screen.getByRole('button', { name: '破壊する' }))
    await user.click(screen.getByRole('button', { name: 'もう一度積む' }))

    expect(onDestroy).toHaveBeenCalledOnce()
    expect(onReset).toHaveBeenCalledOnce()
  })
})
