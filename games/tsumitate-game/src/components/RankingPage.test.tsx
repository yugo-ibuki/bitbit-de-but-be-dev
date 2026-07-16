import { cleanup, render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { RankingPage } from './RankingPage'

describe('RankingPage', () => {
  afterEach(cleanup)

  it('shows a clear empty state for a new challenge day', async () => {
    const user = userEvent.setup()
    const onBack = vi.fn()
    render(
      <RankingPage
        challengeKey="2026-07-17"
        entries={[]}
        onBack={onBack}
      />,
    )

    expect(
      screen.getByRole('heading', { name: 'デイリーランキング' }),
    ).toBeInTheDocument()
    expect(screen.getByText('この端末の記録')).toBeInTheDocument()
    expect(screen.getByText('まだ記録がありません')).toBeInTheDocument()
    expect(screen.getByLabelText('0.00 m')).toBeInTheDocument()
    expect(screen.getByLabelText('0 回')).toBeInTheDocument()

    await user.click(screen.getByRole('button', { name: 'ゲームに戻る' }))
    expect(onBack).toHaveBeenCalledOnce()
  })

  it('renders ranked attempts and summary values', () => {
    render(
      <RankingPage
        challengeKey="2026-07-17"
        entries={[
          {
            id: 'first',
            height: 5.42,
            usedCount: 18,
            completedAt: Date.UTC(2026, 6, 17, 1, 23),
          },
          {
            id: 'second',
            height: 4.8,
            usedCount: 22,
            completedAt: Date.UTC(2026, 6, 17, 2, 5),
          },
        ]}
        onBack={() => undefined}
      />,
    )

    expect(screen.getByText('2026.07.17')).toBeInTheDocument()
    expect(screen.getByText('5.42 m')).toBeInTheDocument()
    expect(screen.getByLabelText('2 回')).toBeInTheDocument()
    expect(screen.getByText('18 個')).toBeInTheDocument()
    expect(screen.getByText('22 個')).toBeInTheDocument()
    expect(screen.getByLabelText('1位')).toHaveTextContent('5.42 m')
    expect(screen.getByLabelText('2位')).toHaveTextContent('4.80 m')
  })
})
