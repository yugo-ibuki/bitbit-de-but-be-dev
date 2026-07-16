import { cleanup, render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { App } from './App'
import { getJapanChallengeKey } from './game/challengeDeck'
import { loadRankingEntries } from './game/rankingStorage'
import type { Vec3 } from './game/types'

interface MockSceneProps {
  onPlace: (point: Vec3) => void
  onHeightChange: (height: number, eligibleForRecord: boolean) => void
}

vi.mock('./components/GameScene', () => ({
  GameScene: ({ onPlace, onHeightChange }: MockSceneProps) => (
    <button
      type="button"
      onClick={() => {
        onPlace([0, 0, 0])
        onHeightChange(4.2, true)
      }}
    >
      テスト物体を置く
    </button>
  ),
}))

describe('App ranking flow', () => {
  beforeEach(() => {
    localStorage.clear()
    window.history.replaceState({}, '', '/')
  })

  afterEach(() => {
    cleanup()
    window.history.replaceState({}, '', '/')
  })

  it('records one result per attempt and shows it on the ranking page', async () => {
    const user = userEvent.setup()
    render(<App />)

    await user.click(screen.getByRole('button', { name: 'テスト物体を置く' }))
    await user.click(screen.getByRole('button', { name: '破壊する' }))
    await user.click(screen.getByRole('button', { name: 'ランキングを見る' }))

    expect(
      loadRankingEntries(getJapanChallengeKey(), localStorage),
    ).toHaveLength(1)
    expect(screen.getByLabelText('1 回')).toBeInTheDocument()
    expect(screen.getByLabelText('1位')).toHaveTextContent('4.20 m')
    expect(screen.getByLabelText('1位')).toHaveTextContent('1 個')

    await user.click(screen.getByRole('button', { name: 'ゲームに戻る' }))
    await user.click(screen.getByRole('button', { name: '破壊する' }))
    expect(
      loadRankingEntries(getJapanChallengeKey(), localStorage),
    ).toHaveLength(1)

    await user.click(screen.getByRole('button', { name: 'もう一度積む' }))
    await user.click(screen.getByRole('button', { name: 'テスト物体を置く' }))
    await user.click(screen.getByRole('button', { name: '破壊する' }))
    expect(
      loadRankingEntries(getJapanChallengeKey(), localStorage),
    ).toHaveLength(2)
  })
})
