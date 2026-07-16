import { useCallback, useEffect, useMemo, useReducer, useRef, useState } from 'react'
import { GameErrorBoundary } from './components/GameErrorBoundary'
import { GameHud } from './components/GameHud'
import { GameScene } from './components/GameScene'
import { RankingPage } from './components/RankingPage'
import {
  getBrowserHeightStorage,
  loadBestHeight,
  saveBestHeight,
} from './game/bestHeightStorage'
import {
  createDailyChallengeDeck,
  getJapanChallengeKey,
} from './game/challengeDeck'
import { MAX_OBJECTS, createStackingItem } from './game/gameRules'
import { gameReducer, initialGameState } from './game/gameState'
import {
  getBrowserRankingStorage,
  loadRankingEntries,
  saveRankingEntry,
} from './game/rankingStorage'
import type { Vec3 } from './game/types'
import { useAppRoute } from './game/useAppRoute'
import { useContainmentRecovery } from './game/useContainmentRecovery'

export function App() {
  const { route, navigate } = useAppRoute()
  const challengeKey = useMemo(() => getJapanChallengeKey(), [])
  const deck = useMemo(
    () => createDailyChallengeDeck(challengeKey),
    [challengeKey],
  )
  const [state, dispatch] = useReducer(gameReducer, initialGameState)
  const [currentHeight, setCurrentHeight] = useState(0)
  const [bestHeight, setBestHeight] = useState(() =>
    loadBestHeight(challengeKey, getBrowserHeightStorage()),
  )
  const rankingStorage = useMemo(() => getBrowserRankingStorage(), [])
  const [rankingEntries, setRankingEntries] = useState(() =>
    loadRankingEntries(challengeKey, rankingStorage),
  )
  const nextId = useRef(1)
  const attemptRecorded = useRef(false)

  const placeObject = useCallback(
    (point: Vec3) => {
      const piece = deck[state.usedCount]
      if (!piece) {
        dispatch({ type: 'deck-complete' })
        return
      }
      const item = createStackingItem(
        piece,
        point,
        'object-' + nextId.current++,
      )
      dispatch({ type: 'place', item })
    },
    [deck, state.usedCount],
  )

  useEffect(() => {
    if (!state.notice) return
    const timer = window.setTimeout(() => dispatch({ type: 'clear-notice' }), 2600)
    return () => window.clearTimeout(timer)
  }, [state.notice])

  const handleHeightChange = useCallback(
    (height: number, eligibleForRecord: boolean) => {
      if (!state.containmentEnabled) return
      setCurrentHeight(height)
      if (!eligibleForRecord) return
      setBestHeight((best) => {
        if (height <= best) return best
        saveBestHeight(challengeKey, height, getBrowserHeightStorage())
        return height
      })
    },
    [challengeKey, state.containmentEnabled],
  )

  const handleReset = useCallback(() => {
    attemptRecorded.current = false
    setCurrentHeight(0)
    dispatch({ type: 'reset' })
  }, [])

  const handleDestroy = useCallback(() => {
    if (
      !attemptRecorded.current &&
      state.items.length > 0 &&
      currentHeight > 0
    ) {
      const completedAt = Date.now()
      const entry = {
        id: `attempt-${completedAt}-${Math.random().toString(36).slice(2, 8)}`,
        height: currentHeight,
        usedCount: state.usedCount,
        completedAt,
      }
      setRankingEntries(
        saveRankingEntry(challengeKey, entry, rankingStorage),
      )
      attemptRecorded.current = true
    }
    dispatch({ type: 'destroy' })
  }, [challengeKey, currentHeight, rankingStorage, state.items.length, state.usedCount])

  const restoreContainment = useCallback(() => {
    dispatch({ type: 'restore-containment' })
  }, [])

  useContainmentRecovery(
    state.containmentEnabled,
    state.destructionVersion,
    restoreContainment,
  )

  return (
    <main className="app-shell">
      <GameErrorBoundary>
        <GameScene
          items={state.items}
          destructionVersion={state.destructionVersion}
          containmentEnabled={state.containmentEnabled}
          currentHeight={currentHeight}
          currentPiece={deck[state.usedCount] ?? null}
          onPlace={placeObject}
          onRemove={(id) => dispatch({ type: 'remove', id })}
          onHeightChange={handleHeightChange}
        />
      </GameErrorBoundary>
      <div className="scene-vignette" aria-hidden="true" />
      <GameHud
        challengeKey={challengeKey}
        currentPiece={deck[state.usedCount] ?? null}
        nextPieces={deck.slice(state.usedCount + 1, state.usedCount + 4)}
        usedCount={state.usedCount}
        max={MAX_OBJECTS}
        currentHeight={currentHeight}
        bestHeight={bestHeight}
        notice={state.notice}
        onDestroy={handleDestroy}
        onReset={handleReset}
        onOpenRanking={() => navigate('ranking')}
      />
      {route === 'ranking' && (
        <RankingPage
          challengeKey={challengeKey}
          entries={rankingEntries}
          onBack={() => navigate('game')}
        />
      )}
    </main>
  )
}
