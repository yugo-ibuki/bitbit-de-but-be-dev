import { useCallback, useEffect, useMemo, useReducer, useRef, useState } from 'react'
import { GameErrorBoundary } from './components/GameErrorBoundary'
import { GameHud } from './components/GameHud'
import { GameScene } from './components/GameScene'
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
import type { Vec3 } from './game/types'
import { useContainmentRecovery } from './game/useContainmentRecovery'

export function App() {
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
  const nextId = useRef(1)

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
    setCurrentHeight(0)
    dispatch({ type: 'reset' })
  }, [])

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
        onDestroy={() => dispatch({ type: 'destroy' })}
        onReset={handleReset}
      />
    </main>
  )
}
