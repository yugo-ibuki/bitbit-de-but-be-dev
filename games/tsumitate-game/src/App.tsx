import { useCallback, useEffect, useReducer, useRef } from 'react'
import { GameErrorBoundary } from './components/GameErrorBoundary'
import { GameHud } from './components/GameHud'
import { GameScene } from './components/GameScene'
import { MAX_OBJECTS, createStackingItem } from './game/gameRules'
import { gameReducer, initialGameState } from './game/gameState'
import type { Vec3 } from './game/types'

export function App() {
  const [state, dispatch] = useReducer(gameReducer, initialGameState)
  const nextId = useRef(1)

  const placeObject = useCallback(
    (point: Vec3) => {
      const item = createStackingItem(
        state.selectedKind,
        point,
        'object-' + nextId.current++,
      )
      dispatch({ type: 'place', item })
    },
    [state.selectedKind],
  )

  useEffect(() => {
    if (!state.notice) return
    const timer = window.setTimeout(() => dispatch({ type: 'clear-notice' }), 2600)
    return () => window.clearTimeout(timer)
  }, [state.notice])

  return (
    <main className="app-shell">
      <GameErrorBoundary>
        <GameScene
          items={state.items}
          destructionVersion={state.destructionVersion}
          onPlace={placeObject}
          onRemove={(id) => dispatch({ type: 'remove', id })}
        />
      </GameErrorBoundary>
      <div className="scene-vignette" aria-hidden="true" />
      <GameHud
        selectedKind={state.selectedKind}
        count={state.items.length}
        max={MAX_OBJECTS}
        notice={state.notice}
        onSelect={(kind) => dispatch({ type: 'select', kind })}
        onDestroy={() => dispatch({ type: 'destroy' })}
        onReset={() => dispatch({ type: 'reset' })}
      />
    </main>
  )
}
