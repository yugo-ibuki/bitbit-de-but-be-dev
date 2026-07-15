import { SHAPE_CONFIG } from '../game/gameRules'
import type { ShapeKind } from '../game/types'

interface GameHudProps {
  selectedKind: ShapeKind
  count: number
  max: number
  notice: string | null
  onSelect: (kind: ShapeKind) => void
  onDestroy: () => void
  onReset: () => void
}

const kinds: ShapeKind[] = ['box', 'sphere', 'cylinder']

export function GameHud(props: GameHudProps) {
  return (
    <div className="hud">
      <section className="shape-panel" aria-label="落とす形">
        {kinds.map((kind) => (
          <button
            className="shape-button"
            type="button"
            key={kind}
            aria-pressed={props.selectedKind === kind}
            onClick={() => props.onSelect(kind)}
          >
            <span className={'shape-icon ' + kind} aria-hidden="true" />
            {SHAPE_CONFIG[kind].label}
          </button>
        ))}
      </section>

      <aside className="counter" aria-label="物体数">
        <span>OBJECTS</span>
        <strong>{props.count} / {props.max}</strong>
      </aside>

      <p className="instructions">タップで投下 · ドラッグで回転 · ホイールでズーム</p>
      {props.notice && (
        <p className="notice" role="status">
          {props.notice}
        </p>
      )}

      <div className="actions">
        <button className="reset-button" type="button" onClick={props.onReset}>
          もう一度積む
        </button>
        <button className="destroy-button" type="button" onClick={props.onDestroy}>
          <span aria-hidden="true">✦</span> 破壊する
        </button>
      </div>
    </div>
  )
}
