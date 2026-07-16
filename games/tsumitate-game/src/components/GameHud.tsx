import { SHAPE_CONFIG } from '../game/gameRules'
import type { ChallengePiece, PieceSize } from '../game/types'

interface GameHudProps {
  challengeKey: string
  currentPiece: ChallengePiece | null
  nextPieces: ChallengePiece[]
  usedCount: number
  max: number
  currentHeight: number
  bestHeight: number
  notice: string | null
  onDestroy: () => void
  onReset: () => void
  onOpenRanking: () => void
}

const sizeLabel: Record<PieceSize, string> = {
  small: 'S',
  medium: 'M',
  large: 'L',
}

function PieceIcon({ piece }: { piece: ChallengePiece }) {
  return (
    <span
      className={`shape-icon ${piece.kind} piece-${piece.size}`}
      style={{ color: piece.color }}
      aria-hidden="true"
    />
  )
}

export function GameHud(props: GameHudProps) {
  return (
    <div className="hud">
      <aside className="measurement-panel" aria-label="高さ計測">
        <span className="measurement-label">TOWER HEIGHT</span>
        <strong className="measurement-value">
          <span>{props.currentHeight.toFixed(2)}</span>
          <small>m</small>
        </strong>
        <div className="measurement-best">
          <span>TODAY&apos;S BEST</span>
          <b>{props.bestHeight.toFixed(2)} m</b>
        </div>
        <button
          className="ranking-open-button"
          type="button"
          aria-label="ランキングを見る"
          onClick={props.onOpenRanking}
        >
          <span>RANKING</span>
          <b aria-hidden="true">→</b>
        </button>
      </aside>

      <section className="challenge-panel" aria-label="日替わりデッキ">
        <header className="challenge-header">
          <span>TODAY&apos;S DECK</span>
          <b>{props.challengeKey.replaceAll('-', '.')}</b>
        </header>
        {props.currentPiece ? (
          <div className="current-piece" aria-label="現在の物体">
            <PieceIcon piece={props.currentPiece} />
            <div>
              <small>CURRENT</small>
              <strong>{SHAPE_CONFIG[props.currentPiece.kind].label}</strong>
            </div>
            <b>{sizeLabel[props.currentPiece.size]}</b>
          </div>
        ) : (
          <div className="deck-complete">DECK COMPLETE</div>
        )}
        <div className="next-pieces" aria-label="次の3個">
          <span className="next-label">NEXT 3</span>
          {props.nextPieces.map((piece, index) => (
            <div className="next-piece" key={`${index}-${piece.kind}-${piece.size}`}>
              <small>{index + 1}</small>
              <PieceIcon piece={piece} />
              <span>{SHAPE_CONFIG[piece.kind].label}</span>
              <b>{sizeLabel[piece.size]}</b>
            </div>
          ))}
        </div>
      </section>

      <aside className="counter" aria-label="使用数">
        <span>USED</span>
        <strong>{props.usedCount} / {props.max}</strong>
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
