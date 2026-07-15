import { MAX_OBJECTS, canSpawn } from './gameRules'
import type { ShapeKind, StackingItem } from './types'

export interface GameState {
  selectedKind: ShapeKind
  items: StackingItem[]
  destructionVersion: number
  notice: string | null
}

export type GameAction =
  | { type: 'select'; kind: ShapeKind }
  | { type: 'place'; item: StackingItem }
  | { type: 'remove'; id: string }
  | { type: 'destroy' }
  | { type: 'reset' }
  | { type: 'clear-notice' }

export const initialGameState: GameState = {
  selectedKind: 'box',
  items: [],
  destructionVersion: 0,
  notice: null,
}

export function gameReducer(state: GameState, action: GameAction): GameState {
  switch (action.type) {
    case 'select':
      return { ...state, selectedKind: action.kind, notice: null }
    case 'place':
      if (!canSpawn(state.items.length)) {
        return {
          ...state,
          notice: '物体は' + MAX_OBJECTS + '個までです。そろそろ壊しましょう！',
        }
      }
      return { ...state, items: [...state.items, action.item], notice: null }
    case 'remove':
      return { ...state, items: state.items.filter((item) => item.id !== action.id) }
    case 'destroy':
      if (state.items.length === 0) {
        return { ...state, notice: '先に物体を積んでください' }
      }
      return {
        ...state,
        destructionVersion: state.destructionVersion + 1,
        notice: null,
      }
    case 'reset':
      return { ...state, items: [], notice: null }
    case 'clear-notice':
      return { ...state, notice: null }
  }
}
