import { MAX_OBJECTS, canSpawn } from './gameRules'
import type { StackingItem } from './types'

export interface GameState {
  items: StackingItem[]
  usedCount: number
  destructionVersion: number
  containmentEnabled: boolean
  notice: string | null
}

export type GameAction =
  | { type: 'place'; item: StackingItem }
  | { type: 'remove'; id: string }
  | { type: 'destroy' }
  | { type: 'restore-containment' }
  | { type: 'deck-complete' }
  | { type: 'reset' }
  | { type: 'clear-notice' }

export const initialGameState: GameState = {
  items: [],
  usedCount: 0,
  destructionVersion: 0,
  containmentEnabled: true,
  notice: null,
}

export function gameReducer(state: GameState, action: GameAction): GameState {
  switch (action.type) {
    case 'place':
      if (!canSpawn(state.usedCount)) {
        return {
          ...state,
          notice: '物体は' + MAX_OBJECTS + '個までです。そろそろ壊しましょう！',
        }
      }
      return {
        ...state,
        items: [...state.items, action.item],
        usedCount: state.usedCount + 1,
        notice: null,
      }
    case 'remove':
      return { ...state, items: state.items.filter((item) => item.id !== action.id) }
    case 'destroy':
      if (state.items.length === 0) {
        return { ...state, notice: '先に物体を積んでください' }
      }
      return {
        ...state,
        destructionVersion: state.destructionVersion + 1,
        containmentEnabled: false,
        notice: null,
      }
    case 'restore-containment':
      return { ...state, containmentEnabled: true }
    case 'deck-complete':
      return { ...state, notice: '本日の100個を使い切りました' }
    case 'reset':
      return {
        ...state,
        items: [],
        usedCount: 0,
        containmentEnabled: true,
        notice: null,
      }
    case 'clear-notice':
      return { ...state, notice: null }
  }
}
