import { Component, type ErrorInfo, type ReactNode } from 'react'

interface Props {
  children: ReactNode
}

interface State {
  failed: boolean
}

export class GameErrorBoundary extends Component<Props, State> {
  state: State = { failed: false }

  static getDerivedStateFromError(): State {
    return { failed: true }
  }

  componentDidCatch(error: Error, info: ErrorInfo) {
    console.error('The 3D game failed to start', error, info)
  }

  render() {
    if (this.state.failed) {
      return (
        <div className="fatal-error" role="alert">
          <p className="eyebrow">SYSTEM PAUSED</p>
          <h2>ゲームを起動できませんでした</h2>
          <p>WebGL対応ブラウザでページを再読み込みしてください。</p>
          <button type="button" onClick={() => window.location.reload()}>
            再読み込み
          </button>
        </div>
      )
    }
    return this.props.children
  }
}
