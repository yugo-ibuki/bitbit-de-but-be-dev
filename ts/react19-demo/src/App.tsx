import { useState, use, useOptimistic, useTransition } from 'react'
import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'

// 非同期データ取得のシミュレーション
function fetchNumber() {
  return new Promise<number>(resolve => {
    setTimeout(() => {
      resolve(Math.floor(Math.random() * 100))
    }, 2000)
  })
}

// 非同期データをラップする
const numberPromise = fetchNumber()

function App() {
  const [count, setCount] = useState(0)
  const [isPending, startTransition] = useTransition()
  const [optimisticCount, addOptimisticCount] = useOptimistic(
    count,
    (currentState: number) => currentState + 1
  )

  // useを使用して非同期データを取得
  const randomNumber = use(numberPromise)

  const handleOptimisticUpdate = async () => {
    startTransition(async () => {
      // 楽観的な更新をトリガー
      addOptimisticCount(1)
      
      try {
        // バックグラウンドで実際の更新を実行
        await new Promise(resolve => setTimeout(resolve, 500))
        setCount(count + 1)
      } catch (error) {
        // エラーが発生した場合、楽観的な更新は自動的にロールバックされます
        console.error('Update failed:', error)
      }
    })
  }

  return (
    <>
      <div>
        <a href="https://vite.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://react.dev" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>React 19 Hooks Demo</h1>
      <div className="card">
        <h2>useOptimistic Demo</h2>
        <button onClick={handleOptimisticUpdate} disabled={isPending}>
          count is {optimisticCount}
        </button>
        <p>Actual count: {count}</p>

        <h2>use Hook Demo</h2>
        <p>Random number from async function: {randomNumber}</p>
      </div>
      <p className="read-the-docs">
        Testing React 19 new hooks
      </p>
    </>
  )
}

export default App
