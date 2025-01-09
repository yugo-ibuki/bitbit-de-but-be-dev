import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [
    react({
      // jsxRuntime: 'automatic', // この指定は不要です
      // 正しくは以下のように指定します
      babel: {
        parserOpts: {
          plugins: ['jsx']
        }
      }
    })
  ],
  resolve: {
    alias: {
      'react': 'react@experimental',
      'react-dom': 'react-dom@experimental'
    }
  }
})