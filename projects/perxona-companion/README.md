# Luma / Local Avatar Companion

ChatGPTサブスクリプションでログインしたCodex CLIと、端末内のVRMアバター、macOSの`/usr/bin/say`を組み合わせたMacローカル用の会話アプリです。3D描画と読み上げにPerxonaなどの外部アバターサービスは使いません。

## 必要なもの

- macOS（`say`の日本語音声が必要）
- Node.js 22.9以上（開発時は24.5.0で確認）
- Codex CLI 0.163.0以上とChatGPTログイン

OpenAI APIキーは使いません。会話生成時だけCodex CLIがOpenAIへ接続します。`OPENAI_API_KEY`や`CODEX_API_KEY`は子プロセスへ渡さず、ChatGPTログイン以外の認証は拒否します。

## 起動

```bash
cd /Users/yugo/ghq/github.com/yugo-ibuki/bitbit-de-but-be-dev/projects/perxona-companion
codex login status
npm ci
cp .env.example .env
npm start
```

ブラウザで <http://127.0.0.1:8788> を開きます。`npm start`はローカルのThree.jsバンドルを生成してから、127.0.0.1だけでサーバーを起動します。

## 使い方

1. テキスト欄からLumaへ話しかけます。
2. Codexの返答をmacOS `say`がWAVへ変換し、ブラウザが再生します。
3. WAVの振幅に合わせてアバターの口が動きます。「発話を止める」で生成中の通信と再生を停止できます。
4. 設定ではインストール済みの日本語音声、動き、背景を選べます。

音声入力、ブラウザの`speechSynthesis`、クラウドTTSは使いません。会話履歴はページ内だけで保持します。

## 構成と安全策

- Three.js 0.180.0と`@pixiv/three-vrm` 3.5.5をesbuildでローカルbundle
- 配布可能なVRMモデルを`public/assets/models/luma.vrm`へ同梱
- `say`へ文章をstdinで渡し、shellを使わず、voice・長さ・timeout・同時実行を制限
- 一時WAVを成功・失敗のどちらでも削除
- Codexを独立tmp cwd、read-only、ephemeral、ユーザー設定無視で起動
- Host/Origin、本文サイズ、会話履歴、子プロセス出力を制限
- CSPはself、data、ローカルVRM texture用blobだけを許可

ローカル単一ユーザー向けのPoCです。公開ホスティングや多人数認証は備えていません。Flutter版は対象外ですが、`public/avatar.js`はDOMコンテナを受け取る独立rendererとして分けています。WebViewへ移植する場合はWebGL、AudioContext、ローカルasset URL、macOS `say`相当のnative bridgeが別途必要です。

## 開発確認

```bash
npm test
npm run check
npm run build
```

VRMファイルのSHA-256は`12c2b97e95e700783a6a550dc0eee2d7880aeedccef9ae67bc4c5a2f0f2631a2`です。モデルとライブラリの出典・ライセンスは[THIRD_PARTY_NOTICES.md](./THIRD_PARTY_NOTICES.md)を参照してください。

## 確認範囲

Codex CLIのChatGPTログイン、`gpt-5.6-sol`の実応答、macOS KyokoによるWAVE生成（mono / 16-bit / 22050Hz）、HTTP契約を確認済みです。ブラウザではtexture付きVRMの頭全体、手が画面内に収まるwave、自然なbow、gesture終了後のidle復帰を確認しました。実際の日本語会話からnative WAVを再生し、振幅peak 0.583と口が開いた画面、再生途中の停止後にphase `stopped`・振幅`0.000`・idleへ戻ることも確認済みです。

送信処理中のEnterで次の下書きが失われないこと、重複除去された日本語音声9件、最終reload後のconsole error/warnが0件であることを確認しました。独立した再検証でも`npm test` 17件、`npm run check`、`npm run build`はすべて成功しています。音声品質を人が耳で評価する聴感確認、nodの目視確認、Flutter/mobileアプリ実装は行っていません。
