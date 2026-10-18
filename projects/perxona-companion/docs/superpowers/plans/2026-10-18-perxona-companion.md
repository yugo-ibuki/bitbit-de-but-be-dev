# Perxona Companion Implementation Plan

旧構成の記録です。現在の実装は[ローカルアバター構成](./2026-10-18-local-avatar.md)に置き換わっており、Perxona SDKやキーの設定は使用しません。

**Goal:** Perxonaの発話するアバターとChatGPTログイン済みCodexの会話をローカルブラウザで接続する。

**Architecture:** Node組み込みHTTPサーバー、公式Perxona Connect SDK、Codex CLI子プロセス。publishableキーでカタログとSDKを利用し、任意のsecretキーはcredit読取だけに使う。

**Tech Stack:** Node.js 22.9+, JavaScript ES modules, HTML/CSS, node:test.

**Spec:** ../../spec.md

## Global Constraints

- この新規プロジェクト内だけ変更する。コミット、push、公開はしない。
- .envのsecretをブラウザへ渡さない。APIキーによるOpenAI呼び出しへフォールバックしない。
- 実際のPerxona認証と3D表示未確認ならその状態を明示する。

## Task 1: ローカルAPIと会話

Files: package.json, .gitignore, .env.example, server.mjs, lib/codex.mjs, lib/perxona.mjs, test/*.test.mjs.

- [ ] node:testで未設定カタログが503、APIキー認証のCodex拒否、空の会話拒否、サーバーsecretがHTTP応答に含まれないことをテストする。足りない機能で失敗することを確認する。
- [ ] createServer()、generateReply(messages, options)、readCatalog(kind)を実装する。サブプロセスはspawn引数配列、stdin、一時cwd、タイムアウトを使う。
- [ ] `npm test` と `node --check` で契約を検証する。

## Task 2: 会話画面と公式SDK

Files: public/index.html, public/app.js, public/style.css, README.md.

- [ ] ステージ、会話ログ、入力フォーム、初期メッセージ、候補プロンプト、設定UIを実装する。
- [ ] `/api/config` とカタログを読んでSDKをロードし、ユーザークリックのハンドラーからresumeAudioPlayback()、initializeWithConnectKey(key, target)を実行する。
- [ ] Readyイベント後だけpresent()を呼び、失敗は会話生成成功と分けて表示する。voiceとmotionの値は実際のカタログに基づく。
- [ ] READMEに.env設定、APIキーを送らないCodexログイン、料金の上限、実際の検証範囲を書く。
- [ ] サーバーを起動し、HTTPとブラウザで未設定状態・本物のテキスト会話・設定画面・失敗回復を確認する。
