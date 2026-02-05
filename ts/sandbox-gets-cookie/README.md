# 🍪 iframe内でのCookie取得検証プロジェクト

このプロジェクトは、iframe内からCookie情報を取得できるかどうかを検証するためのサンドボックス環境です。

## 📋 目的

以下のシナリオでCookieの取得可能性を検証します：

1. **Same-origin iframe**: 親ページと同じドメインのiframe
2. **Sandbox属性付きiframe**: `sandbox="allow-scripts"` を設定したiframe
3. **Sandbox + Same-origin**: `sandbox="allow-scripts allow-same-origin"` を設定したiframe
4. **Cross-origin iframe**: 異なるオリジン（ポート）からのiframe

## 🚀 セットアップ

### 1. 依存関係のインストール

```bash
npm install
```

### 2. サーバーの起動

#### メインサーバーのみ起動（Same-origin検証）

```bash
npm start
# または
npm run dev
```

サーバーは `http://localhost:9472` で起動します。

#### Cross-origin検証を含む完全な検証

Cross-origin iframeの検証を行う場合は、**2つのサーバーを起動**する必要があります。

**ターミナル1:**
```bash
npm start
```

**ターミナル2（別ウィンドウ）:**
```bash
npm run start:cross-origin
```

または、1つのコマンドで両方起動:
```bash
npm run start:both
```

- メインサーバー: `http://localhost:9472`
- Cross-originサーバー: `http://localhost:9473`

## 🔍 使い方

### 基本的な検証手順

1. ブラウザで `http://localhost:9472` を開く
2. 「Cookieを設定」ボタンをクリックしてCookieを設定
3. 各iframeでCookieが取得できるか確認

### 機能説明

#### 親ページ（index.html）

- **Cookieを設定**: 複数のテスト用Cookieを設定
- **親ページのCookieを表示**: 親ページで設定されているCookieを確認
- **すべてのCookieをクリア**: 設定したCookieをすべて削除

#### iframe内ページ（iframe.html）

- **Cookieを再読み込み**: iframe内でアクセスできるCookieを再取得
- **Cookie操作を試行**: Cookie の読み取り、書き込み、削除をテスト

## 📊 検証される内容

### 設定されるCookie

以下のCookieが親ページから設定されます：

- `test_cookie_1`: 基本的なテスト用Cookie
- `test_cookie_2`: 特殊文字を含むCookie
- `session_id`: セッションIDを模したCookie
- `user_pref`: ユーザー設定を模したCookie
- `timestamp`: タイムスタンプ付きCookie

### iframe の種類

1. **Same-origin iframe**
   - 属性: なし
   - 期待: Cookieにアクセス可能

2. **Sandbox iframe (scripts only)**
   - 属性: `sandbox="allow-scripts"`
   - 期待: Cookieアクセスが制限される可能性

3. **Sandbox iframe (scripts + same-origin)**
   - 属性: `sandbox="allow-scripts allow-same-origin"`
   - 期待: Cookieにアクセス可能

4. **Cross-origin iframe**
   - 属性: `sandbox="allow-scripts allow-same-origin"`
   - オリジン: `http://localhost:9473` (親ページは9472)
   - 期待:
     - ❌ 親ページのCookieにアクセス不可
     - ✅ 自分自身のオリジンのCookieにアクセス可能
     - ✅ postMessageによる親ページとの通信は可能

## 🔬 検証情報

各iframe内では以下の情報も表示されます：

- ロケーション（URL）
- オリジン
- プロトコル
- ホスト
- iframe内かどうか
- `document.cookie` の長さ
- `navigator.cookieEnabled` の値
- `sandbox` 属性の内容

## 📁 プロジェクト構造

```
sandbox-gets-cookie/
├── package.json                  # プロジェクト設定
├── server.js                     # メインサーバー (localhost:9472)
├── server-cross-origin.js        # Cross-originサーバー (localhost:9473)
├── README.md                     # このファイル
├── public/                       # Same-origin用の静的ファイル
│   ├── index.html               # 親ページ
│   ├── iframe.html              # Same-origin iframe用ページ
│   ├── parent.js                # 親ページのJavaScript
│   └── iframe.js                # iframe内のJavaScript
└── public-cross-origin/          # Cross-origin用の静的ファイル
    ├── iframe-cross-origin.html # Cross-origin iframe用ページ
    └── iframe-cross-origin.js   # Cross-origin iframe用JavaScript
```

## 🧪 テストシナリオ

### シナリオ1: 基本的なCookie取得

1. 「Cookieを設定」ボタンをクリック
2. 各iframe内でCookieが表示されるか確認
3. 「Cookie取得: 成功」と表示されればOK

### シナリオ2: Cookie操作テスト

1. 各iframe内の「Cookie操作を試行」ボタンをクリック
2. 読み取り、書き込み、削除の各操作結果を確認
3. sandbox属性の違いによる動作の差異を観察

### シナリオ3: Cookie更新の同期

1. 親ページで「親ページのCookieを表示」をクリック
2. iframe内で「Cookieを再読み込み」をクリック
3. 両者で同じCookieが表示されるか確認

### シナリオ4: Cross-origin Cookie検証

1. 両方のサーバーを起動（メイン: 9472、Cross-origin: 9473）
2. `http://localhost:9472` にアクセス
3. 「Cookieを設定」ボタンで親ページにCookieを設定
4. Cross-origin iframeセクションで以下を確認:
   - 「このオリジンのCookieを再読み込み」→ 9473のCookieのみ表示（最初は空）
   - 「このオリジン用のCookieを設定」→ 9473にCookieを設定
   - 「親ページのCookieアクセスを試行」→ セキュリティエラーが表示される（期待通り）
5. ブラウザのコンソールで postMessage による通信を確認

## ⚠️ 注意事項

- **HTTPSの必要性**: `Secure` 属性付きCookieはHTTPS環境でのみ動作します
- **ブラウザの違い**: ブラウザによってCookieの扱いが異なる場合があります
- **Same-Site属性**: `SameSite` 属性の設定によって動作が変わります
- **Third-Party Cookie**: サードパーティCookieのブロック設定に影響される場合があります
- **Cross-origin検証**: Cross-origin iframeの検証には両方のサーバーの起動が必須です
- **ポートの競合**: 9472と9473のポートが使用されていないことを確認してください

## 🔒 セキュリティに関する考慮事項

このプロジェクトは検証目的のため、以下の点に注意してください：

- 本番環境では使用しないでください
- 実際の機密情報をCookieに保存しないでください
- `HTTPOnly` 属性の重要性を理解してください
- CSRF対策として `SameSite` 属性を適切に設定してください
- **Cross-origin制約**: Cross-origin環境では、ブラウザのセキュリティポリシーにより、iframe内から親ページのCookieに直接アクセスできません
- **postMessageの安全性**: postMessageを使用する場合は、必ず送信元オリジンを検証してください
- **CORS設定**: 本プロジェクトではCORSを許可していますが、本番環境では適切に制限してください

## 📚 参考資料

- [MDN - HTTP Cookies](https://developer.mozilla.org/ja/docs/Web/HTTP/Cookies)
- [MDN - iframe sandbox attribute](https://developer.mozilla.org/ja/docs/Web/HTML/Element/iframe#sandbox)
- [MDN - Document.cookie](https://developer.mozilla.org/ja/docs/Web/API/Document/cookie)
- [MDN - Same-origin policy](https://developer.mozilla.org/ja/docs/Web/Security/Same-origin_policy)
- [MDN - Window.postMessage](https://developer.mozilla.org/ja/docs/Web/API/Window/postMessage)
- [MDN - CORS](https://developer.mozilla.org/ja/docs/Web/HTTP/CORS)

## 🐛 トラブルシューティング

### Cookieが表示されない

- ブラウザのCookie設定を確認
- ブラウザのコンソールでエラーを確認
- プライベートブラウジングモードでないか確認

### サーバーが起動しない

- ポート9472が使用されていないか確認
- `npm install` が正常に完了したか確認
- Node.jsのバージョンを確認（推奨: v14以上）

### Cross-origin iframeが表示されない

- Cross-originサーバー（ポート9473）が起動しているか確認
- ブラウザのコンソールでネットワークエラーを確認
- ポート9473が他のプロセスに使用されていないか確認
- `npm run start:both` で両方のサーバーを起動しているか確認

### Cross-origin iframeでCookieにアクセスできない（期待通り）

- これは正常な動作です
- Cross-origin環境では、セキュリティ上の理由により親ページのCookieにアクセスできません
- 各オリジンは自分自身のCookieのみアクセス可能です
- postMessageを使用してデータを共有する必要があります

## 📝 ライセンス

MIT
