# みんなのものさし

身近な問いへ一つずつ答え、自分と他の参加者の「普通」を人数・割合で見比べる匿名参加型Webアプリです。管理者は、可変数の選択肢を持つ質問を最大10問までまとめ、下書き・公開・終了・複製できます。

公開版: <https://minna-no-monosashi.y-ibuki91.workers.dev>

## 必要な環境

- Node.js 22以上
- npm
- Cloudflareアカウント（本番へ公開する場合）

このディレクトリはリポジトリ内の他プロジェクトから独立しています。以下のコマンドはすべて `games/minna-no-monosashi/` で実行してください。

## ローカル起動

```bash
npm install
cp .dev.vars.example .dev.vars
npm run db:migrate:local
npm run db:seed:local
npm run dev
```

表示されたURLをブラウザで開きます。サンプルブロックは `/play/daily-boundaries`、管理画面は `/admin/login` です。

ローカル開発用の管理者パスワードは `dev-admin` です。この値と `.dev.vars.example` の秘密値は本番で使用しないでください。

## 開発コマンド

```bash
npm run test:unit     # ドメインとReact UI
npm run test:worker   # WorkersランタイムとD1を使ったAPI統合テスト
npm run test:e2e      # モバイル・デスクトップの実ブラウザフロー
npm test              # unitとWorker integration
npm run typecheck
npm run build
```

Playwrightのブラウザが未導入の場合は、初回だけ次を実行します。

```bash
npx playwright install chromium
```

## データベース

ローカルD1へマイグレーションとサンプル10問を投入します。

```bash
npm run db:migrate:local
npm run db:seed:local
```

seedは同じIDに対する `INSERT OR IGNORE` で構成しているため、再実行しても質問や選択肢は増えません。ローカルDBを初期化したい場合は、開発データを退避したうえで `.wrangler/state` を削除し、上記2コマンドを再実行してください。

## 管理者パスワードハッシュ

8文字以上のパスワードから、PBKDF2-SHA256（Cloudflare Workersの上限に合わせた100,000回）のハッシュを作成します。引数を省略するとターミナル上で非表示入力します。

```bash
npm run hash-admin-password -- '十分に強い本番パスワード'
npm run hash-admin-password
```

平文パスワードはGit、README、通常ログへ保存しないでください。

## Cloudflareへ公開

まず認証とアカウントを確認します。

```bash
npx wrangler login
npx wrangler whoami
```

D1を作成し、表示された `database_id` で `wrangler.jsonc` のゼロUUIDを置き換えます。

```bash
npx wrangler d1 create minna-no-monosashi
```

本番用の値を3つのCloudflare Secretへ設定します。`ADMIN_PASSWORD_HASH` には前節で生成した文字列を入れ、ほか2つにはそれぞれ独立した十分に長い乱数を使います。

```bash
npx wrangler secret put ADMIN_PASSWORD_HASH
npx wrangler secret put SESSION_SECRET
npx wrangler secret put VOTER_HASH_SECRET
```

本番D1を準備し、サンプルを投入してデプロイします。

```bash
npm run db:migrate:remote
npm run db:seed:remote
npm run deploy
```

デプロイ後は、公開URLの一覧・回答・回答直後の集計・管理画面を確認してください。2つの別ブラウザまたはプライベートウィンドウから異なる回答を行うと、匿名集計が複数参加者として反映されることを確認できます。

この環境で自動生成した本番管理者パスワードは、macOSキーチェーンのサービス名 `minna-no-monosashi-admin` に保存しています。必要なときはキーチェーンアクセスで確認してください。

## 仕様上の制約

- 「1人1回」は本人確認ではなく、ブラウザの保存領域単位です。
- 端末トークンの生値は送信後にHMAC化され、D1にはハッシュだけを保存します。
- localStorageを消す、別ブラウザ・別端末・プライベートブラウジングを使う場合は別参加者として扱われます。
- 氏名、メールアドレス、位置情報、User-Agent、生IPアドレスを回答レコードへ保存しません。
- 公開中の質問別結果は、その質問へ回答済みのブラウザだけが閲覧できます。終了済みブロックの結果は誰でも閲覧できます。
- 公開後の質問内容は変更・削除できません。修正する場合は複製して新しい下書きを作ります。

## 構成

- `src/client/`: 参加者画面と管理画面
- `src/domain/`: 入力ルールと割合計算
- `src/worker/`: Hono API、認証、D1リポジトリ
- `migrations/`: D1スキーマ
- `seed/`: 再実行可能なサンプルデータ
- `test/worker/`: Workers + D1統合テスト
- `e2e/`: Playwrightによる主要フロー
