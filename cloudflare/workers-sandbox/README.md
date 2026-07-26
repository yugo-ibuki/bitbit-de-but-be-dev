# Cloudflare Workers Sandbox

Cloudflare Workers、Queues、Workflowsを実際に動かし、Google CloudのCloud Run、Cloud Tasks、Pub/Sub、Workflowsとの違いを確認するための独立したサンドボックスです。

## 構成

```text
POST /jobs
  |
  +--> workers-sandbox-jobs Queue
  |      |
  |      +--> Queue Consumer
  |              |
  |              +--> workers-sandbox-job-workflow
  |                       |
  |                       +--> Step A: 入力値を正規化
  |                       |
  |                       +--> Step B: Step Aの結果を使って集計
  |
  +--> workers-sandbox-audit Queue  # fanout: true のときだけ明示的に送信
         |
         +--> Audit Consumer -> 構造化ログ
```

通常のWorkersは常駐デーモンやコンテナではなく、リクエストやQueueメッセージなどのイベントで起動するV8 Isolateです。Honoアプリは`listen()`せず、Workerの`fetch`ハンドラーとして`export default`しています。常駐コンテナが必要な用途はCloudflare Containersの検討対象ですが、このサンドボックスには含めません。

主要ファイルは次のとおりです。

- `src/api.ts`: Hono API、Queue Producer、Workflow状態確認API
- `src/consumer.ts`: jobs/audit Queue Consumerと冪等化
- `src/workflow.ts`: Step A、Step B、ステップ単位のリトライ
- `src/index.ts`: Workersの`fetch`/`queue`エントリーポイント
- `wrangler.jsonc`: Queue、DLQ、Workflow、Consumer並列度の設定

## 必要なもの

- [Bun](https://bun.sh/)
- CloudflareアカウントとWranglerのログイン
- QueuesとWorkflowsを利用できるCloudflareプラン

依存関係をインストールします。

```bash
cd cloudflare/workers-sandbox
bun install
```

## ローカル実行

ローカル開発サーバーを起動します。ローカル実行ではCloudflare上のQueueを事前作成する必要はありません。

```bash
bun run dev
```

別のターミナルからヘルスチェックします。

```bash
curl --fail http://localhost:8787/health
```

ジョブを投入します。

```bash
curl --fail-with-body \
  --request POST \
  --header 'content-type: application/json' \
  --data '{
    "target": {
      "id": "sample-001",
      "values": [10, 20, 30]
    },
    "operation": "average",
    "fanout": true,
    "failStepBOnce": true
  }' \
  http://localhost:8787/jobs
```

レスポンスの`jobId`を使って状態と結果を確認します。

```bash
curl --fail-with-body \
  http://localhost:8787/jobs/<jobId>
```

`failStepBOnce: true`は、Step Bの1回目だけ意図的に失敗させる検証用フラグです。ログに`Intentional first-attempt failure`が出たあと、Step Bだけが再試行され、`stepBAttempt: 2`で完了します。成功済みのStep Aは再実行されません。

Workflow CLIでもローカルインスタンスを確認できます。`wrangler dev`を起動したまま実行してください。

```bash
bunx wrangler workflows instances list \
  workers-sandbox-job-workflow \
  --local

bunx wrangler workflows instances describe \
  workers-sandbox-job-workflow \
  <jobId> \
  --local
```

Queueを経由せずWorkflow単体を試す場合は、次のように直接起動できます。

```bash
bunx wrangler workflows trigger \
  workers-sandbox-job-workflow \
  '{"jobId":"workflow-direct-001","submittedAt":"2026-01-01T00:00:00.000Z","target":{"id":"sample-001","values":[10,20,30]},"operation":"sum","failStepBOnce":false}' \
  --id workflow-direct-001 \
  --local
```

## API

### `GET /health`

WorkerとHonoの動作確認用です。

```json
{
  "status": "ok",
  "service": "workers-sandbox-api"
}
```

### `POST /jobs`

入力例です。

```json
{
  "target": {
    "id": "sample-001",
    "values": [10, 20, 30]
  },
  "operation": "average",
  "fanout": true,
  "failStepBOnce": true
}
```

- `target.id`: 処理対象の識別子
- `target.values`: 対象ごとに変わる数値配列
- `operation`: `sum`、`average`、`max`
- `fanout`: audit Queueにも明示的に送るか
- `failStepBOnce`: WorkflowのStep Bを1回だけ失敗させるか

受付成功時は`202 Accepted`を返します。

```json
{
  "jobId": "generated-uuid",
  "statusUrl": "/jobs/generated-uuid"
}
```

### `GET /jobs/:jobId`

Workflowインスタンスの状態と、完了後の結果を返します。Queue ConsumerがまだWorkflowを開始していない間は`404 JOB_NOT_STARTED`になり得るため、少し待って再取得してください。

## Queuesで確認できること

### at-least-once配送と冪等性

QueuesのConsumerはat-least-once配送を前提にしています。同じメッセージが再配送されても、`jobId`をWorkflowインスタンスIDとして使用し、既存インスタンスがあれば新しく開始せず`ack()`します。

Consumerが同じ`jobId`を同時に処理した競合も考慮しています。`create()`に失敗した場合はもう一度`get()`し、別のConsumerがすでに作成していれば成功扱いにします。Workflow開始前の一時的な失敗だけを`retry()`します。

### リトライとDead Letter Queue

`workers-sandbox-jobs`は`wrangler.jsonc`で次を設定しています。

- 最大3回のリトライ
- リトライ間隔2秒
- Dead Letter Queue: `workers-sandbox-jobs-dlq`
- 最大同時実行数10

設定された再試行を使い切ったメッセージはDLQへ送られます。このDLQにはConsumerを設定していないため、失敗メッセージを保持して調査できます。

### 1 Queueにつき1 Consumer

Cloudflare Queuesのpush-based Consumerは、1つのQueueに関連付けられるConsumer Workerが1つです。そのConsumer Workerの実行自体は負荷に応じて複数のWorker invocationへ水平スケールします。

これは、1件のメッセージを複数のSubscriptionへ複製配信するGoogle Cloud Pub/Subではありません。同じWorkerスクリプトが複数のQueueをconsumeする設定はできますが、それぞれのQueueには個別のConsumer設定が必要です。

### 明示的なFan-out

`fanout: true`の場合、API Producerは同じアプリイベントを次の2つへ明示的に送ります。

1. `workers-sandbox-jobs`
2. `workers-sandbox-audit`

この2回の`send()`はトランザクションではありません。汎用Topic/Subscriptionによる自動Fan-outが必要なら、配信先Queueの管理、部分失敗時の再送、Outboxなどをアプリケーション側で設計します。

古いMQTTベースのCloudflare Pub/Subプライベートベータは使用しません。また、Queues Event SubscriptionsはCloudflare製品が発生させるイベントをQueueへ接続する機能であり、任意のアプリケーションイベントを配信する汎用Topicとは別物です。

## Workflowsで確認できること

Workflowは`JobWorkflow`クラスとして実装しています。

1. `transform-input`: 入力値を正規化し、`targetId`、`values`、`count`を返す
2. `calculate-result`: Step Aの永続化された結果を受け取り、指定した集計を行う

各処理は`step.do()`で囲まれています。Step Aが成功したあとにStep Bが失敗しても、Step Aの結果から再開でき、成功済み処理をもう一度実行しません。Step Bには独立した最大3回、1秒間隔のリトライを設定しています。

Queue Consumerは`jobId`を決定的なWorkflowインスタンスIDとして渡すため、Queue再配送時も同じ処理を重複開始しません。

## Google Cloudとの対応

| 確認したい概念 | Cloudflare | Google Cloudとの比較 |
|---|---|---|
| HTTP API | Workers + Hono | Cloud Runは常駐可能なコンテナ。Workersはイベント駆動のIsolate |
| 非同期ジョブ投入 | Queues | Cloud TasksやSQSに近い。HTTP TaskそのものではなくメッセージをConsumerが処理 |
| Consumerのスケール | Queue Consumer Worker | 1 QueueのConsumer処理が複数invocationへ水平スケール |
| 1対多のイベント配信 | 複数Queueへ明示的に`send()` | Pub/SubのTopic/Subscriptionのような自動Fan-outではない |
| 複数ステップの耐久実行 | Workflows + `step.do()` | Google Cloud Workflowsに相当。成功済みステップを保存して再開 |
| 常駐プロセス | 今回は対象外 | Cloud Runに近い常駐コンテナ要件ならCloudflare Containersを検討 |

## テストと静的検証

```bash
bun run test
bun run typecheck
bun run cf-typegen:check
bun run deploy:dry-run
bun run check:startup
```

ユニットテストは入力検証、計算、Hono API、Consumerの冪等性とリトライを確認します。Workers統合テストは実際のWorkerエントリーポイントとWorkflowのStep A/Step B、Step Bの再試行をworkerd上で確認します。

## cloudflare-sandboxへのデプロイ

このサンドボックスの対象アカウントは`cloudflare-sandbox`です。別アカウントへ誤って作成しないよう、Wranglerの専用認証プロファイルをこのディレクトリへ紐づけます。Account IDは`.env`、`wrangler.jsonc`、READMEへ保存しません。

初回だけ専用プロファイルを作成します。ブラウザの認可画面では`cloudflare-sandbox`だけを選択してください。

```bash
bunx wrangler auth create cloudflare-sandbox
bunx wrangler auth activate cloudflare-sandbox .
```

現在の対象を確認します。

```bash
bunx wrangler whoami --json
```

`accounts`が`cloudflare-sandbox`の1件だけであることを確認してから、3つのQueueを作成します。

```bash
bunx wrangler queues create workers-sandbox-jobs
bunx wrangler queues create workers-sandbox-jobs-dlq
bunx wrangler queues create workers-sandbox-audit
```

Worker、Queue Consumer、Workflowをまとめてデプロイします。

```bash
bun run deploy
```

デプロイ後は、表示された`workers.dev` URLでローカルと同じAPIを実行できます。

`workers.dev`のサブドメインはアカウントの表示名とは別に管理されるため、以前のアカウント名がURLに残っている場合があります。操作先は`whoami --json`のアカウント情報と、各Wranglerコマンドに表示される`Active profile: cloudflare-sandbox`で判断します。

```bash
curl --fail https://<workers.dev URL>/health

curl --fail-with-body \
  --request POST \
  --header 'content-type: application/json' \
  --data '{
    "target": {
      "id": "remote-001",
      "values": [2, 4, 8]
    },
    "operation": "sum",
    "fanout": true,
    "failStepBOnce": true
  }' \
  https://<workers.dev URL>/jobs
```

リモートのWorkflow状態も確認できます。

```bash
bunx wrangler workflows instances list \
  workers-sandbox-job-workflow

bunx wrangler workflows instances describe \
  workers-sandbox-job-workflow \
  <jobId>
```

Queueを確認する場合は次を実行します。

```bash
bunx wrangler queues list
```

作業後もこのディレクトリに紐づいたプロファイルを使い、書き込み前に`whoami --json`で対象を確認してください。

## 設定値とSecret

サンドボックス固有のリソース名だけを`wrangler.jsonc`へ置いています。Account ID、API Token、メールアドレスなどはハードコードしていません。

Secretを追加する場合はソースや`.dev.vars`をコミットせず、ローカルでは`.dev.vars.example`のような名前だけを共有し、リモートでは`wrangler secret put`を使用してください。
