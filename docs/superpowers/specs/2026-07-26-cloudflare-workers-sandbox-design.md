# Cloudflare Workers Sandbox 設計

## 目的

`cloudflare/workers-sandbox/` に、Cloudflare Workers、Queues、Workflowsを実際に動かして比較できる独立プロジェクトを作る。Google CloudのCloud Run、Cloud Tasks、Pub/Sub、Workflowsとの違いを、HTTP受付、非同期配送、水平スケール、冪等性、リトライ、Dead Letter Queue、永続ステップ、明示Fan-outを通して理解できる状態にする。

検証用Cloudflareアカウントは `cloudflare-sandbox` を使用する。既存の `Y.ibuki91@gmail.com's Account` は使用しない。Cloudflare account ID、API token、secretなどはソースやWrangler設定へハードコードしない。

## スコープ

このサンドボックスに含めるものは以下とする。

- HonoによるWorkers HTTP API
- `GET /health`
- `POST /jobs`
- `GET /jobs/:jobId`
- API WorkerからCloudflare Queuesへのジョブ投入
- Queue ConsumerからCloudflare Workflowの開始
- payloadによって処理対象を切り替えるステートレスなジョブ処理
- Queueのat-least-once配送を前提にした冪等化
- QueueリトライとDead Letter Queue
- WorkflowsのStep AからStep Bへの結果受け渡し
- `step.do()`による成功済みステップの永続化と再開
- ステップ単位のリトライを意図的に確認する仕組み
- Producerから複数Queueへ明示送信するFan-out例
- 構造化ログとWorkflow status APIによる結果確認
- `wrangler dev`でのローカル検証
- Bunによる依存管理とスクリプト実行
- 単体テスト、Workers統合テスト、型チェック、デプロイdry-run
- `cloudflare-sandbox`への検証用リソース作成とデプロイ
- Cloudflareと対応するGoogle Cloud製品の相違点を説明するREADME

このサンドボックスに含めないものは以下とする。

- Cloudflare Containers
- Cloudflare D1、KV、R2などの追加永続ストレージ
- 古いMQTTベースのCloudflare Pub/Subプライベートベータ
- Queues Event Subscriptionsを任意アプリイベントのTopicとして扱う構成
- 認証、認可、管理画面
- 汎用ジョブ基盤や任意コード実行
- 本番運用向けのSLO、アラート、課金最適化

## 比較した構成

### 採用: 単一Workerスクリプト

1つのWorkerスクリプトにHonoのfetch handler、Queue handler、Workflow classを置く。1つのWrangler設定でローカル起動、型生成、デプロイ、binding確認を完結させる。

学習対象となるイベント境界はHTTP、Queue、Workflowとして明確に残る一方、複数Workerの起動順序やservice bindingへ論点が散らない。Cloudflareでは同じWorkerが複数QueueのProducerおよびConsumerになれるため、明示Fan-outも小さく表現できる。

### 非採用: API、Consumer、Workflowを別Workerへ分割

Cloud Runサービスや独立デプロイ単位との比較はしやすいが、複数のWrangler設定、ローカルプロセス、デプロイ順序が必要になる。今回の目的には構成管理の比重が大きすぎる。

### 非採用: D1ジョブ台帳を追加

ジョブ一覧や長期結果保存には向くが、QueuesとWorkflowsの違いを理解する目的にD1のスキーマ、migration、整合性設計が加わる。今回はWorkflow instance IDとstatus APIを状態確認と冪等性の境界に使う。

## 技術構成

- TypeScript
- Bun
- Hono
- Cloudflare Workers
- Cloudflare Queues
- Cloudflare Workflows
- Wrangler JSONC
- Vitest
- Cloudflare Workers Vitest integration

通常のWorkersは常駐プロセスではなくイベント駆動のV8 Isolateである。HTTP APIは `listen()` を呼ばず、Honoアプリを `export default` されるWorker handlerのfetch処理として利用する。常駐コンテナが必要な場合はCloudflare Containersが候補になるが、この検証では使用しない。

## リソース名

Cloudflare上の名前は検証用と判別できるよう、次を使用する。

- Worker: `workers-sandbox-api`
- メインQueue: `workers-sandbox-jobs`
- Dead Letter Queue: `workers-sandbox-jobs-dlq`
- Fan-out確認用Queue: `workers-sandbox-audit`
- Workflow: `workers-sandbox-job-workflow`

Cloudflare account IDは `CLOUDFLARE_ACCOUNT_ID` からWranglerへ渡す。リポジトリへ値を保存しない。デプロイ前に `wrangler whoami` とCloudflare Dashboardのアカウント名を確認し、`cloudflare-sandbox` 以外ではリソース作成を実行しない。

## API

### `GET /health`

WorkerがHTTPイベントを処理できていることを確認する。

```json
{
  "status": "ok",
  "service": "workers-sandbox-api"
}
```

### `POST /jobs`

次のJSONを受け付ける。

```json
{
  "target": {
    "id": "customer-42",
    "values": [10, 20, 30]
  },
  "operation": "sum",
  "fanout": true,
  "failStepBOnce": false
}
```

`operation` は `sum`、`average`、`max` のいずれかとする。Workerは `crypto.randomUUID()` でjob IDを生成し、入力を検証してメインQueueへ送信する。`fanout: true` の場合は、同じアプリイベントをaudit Queueへも明示的に送信する。

成功時は `202 Accepted` とjob ID、status URLを返す。Queue間の複数送信はトランザクションではない。途中失敗後のクライアント再送で重複が起こり得るため、job IDを冪等性キーとして後段へ伝える。

### `GET /jobs/:jobId`

Workflow bindingの `get(jobId)` と `status()` を使い、instanceの状態、エラー、完了出力を返す。Queue ConsumerがまだWorkflowを開始していない場合、instanceは存在しないため `404` と、Queue処理後に再試行する説明を返す。

## メッセージ形式

HTTP境界で検証済みのデータだけをQueueへ送る。

```ts
type JobMessage = {
  jobId: string;
  submittedAt: string;
  target: {
    id: string;
    values: number[];
  };
  operation: "sum" | "average" | "max";
  failStepBOnce: boolean;
};
```

Audit Queueには同じjob IDを持つ別の判別可能なメッセージを送る。すべてJSON serializableな値だけを使用する。

## Queue Consumerと冪等性

メインQueueのConsumerは、メッセージごとにjob IDと同じWorkflow instance IDを使用する。処理順は次の通りとする。

1. `env.JOB_WORKFLOW.get(jobId)` で既存instanceを確認する。
2. 存在すれば、そのメッセージは以前の配送で処理済みまたは開始済みなのでackする。
3. 存在しなければ `env.JOB_WORKFLOW.create({ id: jobId, params: message })` を実行する。
4. createが失敗した場合は再度getする。instanceが存在すれば同時実行による重複開始と判断してackし、存在しなければretryする。

Workflow instance IDの一意性を、Queueの重複配送に対する冪等性境界として使う。Consumerはリクエスト単位のmutable stateをmodule scopeへ置かず、どの実行でも同じメッセージを処理できる。このためQueueのbacklogに応じてConsumer Workerが複数実行へ水平スケールしても、特定instanceへの固定割り当てを必要としない。

メインQueueには最大リトライ回数、retry delay、Dead Letter QueueをWrangler設定で指定する。Consumerが一時エラーを解決できなければメッセージは再配送され、上限到達後にDLQへ送られる。

Audit QueueのConsumerは構造化ログを出してackする。ログの重複はat-least-once配送上あり得るため、job IDを含めて重複を識別できるようにする。

## Workflow

Workflow classは `WorkflowEntrypoint` を継承し、bindingには `this.env` でアクセスする。

### Step A: 入力加工

`step.do("transform-input", ...)` で入力値を正規化し、対象ID、値の件数、合計値を返す。結果はserializableなplain objectとする。この結果はWorkflowsに永続化され、instance再開時に成功済みのStep Aを再実行しない。

### Step B: 結果計算

`step.do("calculate-result", retryConfig, ...)` でStep Aの返り値を受け取り、`operation` に応じてsum、average、maxを計算する。ステップの返り値をWorkflowの最終出力として返し、status APIから確認できるようにする。

`failStepBOnce: true` の場合、Step B callbackへ渡されるcontextの `attempt === 1` で意図的に例外を投げる。2回目以降は成功するため、ステップ単位のリトライ、attempt番号、Step Aが再実行されないことをログで確認できる。リトライは短いdelayと固定回数を設定し、ローカル検証を待ちすぎない値にする。

## QueueとPub/Subの違い

Cloudflare Queuesのpush-based Consumerは、Queueごとに1つのConsumer Workerを関連付ける。Consumer Worker自体は複数実行へ自動スケールするが、これは同じメッセージを複数Subscriberへ複製配信するPub/Subではない。

Google Cloud Pub/SubのTopic/Subscriptionに相当する汎用的な自動Fan-outを前提にしない。複数の下流処理が必要な例では、Producerが `workers-sandbox-jobs` と `workers-sandbox-audit` へ明示的に送る。Queue間の送信は非atomicであり、各Consumerは重複と部分成功を考慮する。

Queues Event SubscriptionsはCloudflare製品イベントをQueueへ送る機能であり、任意のアプリイベントTopicとは区別する。古いMQTTベースのCloudflare Pub/Subプライベートベータは使用しない。

## Google Cloudとの比較

| Google Cloud | Cloudflare | このサンドボックスで確認する差 |
| --- | --- | --- |
| Cloud Run | Workers | 常駐HTTPサーバーでlistenせず、fetchイベントを処理する |
| Cloud Tasks | Queues | 非同期ジョブ、再配送、retry、DLQ、水平スケールを確認する |
| Pub/Sub | Queuesの明示的な複数送信 | Topic/Subscriptionによる自動Fan-outではない |
| Workflows | Workflows | step結果の永続化と、成功済みstepを飛ばした再開を確認する |

Cloudflare Queuesはこの用途ではCloud TasksやSQSに近い。メッセージブローカーとして比較するときは、配信先モデルとConsumerの関連付けを分けて考える。

## エラー処理

- API入力不正: `400` とfield単位の安定したエラー形式を返す。
- Queue送信失敗: `500` を返し、成功した可能性のある別Queue送信について重複耐性を持たせる。
- Workflow開始失敗: 既存instance確認後も存在しなければメッセージをretryする。
- Workflow step一時失敗: step単位のretry設定へ委ねる。
- Workflow step恒久失敗: retry上限後にinstanceをerrored状態とし、status APIから確認可能にする。
- 不明なQueue: handlerで明示的にエラーを投げ、誤設定を黙ってackしない。
- ログ: job ID、queue名、step名、attempt、状態をJSONで出し、秘密情報やaccount IDは出さない。

## ファイル境界

- `src/index.ts`: Workerのfetch/queue entrypointとWorkflow classのexport
- `src/api.ts`: Hono app、入力検証、HTTP response変換
- `src/messages.ts`: Queue message型とvalidation
- `src/consumer.ts`: Queueごとのrouting、冪等なWorkflow開始、ack/retry
- `src/workflow.ts`: WorkflowのStep A、Step B、retry設定
- `src/job.ts`: Cloudflareへ依存しない入力加工と計算
- `test/*.test.ts`: 純粋ロジックとAPI/Consumer境界の単体テスト
- `test/*.worker.test.ts`: Workers binding、Queue、Workflowの統合テスト
- `wrangler.jsonc`: sandbox resource bindings、consumer retry/DLQ、observability
- `worker-configuration.d.ts`: `wrangler types`で生成するbinding型
- `README.md`: セットアップ、ローカル実行、リソース作成、Workflow確認、デプロイ、Google Cloud比較

各ファイルは1つの責務を持ち、Cloudflare runtimeなしで検証できる純粋ロジックと、bindingを扱う境界を分離する。

## テストと検証

自動テストは次を確認する。

- operationごとのStep A/Step B計算
- 空配列、不正operation、過大payloadのvalidation
- `POST /jobs` がjob IDとstatus URLを返し、Queueへ期待したmessageを送る
- `fanout: true` のときだけaudit Queueへ送る
- `GET /health`
- 存在するWorkflow statusと存在しないjobのresponse
- Queue重複配送で新しいWorkflowを開始しない
- Workflow create失敗後に既存instanceを確認できればackする
- 一時失敗ではretryし、成功時だけackする
- Step BがStep Aの出力を使用する
- 指定時にStep B初回だけが失敗し、retry後に成功する

実行する検証コマンドは次とする。

```bash
bun install
bun run test
bun run typecheck
bun run cf-typegen
bun run deploy:dry-run
bun run dev
```

ローカルでは `curl` で `GET /health`、`POST /jobs`、`GET /jobs/:jobId` を順に実行し、Queue ConsumerとWorkflow stepのログを確認する。Cloudflare上では同じflowを実行し、Dashboard上のQueue、DLQ、Workflow instance、Worker logsも確認する。

## デプロイ

デプロイ前に、Wranglerの認証先に `cloudflare-sandbox` が含まれることと、環境変数 `CLOUDFLARE_ACCOUNT_ID` が同アカウントを指すことを確認する。次にsandbox名のQueueとDLQを作成し、Workerをdeployする。WorkflowはWrangler設定から作成・更新される。

READMEには初回作成と通常deployを分けて記載する。コマンドは環境変数を参照し、account ID、token、secretの実値を含めない。作成後はCloudflare DashboardとWranglerの一覧コマンドで、すべてのリソースが `cloudflare-sandbox` に存在し、他アカウントに作成されていないことを確認する。
