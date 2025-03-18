# Go型定義からTypeScriptとZodスキーマを生成する方法

このプロジェクトでは、Go言語の型定義から自動的にTypeScript型定義とZodバリデーションスキーマを生成するパイプラインを構築しています。

## 必要なツール

- Go (1.18以上)
- Node.js (14以上)
- tygo: `go install github.com/gzuidhof/tygo@latest`
- ts-to-zod: `npm install --save-dev ts-to-zod`
- zod: `npm install --save zod`

## 使い方

1. Goのコードを変更したら、以下のコマンドを実行します:

```bash
npm run generate
```

これにより、以下の処理が行われます:
- `tygo generate` - Go型からTypeScript型定義を生成
- `ts-to-zod` - TypeScript型定義からZodスキーマを生成

## 生成されるファイル

- TypeScript型定義: `web/types/models.ts`
- Zodスキーマ: `web/schemas/models.zod.ts`

## 設定ファイル

- tygo設定: `tygo.yaml`
- ts-to-zod設定: `ts-to-zod.config.js`

## カスタムマッピング

特殊な型のマッピングは、各設定ファイルでカスタマイズできます。

### `tygo.yaml`の例:

```yaml
type_mappings:
  time.Time: "string /* RFC3339 */"
```

### TypeScriptでの利用例

```typescript
import { User } from "../types/models";
import { userSchema } from "../schemas/models.zod";

// 型としての使用
const user: User = {
  id: 1,
  name: "ユーザー名",
  // ...
};

// バリデーションとしての使用
try {
  const validatedUser = userSchema.parse(inputData);
  // 検証に成功した場合の処理
} catch (error) {
  // 検証に失敗した場合の処理
}
``` 