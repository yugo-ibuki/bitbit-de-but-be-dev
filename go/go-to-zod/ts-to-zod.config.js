module.exports = {
  input: './web/types/models.ts',
  output: './web/schemas/models.zod.ts',
  skipParseJSDoc: false, // JSDocコメントを解析するか
  constToEnum: true, // constをz.enumに変換するか
  useNamedExports: true, // 名前付きエクスポートを使用するか
  exportAll: true, // すべてのスキーマをエクスポートするか
  createPartialSchemas: true, // Partialスキーマも生成するか
  // 埋め込み型を展開する設定
  expandTypes: true,
  // 型の展開を制御するオプション
  typeExpansionOptions: {
    expandInterfaces: true,
    expandTypeLiterals: true,
  },
  templateOutput: (output) => `// このファイルは自動生成されています。手動で編集しないでください。
import { z } from 'zod';

${output}`,
} 