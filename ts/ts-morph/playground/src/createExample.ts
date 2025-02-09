import { Project, SyntaxKind, FunctionDeclaration } from "ts-morph";
import * as path from "path";

// プロジェクトの初期化
const project = new Project();

// 新しいファイルのパスを設定
const outputPath = path.join(__dirname, "generated", "example.ts");

// 新しいファイルを作成
const sourceFile = project.createSourceFile(
  outputPath,
  `
interface User {
  id: number;
  name: string;
}

function greetUser(user: User): string {
  return \`Hello, \${user.name}!\`;
}
`,
  { overwrite: true } // 既存のファイルを上書き
);

// 新しい関数を追加する
function addNewFunction() {
  sourceFile.addFunctions([
    {
      name: "processUserList",
      parameters: [{
        name: "users",
        type: "User[]"
      }],
      returnType: "void",
      statements: [
        `users.forEach(user => {
          console.log(greetUser(user));
        });`
      ],
      docs: ["ユーザーリストを処理する関数"]
    },
    {
      name: "fetchUserById",
      isAsync: true,
      parameters: [{
        name: "id",
        type: "number"
      }],
      returnType: "Promise<User>",
      statements: [
        `return { id, name: "User " + id };`
      ],
      docs: ["非同期でユーザーを取得する関数"]
    }
  ]);

  // フォーマットを適用
  sourceFile.formatText();
}

// 関数を追加
addNewFunction();

// ファイルを保存
project.saveSync();

console.log(`File has been created at: ${outputPath}`);

// 生成されたファイルの内容を表示
console.log("\nGenerated file contents:");
console.log(sourceFile.getFullText());

// 実際のファイルに保存したい場合は以下のようにします
// project.saveSync();
