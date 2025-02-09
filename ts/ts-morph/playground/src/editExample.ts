import { Project } from "ts-morph";
import * as path from "path";

// プロジェクトの初期化
const project = new Project();

// 既存のファイルのパスを設定
const filePath = path.join(__dirname, "generated", "example.ts");

// 既存のファイルを読み込む
const sourceFile = project.addSourceFileAtPath(filePath);

// 既存の関数を修正する
function modifyExistingFunctions() {
  // greetUser関数を見つけて修正
  const greetUser = sourceFile.getFunction("greetUser");
  if (greetUser) {
    greetUser.setBodyText(`return \`Hello, \${user.name}! Your ID is \${user.id}\`;`);
  }

  // fetchUserById関数を見つけて修正
  const fetchUser = sourceFile.getFunction("fetchUserById");
  if (fetchUser) {
    fetchUser.setBodyText(`// 実際のAPIコールをシミュレート
      const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));
      await delay(1000);
      return { id, name: "Modified User " + id }`);
  }
}

// 新しい関数を追加
function addNewFunction() {
  // validateUser関数が既に存在するかチェック
  const existingFunction = sourceFile.getFunction("validateUser");
  
  // 関数が存在しない場合のみ追加
  if (!existingFunction) {
    sourceFile.addFunction({
      name: "validateUser",
      parameters: [{
        name: "user",
        type: "User"
      }],
      returnType: "boolean",
      statements: [`
        if (!user.id || !user.name) {
          return false;
        }
        return user.id > 0 && user.name.length > 0;
      `],
      docs: ["ユーザーデータのバリデーションを行う関数"]
    });
    console.log("Added validateUser function");
  } else {
    console.log("validateUser function already exists");
  }
}

// 変更を実行
console.log("Modifying existing functions...");
modifyExistingFunctions();

console.log("Adding new function...");
addNewFunction();

// フォーマットを適用
sourceFile.formatText();

// 変更を保存
project.saveSync();

console.log(`File has been modified at: ${filePath}`);

// 修正後のファイルの内容を表示
console.log("\nModified file contents:");
// console.log(sourceFile.getFullText()); 