import { Project, ObjectLiteralExpression, ArrayLiteralExpression, Node } from "ts-morph";
import * as path from "path";

// プロジェクトの初期化
const project = new Project();

// 新しいファイルを作成（または既存のファイルを読み込む）
const sourceFile = project.createSourceFile(
  path.join(__dirname, "generated", "objects.ts"),
  `
// ユーザー設定のオブジェクト
const config = {
  apiUrl: "https://api.example.com",
  timeout: 5000,
  retryCount: 3,
  features: {
    darkMode: false,
    notifications: true
  }
};

// ユーザーリスト
const users = [
  { id: 1, name: "Alice", role: "admin" },
  { id: 2, name: "Bob", role: "user" }
];

// API エンドポイントの設定
const endpoints = {
  users: "/api/users",
  posts: "/api/posts"
};
`,
  { overwrite: true }
);

// オブジェクトを修正する関数
function modifyObjects() {
  // configオブジェクトを見つけて修正
  const configDeclaration = sourceFile.getVariableDeclaration("config");
  const configObject = configDeclaration?.getInitializer() as ObjectLiteralExpression;
  
  if (configObject) {
    // プロパティの追加
    configObject.addPropertyAssignment({
      name: "version",
      initializer: '"1.0.0"'
    });

    // プロパティの値を変更
    const timeoutProp = configObject.getProperty("timeout");
    if (timeoutProp && Node.isPropertyAssignment(timeoutProp)) {
      timeoutProp.setInitializer("10000");
    }

    // ネストされたオブジェクトのプロパティを変更
    const features = configObject.getProperty("features");
    if (features && Node.isPropertyAssignment(features)) {
      const featuresObj = features.getInitializer() as ObjectLiteralExpression;
      featuresObj.addPropertyAssignment({
        name: "betaFeatures",
        initializer: "true"
      });
    }
  }

  // usersの配列を修正
  const usersDeclaration = sourceFile.getVariableDeclaration("users");
  const usersArray = usersDeclaration?.getInitializer() as ArrayLiteralExpression;
  
  if (usersArray) {
    // 新しいユーザーを追加
    usersArray.addElement(`{ id: 3, name: "Charlie", role: "moderator" }`);
  }

  // endpointsオブジェクトを修正
  const endpointsDeclaration = sourceFile.getVariableDeclaration("endpoints");
  const endpointsObject = endpointsDeclaration?.getInitializer() as ObjectLiteralExpression;
  
  if (endpointsObject) {
    // 新しいエンドポイントを追加
    endpointsObject.addPropertyAssignments([
      {
        name: "comments",
        initializer: '"/api/comments"'
      },
      {
        name: "auth",
        initializer: '"/api/auth"'
      }
    ]);
  }
}

// オブジェクトの型定義を追加
function addTypes() {
  // ファイルの先頭に型定義を追加
  sourceFile.insertStatements(0, `
interface Config {
  apiUrl: string;
  timeout: number;
  retryCount: number;
  version: string;
  features: {
    darkMode: boolean;
    notifications: boolean;
    betaFeatures: boolean;
  }
}

interface User {
  id: number;
  name: string;
  role: string;
}

type Endpoints = Record<string, string>;
`);

  // 変数宣言に型アノテーションを追加
  const configDeclaration = sourceFile.getVariableDeclaration("config");
  configDeclaration?.setType("Config");

  const usersDeclaration = sourceFile.getVariableDeclaration("users");
  usersDeclaration?.setType("User[]");

  const endpointsDeclaration = sourceFile.getVariableDeclaration("endpoints");
  endpointsDeclaration?.setType("Endpoints");
}

// 変更を実行
console.log("Modifying objects and arrays...");
modifyObjects();

console.log("Adding type definitions...");
addTypes();

// フォーマットを適用
sourceFile.formatText();

// 変更を保存
project.saveSync();

console.log(`File has been created/modified at: ${sourceFile.getFilePath()}`); 