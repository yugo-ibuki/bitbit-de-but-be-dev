import * as fs from "fs";
import * as path from "path";

// generatedディレクトリを作成
const generatedDir = path.join(__dirname, "generated");
if (!fs.existsSync(generatedDir)) {
  fs.mkdirSync(generatedDir);
  console.log("Created 'generated' directory");
} 