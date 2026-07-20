import { pbkdf2Sync, randomBytes } from "node:crypto";

function readHidden(prompt) {
  if (!process.stdin.isTTY || !process.stdin.setRawMode) {
    throw new Error("Usage: npm run hash-admin-password -- <password>");
  }
  return new Promise((resolve, reject) => {
    let value = "";
    process.stdout.write(prompt);
    process.stdin.setRawMode(true);
    process.stdin.resume();
    process.stdin.setEncoding("utf8");
    const finish = () => {
      process.stdin.setRawMode(false);
      process.stdin.pause();
      process.stdin.off("data", onData);
      process.stdout.write("\n");
    };
    const onData = (character) => {
      if (character === "\u0003") {
        finish();
        reject(new Error("Cancelled"));
      } else if (character === "\r" || character === "\n") {
        finish();
        resolve(value);
      } else if (character === "\u007f") {
        value = value.slice(0, -1);
      } else {
        value += character;
      }
    };
    process.stdin.on("data", onData);
  });
}

const password = process.argv[2] ?? (await readHidden("Admin password: "));
if (password.length < 8) {
  throw new Error("Password must contain at least 8 characters");
}
const iterations = 100_000;
const salt = randomBytes(16);
const hash = pbkdf2Sync(password, salt, iterations, 32, "sha256");
console.log(
  ["pbkdf2-sha256", iterations, salt.toString("base64"), hash.toString("base64")].join("$"),
);
