const express = require('express');
const path = require('path');
const cors = require('cors');

const app = express();
const PORT = process.env.PORT || 9472;

// CORS設定（クロスオリジンテストのため）
app.use(cors({
    origin: true,
    credentials: true
}));

// 静的ファイルの提供
app.use(express.static(path.join(__dirname, 'public')));

// ルートパスへのアクセス
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public', 'index.html'));
});

// Cookie設定用のエンドポイント（サーバーサイドでCookieを設定）
app.get('/set-cookie', (req, res) => {
    res.cookie('server_set_cookie', 'value_from_server', {
        httpOnly: false,
        maxAge: 3600000, // 1時間
        sameSite: 'lax'
    });
    res.json({ success: true, message: 'Cookie set from server' });
});

// Cookie取得用のエンドポイント（サーバーサイドで確認）
app.get('/get-cookies', (req, res) => {
    res.json({
        cookies: req.headers.cookie || 'No cookies received',
        cookieHeader: req.headers.cookie
    });
});

// サーバー起動
app.listen(PORT, () => {
    console.log(`
╔════════════════════════════════════════════════════════════╗
║  🍪 iframe Cookie検証サーバーが起動しました                 ║
╚════════════════════════════════════════════════════════════╝

📍 アクセスURL: http://localhost:${PORT}

🔍 検証内容:
  - Same-origin iframe でのCookie取得
  - Sandbox属性付きiframeでのCookie取得
  - 様々なCookie属性のテスト

📝 使い方:
  1. ブラウザで http://localhost:${PORT} を開く
  2. 「Cookieを設定」ボタンをクリック
  3. 各iframeでCookieが取得できるか確認

⚠️  停止するには: Ctrl + C

════════════════════════════════════════════════════════════
    `);
});
