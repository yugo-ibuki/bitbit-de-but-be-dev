const express = require('express');
const path = require('path');
const cors = require('cors');

const app = express();
const PORT = process.env.PORT || 9473;

// CORS設定（親ページからのアクセスを許可）
app.use(cors({
    origin: 'http://localhost:9472', // 親ページのオリジン
    credentials: true
}));

// 静的ファイルの提供
app.use(express.static(path.join(__dirname, 'public-cross-origin')));

// ルートパスへのアクセス
app.get('/', (req, res) => {
    res.sendFile(path.join(__dirname, 'public-cross-origin', 'iframe-cross-origin.html'));
});

// Cookie設定用のエンドポイント（このドメイン用のCookie）
app.get('/set-cookie', (req, res) => {
    res.cookie('cross_origin_cookie', 'value_from_cross_origin', {
        httpOnly: false,
        maxAge: 3600000, // 1時間
        sameSite: 'none',
        secure: false // HTTPでもテストできるように
    });
    res.json({ success: true, message: 'Cross-origin cookie set' });
});

// Cookie取得用のエンドポイント
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
║  🌐 Cross-Origin iframe サーバーが起動しました              ║
╚════════════════════════════════════════════════════════════╝

📍 アクセスURL: http://localhost:${PORT}

🔍 このサーバーの役割:
  - 異なるオリジン（ポート）からiframeコンテンツを提供
  - Cross-origin環境でのCookie取得を検証

⚠️  注意:
  - このサーバーは単独では動作しません
  - メインサーバー (http://localhost:9472) から読み込まれます

⚠️  停止するには: Ctrl + C

════════════════════════════════════════════════════════════
    `);
});
