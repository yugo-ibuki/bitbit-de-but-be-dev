// Cross-Origin iframe内のJavaScriptファイル

/**
 * このオリジンのCookieを読み込んで表示する
 */
function loadCookies() {
    const cookieListDiv = document.getElementById('cookieList');
    const statusDiv = document.getElementById('status');

    try {
        const cookies = document.cookie;

        if (!cookies || cookies.trim() === '') {
            cookieListDiv.innerHTML = '<div class="no-cookie">このオリジン（localhost:9473）にはCookieが設定されていません</div>';
            statusDiv.innerHTML = '<div class="status error">Cookie: 未設定</div>';
        } else {
            const cookieArray = cookies.split(';').map(c => c.trim());
            cookieListDiv.innerHTML = cookieArray
                .map(cookie => `<div class="cookie-item">✅ ${escapeHtml(cookie)}</div>`)
                .join('');
            statusDiv.innerHTML = `<div class="status success">Cookie取得: 成功 (${cookieArray.length}個のCookieを取得)</div>`;
        }

        // 検証情報を表示
        displayVerificationInfo();

    } catch (error) {
        cookieListDiv.innerHTML = `<div class="no-cookie">❌ エラー: ${escapeHtml(error.message)}</div>`;
        statusDiv.innerHTML = '<div class="status error">Cookie取得: エラー発生</div>';
        console.error('Cookie取得エラー:', error);
    }
}

/**
 * このオリジン用のCookieを設定する
 */
function setCrossOriginCookie() {
    try {
        const timestamp = new Date().toISOString();
        document.cookie = `cross_origin_cookie=value_from_9473; path=/; SameSite=None`;
        document.cookie = `cross_timestamp=${encodeURIComponent(timestamp)}; path=/; SameSite=None`;

        alert('このオリジン（localhost:9473）用のCookieを設定しました');

        // 再読み込み
        setTimeout(loadCookies, 300);
    } catch (error) {
        alert('Cookie設定エラー: ' + error.message);
    }
}

/**
 * 親ページのCookieへのアクセスを試行する（失敗するはず）
 */
function tryAccessParentCookie() {
    const results = [];

    // 1. document.cookie による直接アクセス
    try {
        const cookies = document.cookie;
        results.push(`✅ document.cookie 読み取り: 成功`);
        results.push(`   → ただし、これは自分のオリジンのCookieのみ`);
        results.push(`   → 親ページ（localhost:9472）のCookieではありません`);
    } catch (error) {
        results.push(`❌ document.cookie 読み取り: 失敗 (${error.message})`);
    }

    // 2. window.parent へのアクセス試行
    try {
        // Cross-origin の場合、parent.document にアクセスできない
        const parentCookie = window.parent.document.cookie;
        results.push(`⚠️ 親ページのCookie読み取り: 成功（予期しない結果）`);
        results.push(`   → Cookie: ${parentCookie}`);
    } catch (error) {
        results.push(`✅ 親ページのCookie読み取り: ブロックされました（期待通り）`);
        results.push(`   → エラー: ${error.name}`);
        results.push(`   → これがCross-Originセキュリティの正常な動作です`);
    }

    // 3. postMessage による通信は可能
    try {
        window.parent.postMessage({
            type: 'crossOriginRequest',
            message: 'Cross-origin iframe から親ページへのメッセージ'
        }, 'http://localhost:9472');
        results.push(`✅ postMessage: 送信成功`);
        results.push(`   → Cross-originでも postMessage は使用可能です`);
    } catch (error) {
        results.push(`❌ postMessage: 失敗 (${error.message})`);
    }

    alert('親ページCookieアクセステスト結果:\n\n' + results.join('\n'));
}

/**
 * 検証情報を表示する
 */
function displayVerificationInfo() {
    const infoDiv = document.getElementById('verificationInfo');
    const commDiv = document.getElementById('communicationTest');

    const info = {
        'ロケーション': window.location.href,
        'オリジン': window.location.origin,
        'プロトコル': window.location.protocol,
        'ホスト': window.location.host,
        'ポート': window.location.port,
        'iframe内か': window.self !== window.top ? 'Yes' : 'No',
        'document.cookie 長さ': document.cookie.length + ' 文字',
        'navigator.cookieEnabled': navigator.cookieEnabled ? 'true' : 'false'
    };

    // 親ページとの関係を確認
    try {
        const parentOrigin = window.parent.location.origin;
        info['親ページのオリジン'] = parentOrigin;
        info['Same-Origin?'] = parentOrigin === window.location.origin ? 'Yes' : 'No';
    } catch (e) {
        info['親ページのオリジン'] = 'アクセス不可（Cross-Origin）';
        info['Same-Origin?'] = 'No（Cross-Origin環境）';
    }

    // sandbox属性の確認
    try {
        const sandboxAttr = window.frameElement ? window.frameElement.getAttribute('sandbox') : null;
        info['sandbox属性'] = sandboxAttr ? sandboxAttr : 'なし';
    } catch (e) {
        info['sandbox属性'] = 'アクセス不可（Cross-Origin）';
    }

    infoDiv.innerHTML = Object.entries(info)
        .map(([key, value]) => `<div class="cookie-item"><strong>${escapeHtml(key)}:</strong> ${escapeHtml(String(value))}</div>`)
        .join('');

    // 通信テスト
    commDiv.innerHTML = `
        <div class="cookie-item">
            <strong>postMessage による通信:</strong><br>
            Cross-origin環境でも postMessage API を使用すれば、<br>
            親ページとiframe間でメッセージのやり取りが可能です。<br>
            ただし、Cookieの直接アクセスはセキュリティ上ブロックされます。
        </div>
    `;
}

/**
 * HTMLエスケープ処理
 */
function escapeHtml(text) {
    const map = {
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#039;'
    };
    return text.replace(/[&<>"']/g, m => map[m]);
}

// ページ読み込み時にCookieを自動的に読み込む
window.addEventListener('load', () => {
    console.log('Cross-origin iframeが読み込まれました');
    console.log('このオリジン:', window.location.origin);
    loadCookies();
});

// 親ウィンドウからのメッセージを受信
window.addEventListener('message', (event) => {
    // セキュリティ: 送信元を確認
    if (event.origin !== 'http://localhost:9472') {
        console.warn('不明なオリジンからのメッセージ:', event.origin);
        return;
    }

    console.log('親ページからのメッセージ:', event.data);

    if (event.data.type === 'reloadCookies') {
        loadCookies();
    } else if (event.data.type === 'parentCookieInfo') {
        // 親ページからCookie情報を受信
        const commDiv = document.getElementById('communicationTest');
        commDiv.innerHTML += `
            <div class="cookie-item" style="background-color: #fff3cd;">
                <strong>親ページから受信したCookie情報:</strong><br>
                ${escapeHtml(event.data.cookies || 'なし')}
            </div>
        `;
    }
});
