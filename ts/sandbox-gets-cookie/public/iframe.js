// iframe内のJavaScriptファイル

/**
 * Cookieを読み込んで表示する
 */
function loadCookies() {
    const cookieListDiv = document.getElementById('cookieList');
    const statusDiv = document.getElementById('status');

    try {
        const cookies = document.cookie;

        if (!cookies || cookies.trim() === '') {
            cookieListDiv.innerHTML = '<div class="no-cookie">❌ Cookieにアクセスできません、または設定されていません</div>';
            statusDiv.innerHTML = '<div class="status error">Cookie取得: 失敗</div>';
        } else {
            const cookieArray = cookies.split(';').map(c => c.trim());
            cookieListDiv.innerHTML = cookieArray
                .map(cookie => `<div class="cookie-item">✅ ${escapeHtml(cookie)}</div>`)
                .join('');
            statusDiv.innerHTML = `<div class="status success">Cookie取得: 成功 (${cookieArray.length}個のCookieを取得)</div>`;
        }

        // 検証情報を表示
        displayVerificationInfo();

        // 親ウィンドウにステータスを送信
        window.parent.postMessage({
            type: 'cookieStatus',
            hasAccess: !!cookies,
            count: cookies ? cookies.split(';').length : 0
        }, '*');

    } catch (error) {
        cookieListDiv.innerHTML = `<div class="no-cookie">❌ エラー: ${escapeHtml(error.message)}</div>`;
        statusDiv.innerHTML = '<div class="status error">Cookie取得: エラー発生</div>';
        console.error('Cookie取得エラー:', error);
    }
}

/**
 * Cookie操作を試行する
 */
function tryCookieOperations() {
    const results = [];

    // 読み取りテスト
    try {
        const cookies = document.cookie;
        results.push(`✅ Cookie読み取り: 成功 (${cookies ? cookies.split(';').length : 0}個)`);
    } catch (error) {
        results.push(`❌ Cookie読み取り: 失敗 (${error.message})`);
    }

    // 書き込みテスト
    try {
        const testCookieName = 'iframe_test_cookie';
        const testCookieValue = 'test_value_' + Date.now();
        document.cookie = `${testCookieName}=${testCookieValue}; path=/`;

        // 書き込めたか確認
        const afterWrite = document.cookie;
        if (afterWrite.includes(testCookieName)) {
            results.push('✅ Cookie書き込み: 成功');
        } else {
            results.push('⚠️ Cookie書き込み: 設定したが確認できない');
        }
    } catch (error) {
        results.push(`❌ Cookie書き込み: 失敗 (${error.message})`);
    }

    // 削除テスト
    try {
        document.cookie = 'iframe_test_cookie=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/';
        results.push('✅ Cookie削除: 実行成功');
    } catch (error) {
        results.push(`❌ Cookie削除: 失敗 (${error.message})`);
    }

    alert('Cookie操作テスト結果:\n\n' + results.join('\n'));

    // 再読み込み
    loadCookies();
}

/**
 * 検証情報を表示する
 */
function displayVerificationInfo() {
    const infoDiv = document.getElementById('verificationInfo');

    const info = {
        'ロケーション': window.location.href,
        'オリジン': window.location.origin,
        'プロトコル': window.location.protocol,
        'ホスト': window.location.host,
        'iframe内か': window.self !== window.top ? 'Yes' : 'No',
        'document.cookie 長さ': document.cookie.length + ' 文字',
        'navigator.cookieEnabled': navigator.cookieEnabled ? 'true' : 'false'
    };

    // sandbox属性の確認
    try {
        const sandboxAttr = window.frameElement ? window.frameElement.getAttribute('sandbox') : null;
        info['sandbox属性'] = sandboxAttr ? sandboxAttr : 'なし';
    } catch (e) {
        info['sandbox属性'] = 'アクセス不可 (cross-origin?)';
    }

    infoDiv.innerHTML = Object.entries(info)
        .map(([key, value]) => `<div class="cookie-item"><strong>${escapeHtml(key)}:</strong> ${escapeHtml(String(value))}</div>`)
        .join('');
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
    console.log('iframeが読み込まれました');
    loadCookies();
});

// 親ウィンドウからのメッセージを受信
window.addEventListener('message', (event) => {
    if (event.data.type === 'reloadCookies') {
        loadCookies();
    }
});
