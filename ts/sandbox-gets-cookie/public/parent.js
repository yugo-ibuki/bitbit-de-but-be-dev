// 親ページのJavaScriptファイル

/**
 * Cookieを設定する
 */
function setCookies() {
    // 様々なタイプのCookieを設定
    const cookies = [
        { name: 'test_cookie_1', value: 'value1', path: '/' },
        { name: 'test_cookie_2', value: 'value2_with_special_chars_!@#', path: '/' },
        { name: 'session_id', value: 'abc123xyz789', path: '/' },
        { name: 'user_pref', value: 'dark_mode', path: '/' },
        { name: 'timestamp', value: new Date().toISOString(), path: '/' }
    ];

    cookies.forEach(cookie => {
        // 基本的なCookieを設定（HTTPOnly属性なし）
        document.cookie = `${cookie.name}=${encodeURIComponent(cookie.value)}; path=${cookie.path}; SameSite=Lax`;
    });

    // Secure属性付きのCookieも設定を試みる（HTTPSが必要）
    try {
        document.cookie = `secure_cookie=secure_value; path=/; Secure; SameSite=None`;
    } catch (e) {
        console.warn('Secure cookieの設定に失敗しました（HTTPSが必要）:', e);
    }

    alert(`${cookies.length}個のCookieを設定しました。\niframe内でCookieが取得できるか確認してください。`);

    // iframeを再読み込みして結果を確認
    setTimeout(() => {
        const frames = document.querySelectorAll('iframe');
        frames.forEach(frame => {
            frame.contentWindow.postMessage({ type: 'reloadCookies' }, '*');
        });
    }, 500);
}

/**
 * 親ページのCookieを表示する
 */
function showParentCookies() {
    const cookiesDiv = document.getElementById('parentCookies');
    const cookieListDiv = document.getElementById('parentCookieList');

    const cookies = document.cookie;

    if (!cookies) {
        cookieListDiv.innerHTML = '<div class="cookie-item" style="color: #d32f2f;">Cookie が設定されていません</div>';
    } else {
        const cookieArray = cookies.split(';').map(c => c.trim());
        cookieListDiv.innerHTML = cookieArray
            .map(cookie => `<div class="cookie-item">${escapeHtml(cookie)}</div>`)
            .join('');
    }

    cookiesDiv.style.display = 'block';
}

/**
 * すべてのCookieをクリアする
 */
function clearAllCookies() {
    const cookies = document.cookie.split(';');

    cookies.forEach(cookie => {
        const eqPos = cookie.indexOf('=');
        const name = eqPos > -1 ? cookie.substr(0, eqPos).trim() : cookie.trim();
        // 過去の日付を設定して削除
        document.cookie = `${name}=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/`;
    });

    alert('すべてのCookieをクリアしました');

    // 表示を更新
    const cookiesDiv = document.getElementById('parentCookies');
    cookiesDiv.style.display = 'none';

    // iframeを再読み込み
    setTimeout(() => {
        const frames = document.querySelectorAll('iframe');
        frames.forEach(frame => {
            frame.contentWindow.postMessage({ type: 'reloadCookies' }, '*');
        });
    }, 500);
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

// ページ読み込み時の処理
window.addEventListener('load', () => {
    console.log('親ページが読み込まれました');
    console.log('現在のCookie:', document.cookie);
});

// iframe からのメッセージを受信
window.addEventListener('message', (event) => {
    console.log('受信したメッセージ:', event.data, 'オリジン:', event.origin);

    if (event.data.type === 'cookieStatus') {
        console.log('iframe からのCookie状態:', event.data);
    } else if (event.data.type === 'crossOriginRequest') {
        console.log('Cross-origin iframe からのリクエスト:', event.data.message);

        // Cross-origin iframe に親ページのCookie情報を送信
        // （セキュリティ上、これは意図的な情報共有）
        const crossOriginFrame = document.getElementById('crossOriginFrame');
        if (crossOriginFrame && crossOriginFrame.contentWindow) {
            crossOriginFrame.contentWindow.postMessage({
                type: 'parentCookieInfo',
                cookies: document.cookie,
                message: '親ページからの応答'
            }, 'http://localhost:9473');
        }
    }
});
