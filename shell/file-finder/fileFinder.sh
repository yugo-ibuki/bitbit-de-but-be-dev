#!/bin/sh

# ディレクトリパスの配列
directories="
src/app/lib/sample
src/app/api/cron/sample
src/app/(authenticated)/admin/sample
"

# ファイルパスの配列
files="
src/app/lib/sample/sample.ts
src/app/api/cron/sample/route.ts
src/app/(authenticated)/admin/sample/page.tsx
vercel.json
"

# ディレクトリの存在確認
printf "📁 ディレクトリの確認\n"
echo "$directories" | while read -r dir; do
    [ -z "$dir" ] && continue
    if [ -d "$dir" ]; then
        echo "✅ 存在: $dir"
    else
        echo "❌ 不在: $dir"
    fi
done

# ファイルの存在確認
printf "\n📄 ファイルの確認\n"
echo "$files" | while read -r file; do
    [ -z "$file" ] && continue
    if [ -f "$file" ]; then
        echo "✅ 存在: $file"
    else
        echo "❌ 不在: $file"
    fi
done

# パッケージの確認
printf "\n📦 パッケージの確認\n"
if grep -q '"rss-parser"' package.json 2>/dev/null; then
    echo "✅ rss-parser あり"
else
    echo "❌ rss-parser なし"
fi

# 解説
# このシェルスクリプトは以下の3つの確認を行います：
#
# 1. 指定された複数のディレクトリの存在確認
#    - 存在する場合: ✅ 存在: [パス]
#    - 存在しない場合: ❌ 不在: [パス]
#    を表示します
#
# 2. 指定された複数のファイルの存在確認
#    - 存在する場合: ✅ 存在: [パス]
#    - 存在しない場合: ❌ 不在: [パス]
#    を表示します
#
# 3. package.jsonファイル内の"rss-parser"パッケージの存在確認
#    - パッケージが存在する場合: ✅ rss-parser あり
#    - パッケージが存在しない場合: ❌ rss-parser なし
#    を表示します
#
# POSIX互換の標準的なシェルスクリプトで書かれており、実装に必要なファイルと
# パッケージのセットアップ状況を簡単に確認できます。

# ワンライナーバージョン（コピペして使用可能）：
# directories="
# src/app/lib/sample
# src/app/api/cron/sample
# src/app/(authenticated)/admin/sample
# " && files="
# src/app/lib/sample/sample.ts
# src/app/api/cron/sample/route.ts
# src/app/(authenticated)/admin/sample/page.tsx
# vercel.json
# " && printf "📁 ディレクトリの確認\n" && echo "$directories" | while read -r d; do [ -z "$d" ] && continue; [ -d "$d" ] && echo "✅ 存在: $d" || echo "❌ 不在: $d"; done && printf "\n📄 ファイルの確認\n" && echo "$files" | while read -r f; do [ -z "$f" ] && continue; [ -f "$f" ] && echo "✅ 存在: $f" || echo "❌ 不在: $f"; done && printf "\n📦 パッケージの確認\n" && (grep -q '"rss-parser"' package.json 2>/dev/null && echo "✅ rss-parser あり" || echo "❌ rss-parser なし")
