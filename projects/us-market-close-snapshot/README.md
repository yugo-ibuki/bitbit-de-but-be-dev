# US Market Close Snapshot

https://sekai-kabuka.com/pc-index.html に表示される日経平均・ダウ平均・ナスダック・S&P500の引け後スナップショットを、日付別のJSONとして保存する独立プロジェクトです。

保存先は `data/YYYY-MM-DD.json` です。ファイル名の日付は米国市場のセッション日であり、日経平均の取引日とは限りません。

専用の収集スクリプトは置かず、定期実行されたCodexがブラウザーから4指数を取得し、内容を検証してから `data/YYYY-MM-DD.json` へ保存します。
