
## ローカルでの実行

ローカル環境で動作確認を行う場合は、DockerなどでRedisを起動する必要があります。

```fish
# Redisを起動
docker run -d -p 6379:6379 --name redis-local redis:7.0

# アプリケーション起動
nr dev
```
