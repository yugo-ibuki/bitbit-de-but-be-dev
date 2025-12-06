#!/bin/bash
# Redis監視スクリプト

echo "=========================================="
echo "Redis監視ツール"
echo "=========================================="
echo ""

# 1. 基本情報
echo "📊 [1] Redis基本情報:"
docker exec redis-demo-redis-1 redis-cli INFO server | grep -E "redis_version|os|uptime_in_seconds"
echo ""

# 2. 接続情報
echo "🔗 [2] クライアント接続数:"
docker exec redis-demo-redis-1 redis-cli INFO clients | grep connected_clients
echo ""

# 3. メモリ使用状況
echo "💾 [3] メモリ使用状況:"
docker exec redis-demo-redis-1 redis-cli INFO memory | grep -E "used_memory_human|used_memory_peak_human|maxmemory_human"
echo ""

# 4. キャッシュキー数
echo "🔑 [4] 保存されているキー:"
docker exec redis-demo-redis-1 redis-cli DBSIZE
docker exec redis-demo-redis-1 redis-cli KEYS "*" | head -10
echo ""

# 5. 統計情報
echo "📈 [5] コマンド実行統計:"
docker exec redis-demo-redis-1 redis-cli INFO stats | grep -E "total_commands_processed|instantaneous_ops_per_sec|keyspace_hits|keyspace_misses"
echo ""

# 6. スローログ
echo "🐌 [6] スローログ（1ms以上のコマンド）:"
docker exec redis-demo-redis-1 redis-cli SLOWLOG GET 5
echo ""

# 7. キャッシュヒット率
echo "🎯 [7] キャッシュヒット率:"
HITS=$(docker exec redis-demo-redis-1 redis-cli INFO stats | grep keyspace_hits | cut -d: -f2 | tr -d '\r')
MISSES=$(docker exec redis-demo-redis-1 redis-cli INFO stats | grep keyspace_misses | cut -d: -f2 | tr -d '\r')
if [ "$HITS" != "" ] && [ "$MISSES" != "" ]; then
    TOTAL=$((HITS + MISSES))
    if [ $TOTAL -gt 0 ]; then
        HIT_RATE=$(echo "scale=2; $HITS * 100 / $TOTAL" | bc)
        echo "  ヒット: $HITS"
        echo "  ミス: $MISSES"
        echo "  ヒット率: ${HIT_RATE}%"
    fi
fi
echo ""

echo "=========================================="
echo "リアルタイム監視を開始する場合:"
echo "  docker exec -it redis-demo-redis-1 redis-cli MONITOR"
echo "=========================================="
