## ジェネリクス

https://zenn.dev/nobishii/articles/type_param_intro

# 型制約について

型制約には以下の方法があります：

1. 標準の `comparable` 制約（組み込み）
   - `==`, `!=` の比較が可能な型に制限
   - 標準ライブラリの一部

2. カスタム制約インターフェース
   ```go
   type Integer interface {
       ~int | ~int8 | ~int16 | ~int32 | ~int64 |
       ~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64
   }
   ```

3. `golang.org/x/exp/constraints` パッケージ（実験的）
   - 注意：これは標準ライブラリではなく、実験的なパッケージです
   - `constraints.Number`, `constraints.Ordered`, `constraints.Integer` などの制約を提供

## 使用例
```go
// 基本的な使用例（型推論を利用）
numbers := []int{1, 2, 3, 4, 5}
strings := Map(numbers, func(n int) string {
    return strconv.Itoa(n)
})
// 数値型のみを受け付けるバージョン
doubled := MapNumbers(numbers, func(n int) int {
    return n * 2
})
```

そして、コードファイルには以下のようなコメントを追加します：

```go
package genericmap
import "golang.org/x/exp/constraints"
// Map は任意の型のスライスを別の型のスライスに変換する汎用的な関数です。
// T: 入力スライスの要素の型
// U: 出力スライスの要素の型
// s: 入力スライス
// f: 変換関数
func Map[T, U any](s []T, f func(T) U) []U {
    result := make([]U, len(s))
    for i, v := range s {
    result[i] = f(v)
    }
    return result
}

// MapNumbers は数値型のスライスのみを受け付けるMap関数です。
// constraints.Number は実験的なパッケージ（golang.org/x/exp/constraints）の型制約です。
// 注意: これは標準ライブラリではありません。
func MapNumbers[T constraints.Number, U any](s []T, f func(T) U) []U {
    result := make([]U, len(s))
    for i, v := range s {
        result[i] = f(v)
    }
    return result
}

// Integer は整数型の制約を定義するカスタムインターフェースです。
// ~記号は、その型に基づくユーザー定義の型も含むことを示します。
type Integer interface {
    ~int | ~int8 | ~int16 | ~int32 | ~int64 |
    ~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64
}

// MapIntegers は整数型のスライスのみを受け付けるMap関数です。
// カスタム定義のInteger制約を使用しています。
func MapIntegers[T Integer, U any](s []T, f func(T) U) []U {
    result := make([]U, len(s))
    for i, v := range s {
        result[i] = f(v)
    }
    return result
}

// MapComparable は比較可能な型のスライスのみを受け付けるMap関数です。
// comparable は組み込みの制約で、== と != による比較が可能な型を示します。
func MapComparable[T comparable, U any](s []T, f func(T) U) []U {
    result := make([]U, len(s))
    for i, v := range s {
        result[i] = f(v)
    }
    return result
}
```
