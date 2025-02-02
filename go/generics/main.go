package main

import "fmt"

// any は interface{} の別名（Go 1.18以降）で、「任意の型」を表します
// - メリット：interface{}より短く読みやすい
// - デメリット：制約が緩すぎて型安全性が低くなる可能性がある
func Map[T, U any](s []T, f func(T) U) []U {
	// 入力スライスと同じ長さの結果用スライスを事前に確保
	result := make([]U, len(s))
	for i, v := range s {
		// 変換関数fを各要素に適用
		result[i] = f(v)
	}
	return result
}

// Filter は条件に合う要素のみを含む新しいスライスを返す
// T: スライスの要素の型
// f: フィルタリング条件を定義する関数（trueを返す要素のみが結果に含まれる）
func Filter[T any](s []T, f func(T) bool) []T {
	// 動的に拡張可能な空のスライスを作成
	var result []T
	for _, v := range s {
		// フィルタ条件を満たす要素のみを追加
		if f(v) {
			result = append(result, v)
		}
	}
	return result
}

// より具体的な制約を使用する例
// comparable は == や != による比較が可能な型を表す制約
func FindFirst[T comparable](s []T, target T) int {
	for i, v := range s {
		if v == target {
			return i
		}
	}
	return -1
}

// 数値型のみを受け付ける例
type Number interface {
	// ~ は型のエイリアスを許可する演算子
	// 例：~int は int と int32 の両方を許可する
	// つまり、intやfloat64を基底型とするすべてのカスタム型を受け付ける
	~int | ~float64
}

func Sum[T Number](numbers []T) T {
	var sum T
	for _, n := range numbers {
		sum += n
	}
	return sum
}

func main() {
	// ===== 数値スライスでの使用例 =====
	numbers := []int{1, 2, 3, 4, 5}
	
	// Map関数の使用例：
	// - 型パラメータT=int, U=intとして自動的に推論される
	// - 各要素を2倍にする変換関数を渡す
	doubled := Map(numbers, func(x int) int {
		return x * 2
	})
	fmt.Println("Doubled numbers:", doubled) // [2 4 6 8 10]

	// Filter関数の使用例：
	// - 型パラメータT=intとして自動的に推論される
	// - 偶数のみを抽出する条件関数を渡す
	evens := Filter(numbers, func(x int) bool {
		return x%2 == 0
	})
	fmt.Println("Even numbers:", evens) // [2 4]

	// ===== 文字列スライスでの使用例 =====
	// 同じジェネリック関数が文字列スライスでも使用可能
	words := []string{"hello", "world", "golang"}
	
	// Map関数の使用例：
	// - 型パラメータT=string, U=intとして自動的に推論される
	// - 文字列から整数（長さ）への変換を行う
	lengths := Map(words, func(s string) int {
		return len(s)
	})
	fmt.Println("Word lengths:", lengths) // [5 5 6]
} 