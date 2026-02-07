# Haskell モナド学習

## 📚 参照ドキュメント

| ソース | リンク |
|--------|--------|
| Haskell Base Library | https://hackage.haskell.org/package/base-4.21.0.0/docs |
| Control.Monad | https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Monad.html |
| Data.Maybe | https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html |
| Data.Either | https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Either.html |
| Context7 Library ID | /websites/hackage_haskell_package_base-4_21_0_0 |

## 学習順序

1. **01-functor.hs** - Functor（写像）の理解
2. **02-applicative.hs** - Applicative Functor
3. **03-monad-basics.hs** - Monadの基礎
4. **04-maybe-monad.hs** - Maybe モナド
5. **05-either-monad.hs** - Either モナド
6. **06-io-monad.hs** - IO モナド
7. **07-do-notation.hs** - do記法

## 実行方法

```bash
# GHCi で対話的に実行
ghci 01-functor.hs

# または直接実行
runhaskell 01-functor.hs
```

## 関数型プログラミングの核心的な考え方

### 純粋性（Purity）
- 同じ入力に対して常に同じ出力を返す
- 副作用を持たない（外部状態を変更しない）

### 不変性（Immutability）
- データは一度作成されると変更されない
- 「更新」は新しいデータの作成を意味する

### 参照透過性（Referential Transparency）
- 式をその評価結果で置き換えても意味が変わらない
- デバッグやテストが容易

### 型による設計（Type-Driven Design）
- 型システムが不正な状態を表現できないよう設計
- 「コンパイルが通れば正しい」を目指す
