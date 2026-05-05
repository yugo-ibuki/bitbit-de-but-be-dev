# Nix Sample

Nix の主な使い方を小さく試すためのサンプルです。

- パッケージ管理: `nix profile install` / `nix run`
- 開発環境: `nix develop`
- ビルド可能な成果物: `nix build`

## 1. 一回だけパッケージを使う

インストールせずに `hello` パッケージを実行します。

```bash
nix run nixpkgs#hello
```

`ripgrep` も同じように一回だけ使えます。

```bash
nix run nixpkgs#ripgrep -- --version
```

## 2. ユーザー環境にパッケージを入れる

`brew install` や `mise install` に近い使い方です。

```bash
nix profile install nixpkgs#ripgrep
rg --version
```

消す場合:

```bash
nix profile remove ripgrep
```

## 3. プロジェクトの開発環境に入る

このディレクトリで実行します。

```bash
cd nix
nix develop
```

この shell の中では `flake.nix` の `devShells.default.packages` に書いたコマンドが使えます。

```bash
node --version
rg --version
```

外側に `mise` や Homebrew の Node.js があっても、この shell の中では Nix の Node.js が優先されます。

## 4. Node.js のバージョンを変える

`flake.nix` の `pkgs.nodejs_22` を変えます。

```nix
packages = [
  pkgs.nodejs_22
  pkgs.ripgrep
];
```

Node.js 20 にしたい場合:

```nix
packages = [
  pkgs.nodejs_20
  pkgs.ripgrep
];
```

そのあと入り直します。

```bash
exit
nix develop
node --version
```

正確な patch version は `flake.lock` が固定している nixpkgs の revision で決まります。

## 5. この flake のアプリを実行する

```bash
nix run
```

引数も渡せます。

```bash
nix run . -- Yugo
```

## 6. ビルドして成果物を作る

```bash
nix build
./result/bin/nix-node-sample
```

`result` は Nix store 内のビルド結果へのシンボリックリンクです。

## 7. flake.lock を更新する

`nixpkgs` の固定 revision を更新したい場合:

```bash
nix flake update
```

同じ `pkgs.nodejs_22` でも、`flake.lock` 更新後はより新しい Node.js 22.x になることがあります。
