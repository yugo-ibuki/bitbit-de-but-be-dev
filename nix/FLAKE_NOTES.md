# flake.nix Notes

このメモは、このディレクトリの `flake.nix` を読むための補助です。

## flake.nix の役割

`flake.nix` は、このプロジェクトで Nix が扱う入口をまとめるファイルです。

```text
devShells -> nix develop
packages  -> nix build
apps      -> nix run
inputs    -> 依存
flake.lock -> 依存の固定
```

`shell.nix` が主に「開発 shell に入る」ためのファイルなのに対して、`flake.nix` は開発環境、実行、ビルド、チェック、フォーマッタなどへ横展開しやすい構成です。

## 基本構造

`flake.nix` は大きく `inputs` と `outputs` で構成されます。

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    # devShells, packages, apps などを書く
  };
}
```

`inputs` は依存です。このサンプルでは `nixpkgs` だけを使っています。

`outputs` は、この flake が外に提供するものです。`nix develop` 用の `devShells`、`nix build` 用の `packages`、`nix run` 用の `apps` などを書きます。

## flake.lock

`flake.nix` の `nixpkgs.url` は `nixos-unstable` を指していますが、実際に使う具体的な revision は `flake.lock` に固定されます。

```bash
nix flake update
```

を実行すると `flake.lock` が更新されます。

```text
flake.nix:
  どの系列の依存を見るかを書く

flake.lock:
  具体的にどの revision を使うかを固定する
```

## system ごとの定義

Nix は OS と CPU ごとにパッケージ集合が違います。

このサンプルでは、次の system に対応する output を作っています。

```nix
systems = [
  "aarch64-darwin"
  "x86_64-darwin"
  "aarch64-linux"
  "x86_64-linux"
];
```

それぞれの system に対して、対応する `pkgs` を読み込んでいます。

```nix
pkgs = import nixpkgs { inherit system; };
```

`inherit system;` は次と同じ意味です。

```nix
{ system = system; }
```

## forAllSystems

このサンプルでは、複数 system 向けの output を作るために `forAllSystems` というヘルパーを定義しています。

```nix
forAllSystems =
  f:
  nixpkgs.lib.genAttrs systems (
    system:
    f {
      inherit system;
      pkgs = import nixpkgs { inherit system; };
    }
  );
```

`genAttrs` は、リストから attribute set を作る関数です。

概念的には、次のような形を作っています。

```nix
{
  aarch64-darwin = ...;
  x86_64-darwin = ...;
  aarch64-linux = ...;
  x86_64-linux = ...;
}
```

## `{ pkgs, ... }` の `...`

このサンプルでは、いくつかの場所で次のように書いています。

```nix
{ pkgs, ... }:
```

`forAllSystems` は関数に `pkgs` だけでなく `system` も渡しています。

```nix
f {
  inherit system;
  pkgs = import nixpkgs { inherit system; };
}
```

そのため、受け側を次のように書くとエラーになります。

```nix
{ pkgs }:
```

`system` という余分な引数を受け取れないためです。

余分な引数を無視したい場合は `...` を付けます。

```nix
{ pkgs, ... }:
```

`system` も使いたい場合は、明示的に受け取ります。

```nix
{ system, pkgs, ... }:
```

## devShells

`nix develop` が読むのは `devShells` です。

```nix
devShells = forAllSystems (
  { pkgs, ... }:
  {
    default = pkgs.mkShell {
      packages = [
        pkgs.go_1_26
        pkgs.nodejs_22
        pkgs.ripgrep
      ];
    };
  }
);
```

`packages` に書いたものが、開発 shell の `PATH` に入ります。

このサンプルでは、外側の Go と違うことが分かりやすいように `pkgs.go_1_26` を入れています。

```bash
nix develop -c sh -c 'which go && go version'
```

## shellHook

`shellHook` は、開発 shell に入ったときに実行されるスクリプトです。

```nix
shellHook = ''
  echo "Entered nix devShell"
  echo "go:   $(go version)"
  echo "node: $(node --version)"
  echo "rg:   $(rg --version | head -n 1)"
'';
```

このサンプルでは、どのバージョンが使われているかを確認するために使っています。

注意点として、`nix develop -c fish` のように fish を起動すると、`config.fish` が再実行されます。その結果、`mise` などが `PATH` を並べ替えて、Nix の Node より外側の Node が優先されることがあります。

その場合は、次で実際にどのコマンドが使われているか確認します。

```bash
which -a node
which -a go
```

## packages

`nix build` が読むのは `packages` です。

```nix
packages = forAllSystems (
  { pkgs, ... }:
  {
    default = pkgs.writeShellApplication {
      name = "nix-node-sample";
      runtimeInputs = [
        pkgs.nodejs_22
      ];
      text = ''
        exec node ${./hello.js} "$@"
      '';
    };
  }
);
```

`pkgs.writeShellApplication` は、小さい CLI を作るときに便利です。

`runtimeInputs` に書いたものは、このアプリを実行するときの `PATH` に入ります。

このサンプルでは、`hello.js` を Node.js 22 で実行する CLI を作っています。

```bash
nix build
./result/bin/nix-node-sample
```

## apps

`nix run` が読む入口は `apps` です。

```nix
apps = forAllSystems (
  { system, ... }:
  {
    default = {
      type = "app";
      program = "${self.packages.${system}.default}/bin/nix-node-sample";
    };
  }
);
```

ここでは、`packages.default` で作ったコマンドを `nix run` から呼べるようにしています。

```bash
nix run . -- Yugo
```

## バージョン指定の注意点

`pkgs.nodejs_22` や `pkgs.go_1_26` のような名前は、使っている `nixpkgs` revision に依存します。

常に全バージョンが存在するわけではありません。

また、属性として見えても、EOL などの理由で利用が拒否されることがあります。このサンプルでは最初に `go_1_23` を試しましたが、Nixpkgs 側で EOL として拒否されたため `go_1_26` にしました。

使える属性を探す例:

```bash
nix search nixpkgs go_1
```

特定パッケージのバージョンを見る例:

```bash
nix eval --raw nixpkgs#go_1_26.version
```

experimental features が必要な環境では、次のように付けます。

```bash
nix --extra-experimental-features 'nix-command flakes' eval --raw nixpkgs#go_1_26.version
```

## よく使うコマンド

```bash
# 開発 shell に入る
nix develop

# fish を起動する
nix develop -c fish

# sh でコマンドだけ実行する
nix develop -c sh -c 'go version'

# アプリを実行する
nix run . -- Yugo

# ビルドする
nix build

# lock を更新する
nix flake update
```

experimental features を恒久的に有効化していない場合は、次のようにします。

```bash
nix --extra-experimental-features 'nix-command flakes' develop
```

## shell.nix との違い

このサンプルには `shell.nix` もあります。

```text
nix-shell + shell.nix:
  flakes なしで使える
  主に開発 shell を作る

nix develop + flake.nix:
  flakes を使う
  flake.lock で依存を固定しやすい
  devShell / run / build などへ横展開しやすい
```

今回の `shell.nix` は、`flake.lock` を読んで同じ nixpkgs revision を使うようにしています。ただし、`shell.nix` 自体は flake ではありません。
