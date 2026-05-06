# Nix Gotchas

このメモは、Nix を実際に使うときに気をつけることをまとめたものです。

`flake.nix` の構文や output の読み方は `FLAKE_NOTES.md` を参照してください。

## Nix shell はコンテナではない

`nix develop` や `nix-shell` は、Docker のような完全隔離環境ではありません。

基本的には、今の shell に Nix のパッケージや環境変数を足します。

そのため、外側の `PATH`、`HOME`、`SHELL`、`mise`、`Homebrew`、`fish` 設定などの影響を受けます。

```bash
echo $IN_NIX_SHELL
which -a node
which -a go
```

`IN_NIX_SHELL` がセットされていれば Nix shell の中にいます。ただし、実際にどのコマンドが使われるかは `which -a` で確認します。

## fish や mise との PATH 衝突

`nix develop -c fish` は、Nix shell の中で fish を起動します。

このとき `~/.config/fish/config.fish` が再実行されます。`mise activate fish | source` や `fish_add_path` があると、Nix が追加した `/nix/store/.../bin` より前に mise や Homebrew のパスが来ることがあります。

この場合、Nix shell に入っていても、`node` などは外側のものが選ばれることがあります。

```bash
nix develop -c fish
which -a node
node --version
```

Nix 側のコマンドを確実に確認したいときは、fish を起動せずに `sh` で確認します。

```bash
nix develop -c sh -c 'which node && node --version'
```

## `impure` の意味

`nix develop` で入ると、よく次のようになります。

```bash
echo $IN_NIX_SHELL
# impure
```

`impure` は、外側の環境変数を引き継いだ Nix shell という意味です。

便利ですが、外側のツール管理と衝突しやすくなります。

より外側の環境を減らしたい場合は、次のような選択肢があります。

```bash
nix develop --ignore-environment
nix-shell --pure
```

ただし、これも Docker のような完全隔離ではありません。普段の shell 設定や PATH が使えなくなることもあります。

## `nix develop` と `nix develop -c command` の違い

`nix develop` は開発 shell に入ります。

```bash
nix develop
```

`nix develop -c command` は、Nix 環境で指定したコマンドだけ実行します。

```bash
nix develop -c go version
nix develop -c sh -c 'go version && node --version'
```

確認や CI では `-c` を使うと、普段の対話 shell 設定に引っ張られにくくなります。

## dirty tree warning

Git 管理下で未コミット変更があると、flake コマンドで次の warning が出ることがあります。

```text
warning: Git tree '...' is dirty
```

これは、flake が Git tree を入力として扱うためです。

多くの場合は warning だけで実行できます。ただし、再現性を重視する場合は、実行前に必要な変更を commit しておくと状態が分かりやすくなります。

## flake.lock は commit する

`flake.lock` は commit するのが基本です。

`flake.lock` があることで、別のマシンや CI でも同じ nixpkgs revision を使いやすくなります。

```bash
git add flake.nix flake.lock
git commit -m "chore: update nix flake"
```

`flake.lock` を更新したいときは明示的に実行します。

```bash
nix flake update
```

## nixpkgs を更新するとパッケージが変わる

`pkgs.nodejs_22` のように同じ名前を書いていても、`flake.lock` を更新すると実際の patch version が変わることがあります。

```text
更新前:
  nodejs_22 -> 22.22.2

更新後:
  nodejs_22 -> 22.x.y
```

また、古いパッケージが EOL として使えなくなることもあります。

そのため、`nix flake update` は依存更新として扱い、更新後に `nix develop`、`nix build`、`nix run` などを確認します。

## package 名は安定 API ではない

`pkgs.go_1_26` や `pkgs.nodejs_22` のような属性名は便利ですが、すべての nixpkgs revision に存在するとは限りません。

存在確認には `nix search` や `nix eval` を使います。

```bash
nix search nixpkgs go_1
nix eval --raw nixpkgs#go_1_26.version
```

プロジェクトで長く使う場合は、`flake.lock` で固定した上で、更新タイミングを明示的に管理します。

## Nix store はプロジェクト内に作られない

`nix develop` や `nix build` で取得された依存は、基本的に `/nix/store` に置かれます。

プロジェクト内に `node_modules` のような依存ディレクトリが作られるわけではありません。

`nix build` を実行した場合は、プロジェクト内に `result` という symlink ができます。

```bash
nix build
ls -l result
```

不要になった store path は、あとで garbage collect できます。

```bash
nix-collect-garbage
```

## GUI アプリや OS 固有アプリは別に考える

Nix は platform ごとの対応範囲があります。

Linux/WSL では macOS 専用アプリは基本的に使えません。macOS でも Linux 専用パッケージは使えません。

CLI や開発依存は Nix、GUI アプリや OS 固有ツールは Homebrew、winget、Scoop などに分けるのが現実的です。

## mise と Nix の役割を分ける

Node.js だけを切り替えるなら、mise のほうが軽くて自然なことが多いです。

Nix は、言語ランタイムだけでなく、OS 依存のライブラリ、CLI、DB、ビルド手順、CI の再現性までまとめたいときに強くなります。

```text
mise:
  Node / Go / Python などの普段のバージョン管理

Nix:
  devShell / build / run / CI / OS 依存込みの再現性
```

同じプロジェクトで両方を使う場合は、どちらが `PATH` の主導権を持つかを意識します。
