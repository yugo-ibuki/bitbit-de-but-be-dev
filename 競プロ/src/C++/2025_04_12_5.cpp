#include <iostream>  // 入出力のためのヘッダーファイル
#include <string>    // 文字列操作のためのヘッダーファイル
#include <vector>
using namespace std;

/**
問題文
縦 H ピクセル、横 W ピクセルの画像があります。 各ピクセルは英小文字で表されます。
上から i 番目、左から j 番目のピクセルは aij です。
この画像の周囲 1 ピクセルを # で囲んだものを出力してください。

https://atcoder.jp/contests/abc062/tasks/abc062_b
 */
int main() {
  int H, W;
  cin >> H >> W;

  vector<string> a(H);
  for (int i = 0; i < H; i++) {
    cin >> a[i];
  }

  // H+2で、まずはVectorのサイズを確保しておく
  // その後、W+2で、各要素をstring(W+2, '#')で初期化しておく
  // つまり、H+2個の要素を持つVectorを作成し、それぞれの要素をstring(W+2, '#')によってH+2の数だけW+2を作成し初期化する
  vector<string> S(H+2, string(W+2, '#'));

  // 真ん中を埋めていく
  for (int i = 1; i < H+1; i++) {
    for (int j = 1; j < W+1; j++) {
      S[i][j] = a[i-1][j-1];
    }
  }

 // 出力する
  for (int i = 0; i < H+2; i++) cout << S[i] << endl;
}
