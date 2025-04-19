#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>
#define rep(i, a, b) for (int i = a; i < b; i++)
using namespace std;

/**
 * 問題文：
 * 縦 H 行、横 W 列のマス目があります。上から i 行目、左から j 列目のマスを (i,j) と表します。
 * 各マスは白または黒です。マス目の配色は、H 行 W 列の行列 (a_i,j) によって与えられます。
 * a_i,j が '.' ならばマス (i,j) は白であり、a_i,j が '#' ならばマス (i,j) は黒です。
 * 
 * すぬけ君はこのマス目を圧縮しようとしています。そのために、白いマスのみからなる行または列が
 * 存在する間、次の操作を繰り返し行います。
 * 
 * 操作: 白いマスのみからなる行または列をひとつ任意に選び、その行または列を取り除いて空白を詰める。
 * 
 * 各操作でどの行または列を選ぶかによらず、最終的なマス目は一意に定まることが示せます。
 * 最終的なマス目を求めてください。
 */
int H, W;
string A[101];
int check() {
  rep(y, 0, H) {
    int ok = 1;
    rep(x, 0, W) if (A[y][x] == '#') ok = 0;
    if (ok) {
      rep(yy, y + 1, H) swap(A[yy -1], A[yy]);
      H--;
      return 1;
    }
  }

  rep(x, 0, W) {
      int ok = 1;
      rep(y, 0, H) if (A[y][x] == '#') ok = 0;
      if (ok) {
          // 列を削除
          // str.substr(start, length): start位置からlength文字分の部分文字列を返す
          // str.substr(start): start位置から文字列の最後までの部分文字列を返す
          rep(yy, 0, H) A[yy] = A[yy].substr(0, x) + A[yy].substr(x + 1);
          // TSで書くと
          // // 文字列を配列に変換し、x番目の要素を削除して再結合
          // A[yy] = [...A[yy]].filter((_, i) => i !== x).join('');
          W--;
          return 1;
      }
  }
  return 0;
}

int main() {
  cin >> H >> W;
  rep(i, 0, H) cin >> A[i];
  while (check());
  rep(i, 0, H) cout << A[i] << endl;
}
